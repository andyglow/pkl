/*
 * Copyright © 2026 Apple Inc. and the Pkl project authors. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.pkl.config.scala

import org.pkl.config.scala.syntax.*
import org.pkl.core.{Evaluator, ModuleSource}
import org.scalatest.Outcome
import org.scalatest.funsuite.FixtureAnyFunSuite

/**
 * Spike validation for the typeclass-driven `Decoder` design. Touches enough shapes to judge
 * whether the API ergonomics are sane:
 *
 *   - primitives (Int, String, Boolean, Long)
 *   - generic collections (Seq, List, Set, Map)
 *   - collections of one-directional elements (union / Either / Option)
 *   - Option (with present and null sources)
 *   - Either (source dispatch)
 *   - Pair → Tuple2
 *   - case class via `Mirror.ProductOf` derivation (field-by-field, no java.lang.reflect)
 *   - nested case class
 *   - Scala 3 union and singleton string-literal via the quoted-macro fallback
 *   - direct domain decoder (FiniteDuration)
 *   - sum types (sealed trait, enum) via `Mirror.SumOf`
 *
 * Whole-module shapes use `to[T]`; individual properties are pulled with `getProperty(name)` and
 * decoded via `decodeAs[T]` (the navigate-by-name `as(name)` sugar was removed — `to[T]` plus
 * `decodeAs[T]` cover the surface).
 */
class DecoderSpec extends FixtureAnyFunSuite {

  import DecoderSpec.*

  type FixtureParam = Evaluator

  override def withFixture(test: OneArgTest): Outcome = {
    val ev = Evaluator.preconfigured()
    try withFixture(test.toNoArgTest(ev))
    finally ev.close()
  }

  test("primitive: Int via top-level property") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("value = 42"))
    assert(pmodule.getProperty("value").decodeAs[Int] == Right(42))
  }

  test("primitive: String via top-level property") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("""value = "hello""""))
    assert(pmodule.getProperty("value").decodeAs[String] == Right("hello"))
  }

  test("Seq[Int] from Listing<Int>") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("value: Listing<Int> = new { 1; 2; 3 }"))
    assert(pmodule.getProperty("value").decodeAs[Seq[Int]] == Right(Seq(1, 2, 3)))
  }

  test("Set[Int] dedups Listing<Int>") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("value: Listing<Int> = new { 1; 2; 2; 3 }"))
    assert(pmodule.getProperty("value").decodeAs[Set[Int]] == Right(Set(1, 2, 3)))
  }

  test("Map[String, Int] from Mapping<String, Int>") { ev =>
    val pmodule = ev.evaluate(
      ModuleSource.text("""value: Mapping<String, Int> = new { ["a"] = 1; ["b"] = 2 }""")
    )
    assert(pmodule.getProperty("value").decodeAs[Map[String, Int]] == Right(Map("a" -> 1, "b" -> 2)))
  }

  test("Option[Int] is Some on present value") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("value: Int? = 5"))
    assert(pmodule.getProperty("value").decodeAs[Option[Int]] == Right(Some(5)))
  }

  test("Option[Int] is None on null") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("value: Int? = null"))
    assert(pmodule.getProperty("value").decodeAs[Option[Int]] == Right(None))
  }

  test("Either[String, Int] dispatches by source — String → Left") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("""value: String | Int = "hi""""))
    assert(pmodule.getProperty("value").decodeAs[Either[String, Int]] == Right(Left("hi")))
  }

  test("Either[String, Int] dispatches by source — Int → Right") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("value: String | Int = 42"))
    assert(pmodule.getProperty("value").decodeAs[Either[String, Int]] == Right(Right(42)))
  }

  // ---------- Collections of one-directional elements (decode-only / union elements) ----------
  // These resolve only because the collection givens are split per-direction and require just a
  // `Decoder` per element — a bundled `Codec[Seq[A]]` would demand `Codec[Either]`, which doesn't exist.

  test("Listing<String|Int> decodes to Seq[Either[String, Int]]") { ev =>
    val pmodule =
      ev.evaluate(ModuleSource.text("""value: Listing<String|Int> = new { "a"; 1; "b" }"""))
    assert(
      pmodule.getProperty("value").decodeAs[Seq[Either[String, Int]]] ==
        Right(Seq(Left("a"), Right(1), Left("b")))
    )
  }

  test("Listing<Int|Boolean> decodes to List[Int | Boolean]") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("value: Listing<Int|Boolean> = new { 1; true; 2 }"))
    assert(pmodule.getProperty("value").decodeAs[List[Int | Boolean]] == Right(List(1, true, 2)))
  }

  test("Mapping<String, String|Int> decodes to Map with Either values") { ev =>
    val pmodule = ev.evaluate(
      ModuleSource.text("""value: Mapping<String, String|Int> = new { ["a"] = 1; ["b"] = "x" }""")
    )
    assert(
      pmodule.getProperty("value").decodeAs[Map[String, Either[String, Int]]] ==
        Right(Map("a" -> Right(1), "b" -> Left("x")))
    )
  }

  test("Listing<Int?> decodes element-wise to Seq[Option[Int]]") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("value: Listing<Int?> = new { 1; null; 3 }"))
    assert(
      pmodule.getProperty("value").decodeAs[Seq[Option[Int]]] == Right(Seq(Some(1), None, Some(3)))
    )
  }

  test("Pair → (Int, String)") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("""value: Pair<Int, String> = Pair(7, "seven")"""))
    assert(pmodule.getProperty("value").decodeAs[(Int, String)] == Right((7, "seven")))
  }

  test("case class via Mirror.ProductOf — flat fields") { ev =>
    val code = """
      |module M
      |name = "Alice"
      |age = 30
      |""".stripMargin
    val pmodule = ev.evaluate(ModuleSource.text(code))
    assert(pmodule.to[Person] == Person("Alice", 30))
  }

  test("case class via Mirror — nested") { ev =>
    val code = """
      |module M
      |name = "Bob"
      |home {
      |  zip = "12345"
      |  city = "Springfield"
      |}
      |""".stripMargin
    val pmodule = ev.evaluate(ModuleSource.text(code))
    assert(pmodule.to[Citizen] == Citizen("Bob", Address("12345", "Springfield")))
  }

  test("FiniteDuration via domain decoder preserves unit") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("value: Duration = 5.ms"))
    import scala.concurrent.duration.*
    assert(
      pmodule.getProperty("value").decodeAs[FiniteDuration] == Right(FiniteDuration(5, MILLISECONDS))
    )
  }

  test("union Int | Boolean — Int side via macro fallback") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("value: Int | Boolean = 42"))
    assert(pmodule.getProperty("value").decodeAs[Int | Boolean] == Right(42))
  }

  test("union Int | Boolean — Boolean side via macro fallback") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("value: Int | Boolean = true"))
    assert(pmodule.getProperty("value").decodeAs[Int | Boolean] == Right(true))
  }

  test("""singleton "hello" succeeds when source matches""") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("""value: "hello" = "hello""""))
    assert(pmodule.getProperty("value").decodeAs["hello"] == Right("hello"))
  }

  test("""singleton "hello" errors when source doesn't match""") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("""value: String = "world""""))
    assert(pmodule.getProperty("value").decodeAs["hello"].isLeft)
  }

  // ---------- Sum-type derivation (Mirror.SumOf) ----------

  test("singleton enum decodes from PObject with branch class name") { ev =>
    val code = """
      |open class Color
      |class Red extends Color
      |class Green extends Color
      |class Blue extends Color
      |value: Color = new Green {}
      |""".stripMargin
    val pmodule = ev.evaluate(ModuleSource.text(code))
    assert(pmodule.getProperty("value").decodeAs[Color] == Right(Color.Green))
  }

  test("singleton enum decodes via `_type` discriminator") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("""value: Dynamic = new { _type = "Red" }"""))
    assert(pmodule.getProperty("value").decodeAs[Color] == Right(Color.Red))
  }

  test("singleton enum errors on unknown branch") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("""value: Dynamic = new { _type = "Yellow" }"""))
    val err = pmodule.getProperty("value").decodeAs[Color].swap.toOption.get
    assert(err.getMessage.contains("Unknown sum branch `Yellow`"))
    assert(err.getMessage.contains("Red, Green, Blue"))
  }

  test("sealed trait dispatches by Pkl class name") { ev =>
    val code = """
      |open class Shape
      |class Square extends Shape { sideLength: Float }
      |class Circle extends Shape { radius: Float }
      |value: Shape = new Square { sideLength = 2.0 }
      |""".stripMargin
    val pmodule = ev.evaluate(ModuleSource.text(code))
    assert(pmodule.getProperty("value").decodeAs[Shape] == Right(Square(2.0)))
  }

  test("sealed trait dispatches each branch correctly") { ev =>
    val code = """
      |open class Shape
      |class Square extends Shape { sideLength: Float }
      |class Circle extends Shape { radius: Float }
      |s: Shape = new Square { sideLength = 3.5 }
      |c: Shape = new Circle { radius = 1.0 }
      |""".stripMargin
    val pmodule = ev.evaluate(ModuleSource.text(code))
    assert(pmodule.getProperty("s").decodeAs[Shape] == Right(Square(3.5)))
    assert(pmodule.getProperty("c").decodeAs[Shape] == Right(Circle(1.0)))
  }

  test("sealed trait falls back to `_type` discriminator when class name doesn't match") { ev =>
    val code = """
      |value: Dynamic = new {
      |  _type = "Square"
      |  sideLength = 7.0
      |}
      |""".stripMargin
    val pmodule = ev.evaluate(ModuleSource.text(code))
    assert(pmodule.getProperty("value").decodeAs[Shape] == Right(Square(7.0)))
  }

  test("sealed trait errors when neither class name nor `_type` identifies a branch") { ev =>
    val code = """
      |value: Dynamic = new { sideLength = 1.0 }
      |""".stripMargin
    val pmodule = ev.evaluate(ModuleSource.text(code))
    val err = pmodule.getProperty("value").decodeAs[Shape].swap.toOption.get
    assert(err.getMessage.contains("Cannot determine sum branch"))
    assert(err.getMessage.contains("Square, Circle"))
  }
}

object DecoderSpec {
  case class Person(name: String, age: Int) derives Decoder
  case class Address(zip: String, city: String) derives Decoder
  case class Citizen(name: String, home: Address) derives Decoder

  enum Color derives Decoder { case Red, Green, Blue }

  sealed trait Shape derives Decoder
  case class Square(sideLength: Double) extends Shape derives Codec
  case class Circle(radius: Double) extends Shape derives Codec
}
