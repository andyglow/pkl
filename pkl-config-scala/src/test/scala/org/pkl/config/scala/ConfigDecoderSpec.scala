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
 * Spike validation for the typeclass-driven `ConfigDecoder` design. Touches enough shapes to judge
 * whether the API ergonomics are sane:
 *
 *   - primitives (Int, String, Boolean, Long)
 *   - generic collections (Seq, List, Set, Map)
 *   - Option (with present and null sources)
 *   - Either (direct dispatch)
 *   - Pair → Tuple2
 *   - case class via `Mirror.ProductOf` derivation (field-by-field, no java.lang.reflect)
 *   - nested case class
 *   - Scala 3 union via izumi `Tag` fallback
 *   - singleton string-literal via izumi `Tag` fallback
 *   - direct domain decoder (Duration)
 */
class ConfigDecoderSpec extends FixtureAnyFunSuite {

  import ConfigDecoderSpec.*

  type FixtureParam = Evaluator

  override def withFixture(test: OneArgTest): Outcome = {
    val ev = Evaluator.preconfigured()
    try withFixture(test.toNoArgTest(ev))
    finally ev.close()
  }

  test("primitive: Int via top-level property") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("value = 42"))
    assert(pmodule.as[Int]("value") == 42)
  }

  test("primitive: String via top-level property") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("""value = "hello""""))
    assert(pmodule.as[String]("value") == "hello")
  }

  test("Seq[Int] from Listing<Int>") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("value: Listing<Int> = new { 1; 2; 3 }"))
    assert(pmodule.as[Seq[Int]]("value") == Seq(1, 2, 3))
  }

  test("Set[Int] dedups Listing<Int>") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("value: Listing<Int> = new { 1; 2; 2; 3 }"))
    assert(pmodule.as[Set[Int]]("value") == Set(1, 2, 3))
  }

  test("Map[String, Int] from Mapping<String, Int>") { ev =>
    val pmodule = ev.evaluate(
      ModuleSource.text("""value: Mapping<String, Int> = new { ["a"] = 1; ["b"] = 2 }""")
    )
    assert(pmodule.as[Map[String, Int]]("value") == Map("a" -> 1, "b" -> 2))
  }

  test("Option[Int] is Some on present value") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("value: Int? = 5"))
    assert(pmodule.as[Option[Int]]("value") == Some(5))
  }

  test("Option[Int] is None on null") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("value: Int? = null"))
    assert(pmodule.as[Option[Int]]("value") == None)
  }

  test("Either[String, Int] dispatches by source — String → Left") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("""value: String | Int = "hi""""))
    assert(pmodule.as[Either[String, Int]]("value") == Left("hi"))
  }

  test("Either[String, Int] dispatches by source — Int → Right") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("value: String | Int = 42"))
    assert(pmodule.as[Either[String, Int]]("value") == Right(42))
  }

  test("Pair → (Int, String)") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("""value: Pair<Int, String> = Pair(7, "seven")"""))
    assert(pmodule.as[(Int, String)]("value") == ((7, "seven")))
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

  test("Duration via domain decoder preserves unit") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("value: Duration = 5.ms"))
    val d = pmodule.as[scala.concurrent.duration.Duration]("value")
    import scala.concurrent.duration.*
    assert(d == FiniteDuration(5, MILLISECONDS))
  }

  test("union Int | Boolean — Int side via Tag fallback") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("value: Int | Boolean = 42"))
    val r: Int | Boolean = pmodule.as[Int | Boolean]("value")
    assert(r == 42)
  }

  test("union Int | Boolean — Boolean side via Tag fallback") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("value: Int | Boolean = true"))
    val r: Int | Boolean = pmodule.as[Int | Boolean]("value")
    assert(r == true)
  }

  test("""singleton "hello" succeeds when source matches""") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("""value: "hello" = "hello""""))
    assert(pmodule.as["hello"]("value") == "hello")
  }

  test("""singleton "hello" errors when source doesn't match""") { ev =>
    val pmodule = ev.evaluate(ModuleSource.text("""value: String = "world""""))
    intercept[ConfigError] {
      pmodule.as["hello"]("value")
    }
  }
}

object ConfigDecoderSpec {
  case class Person(name: String, age: Int) derives ConfigDecoder
  case class Address(zip: String, city: String) derives ConfigDecoder
  case class Citizen(name: String, home: Address) derives ConfigDecoder
}
