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

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

/**
 * Exercises the encode side, which the decode-only [[DecoderSpec]] never touched. The property
 * under test is round-trip identity: `decode(encode(x)) == Right(x)`. Encode produces Pkl runtime
 * values (`java.lang.Long`, `PObject`, `java.util.List`, ...) purely in-memory — no `Evaluator`
 * needed — and decode consumes them back.
 *
 * Each value is summoned through `Encoder[T]` + `Decoder[T]` SEPARATELY (not a `Codec[T]`), so this
 * also proves the bottom-up halves resolve on their own: primitives via subsumption from the leaf
 * `given Codec`, combinators via the split per-direction givens, composites via `Encoder.derived` /
 * `Decoder.derived`, and `Either` via the matched hand-written encoder/decoder pair.
 */
class RoundTripSpec extends AnyFunSuite with TableDrivenPropertyChecks {

  import RoundTripSpec.*

  /** Encode then decode through independently-summoned halves. */
  def roundTrip[T](x: T)(using e: Encoder[T], d: Decoder[T]): Either[ConfigError, T] =
    d.decode(e.encode(x))

  // ---------- primitives (table-driven) ----------

  test("round-trip: Int") {
    forAll(Table("n", 0, 1, -1, 42, Int.MaxValue, Int.MinValue)) { n =>
      assert(roundTrip(n) == Right(n))
    }
  }

  test("round-trip: Long") {
    forAll(Table("n", 0L, 1L, -1L, Long.MaxValue, Long.MinValue)) { n =>
      assert(roundTrip(n) == Right(n))
    }
  }

  test("round-trip: Double") {
    forAll(Table("d", 0.0, 1.5, -2.25, 1e10, -3.14159)) { d =>
      assert(roundTrip(d) == Right(d))
    }
  }

  test("round-trip: Boolean") {
    forAll(Table("b", true, false)) { b =>
      assert(roundTrip(b) == Right(b))
    }
  }

  test("round-trip: String") {
    forAll(Table("s", "", "hello", "with spaces", "unicode-✓")) { s =>
      assert(roundTrip(s) == Right(s))
    }
  }

  // ---------- collections ----------

  test("round-trip: Seq[Int]") {
    assert(roundTrip(Seq(1, 2, 3)) == Right(Seq(1, 2, 3)))
  }

  test("round-trip: List[String]") {
    assert(roundTrip(List("a", "b")) == Right(List("a", "b")))
  }

  test("round-trip: Vector[Int]") {
    assert(roundTrip(Vector(7, 8, 9)) == Right(Vector(7, 8, 9)))
  }

  test("round-trip: Set[Int]") {
    assert(roundTrip(Set(1, 2, 3)) == Right(Set(1, 2, 3)))
  }

  test("round-trip: Map[String, Int] preserves entries") {
    assert(roundTrip(Map("a" -> 1, "b" -> 2)) == Right(Map("a" -> 1, "b" -> 2)))
  }

  test("round-trip: Tuple2") {
    assert(roundTrip((7, "seven")) == Right((7, "seven")))
  }

  test("round-trip: FiniteDuration preserves unit") {
    import scala.concurrent.duration.*
    forAll(Table("d", 5.millis, 3.seconds, 1.hour, 250.micros)) { d =>
      assert(roundTrip(d) == Right(d))
    }
  }

  // ---------- effects ----------

  test("round-trip: Option[Int] — Some and None") {
    assert(roundTrip(Option(5)) == Right(Some(5)))
    assert(roundTrip(Option.empty[Int]) == Right(None))
  }

  test("round-trip: Either[String, Int] — both sides (distinguishable shapes)") {
    assert(roundTrip[Either[String, Int]](Left("hi")) == Right(Left("hi")))
    assert(roundTrip[Either[String, Int]](Right(42)) == Right(Right(42)))
  }

  test("round-trip: collection of Either elements") {
    val xs = Seq(Left("a"), Right(1), Left("b"))
    assert(roundTrip[Seq[Either[String, Int]]](xs) == Right(xs))
  }

  // ---------- derived composites ----------

  test("round-trip: case class") {
    assert(roundTrip(Point(3, 4)) == Right(Point(3, 4)))
  }

  test("round-trip: nested case class") {
    val line = Line(Point(0, 0), Point(3, 4))
    assert(roundTrip(line) == Right(line))
  }

  test("round-trip: enum branches") {
    forAll(Table("c", Color.Red, Color.Green, Color.Blue)) { c =>
      assert(roundTrip(c) == Right(c))
    }
  }

  test("round-trip: sealed trait branches") {
    assert(roundTrip[Shape](Square(2.0)) == Right(Square(2.0)))
    assert(roundTrip[Shape](Circle(1.5)) == Right(Circle(1.5)))
  }
}

object RoundTripSpec {
  case class Point(x: Int, y: Int) derives Codec
  case class Line(from: Point, to: Point) derives Codec

  enum Color derives Codec { case Red, Green, Blue }

  sealed trait Shape derives Codec
  case class Square(side: Double) extends Shape derives Codec
  case class Circle(radius: Double) extends Shape derives Codec
}
