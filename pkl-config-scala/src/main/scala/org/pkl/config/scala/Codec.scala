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

import org.pkl.config.scala.codecs.*

import scala.deriving.Mirror
import scala.quoted.*

/** Encodes a Scala value into a Pkl runtime value (`String`, `java.lang.Long`, `PObject`, ...). */
type Encoder[T] = Codec.Encoder[T]

/** Decodes a Pkl runtime value (`String`, `java.lang.Long`, `PObject`, ...) into a Scala value. */
type Decoder[T] = Codec.Decoder[T]

/**
 * Term-level alias so `Encoder.instance(...)` / `Decoder.instance(...)` and the `derives` clauses
 * resolve to the canonical site.
 */
val Encoder = Codec.Encoder
val Decoder = Codec.Decoder

/**
 * A round-trip-capable typeclass bundling encode and decode for a single type.
 *
 * `Codec[T]` is a SUBTYPE of `Encoder[T]` and `Decoder[T]` (bottom-up design). Encoder and Decoder
 * are the primitives; `Codec` is the composite. Two consequences fall out of the subtyping:
 *
 *   - A single round-trippable instance written as `given Codec[T]` (primitives, collections,
 *     `Option`) directly satisfies `Encoder[T]` and `Decoder[T]` queries by subsumption — no bridge
 *     given is needed in either direction. (The old design had a `fromCodec` bridge in BOTH the
 *     `Encoder` and `Decoder` companions; that closed a Codec ⇄ Decoder cycle and forced a tower of
 *     priority traits to linearize it. Both bridges are gone.)
 *   - Mirror derivation lives on the two halves (`Encoder.derived` / `Decoder.derived`), each its own
 *     traversal, NOT on `Codec`. There is deliberately no derived-`Codec` given: such a given, being
 *     `<: Decoder`, would look MORE specific than a hand-written `Decoder` (e.g.
 *     `decoderForEither`) and silently win — the exact ambiguity that made an earlier subtyping
 *     attempt fail. With derivation on the halves, an `Either` decoder query sees two `Decoder`-typed
 *     candidates and ordinary linearization lets the hand-written one win.
 *
 * `Codec[T]` for a derivable type is assembled from its halves explicitly — via `Codec.from(e, d)`
 * or the `derives Codec` clause (which calls `Codec.derived = from(Encoder.derived, Decoder.derived)`).
 * There is deliberately NO implicit `Codec`-from-parts given: such a given would be `<: Decoder` and
 * `<: Encoder`, so it would re-enter `Decoder[T]`/`Encoder[T]` resolution as a universal, maximally
 * type-specific candidate and collide with hand-written direction-specific givens (the `Either` case:
 * type-specificity favours the parts given, owner-specificity favours `decoderForEither`, and Scala
 * reports ambiguity). Keeping the join explicit is the same boundary circe draws. Types for which one
 * direction is meaningful and the other isn't simply ship the half that exists; no `Codec`
 * materializes unless BOTH halves are assembled deliberately.
 *
 * The Pkl runtime value model that encoders produce and decoders consume:
 *   - boxed primitives: `java.lang.Long`, `java.lang.Double`, `java.lang.Boolean`, `String`
 *   - nullability: `null` or `PNull`
 *   - typed objects: `PObject` / `PModule`. Encoders construct `PObject(PClassInfo.Dynamic, ...)`
 *     so the result is a `Composite` that `PcfRenderer` / `JsonRenderer` render as a Pkl object.
 *   - listings: `java.util.Collection` on decode (whatever Pkl gave us); on encode we build a Scala
 *     `mutable.ArrayBuffer` and hand back its `.asJava` view.
 *   - mappings: `java.util.Map` on decode; on encode we build a Scala `mutable.LinkedHashMap` and
 *     hand back its `.asJava` view (insertion order preserved).
 *   - domain values: `org.pkl.core.{Duration, Pair}`, `byte[]`, `java.util.regex.Pattern`
 *
 * Instances are organized to match the bottom-up shape:
 *   - leaf bijections (primitives, `Duration`, `Instant`, `Regex`) ship as `given Codec[T]` in
 *     [[org.pkl.config.scala.codecs.PrimitiveCodecs]], mixed into THIS companion — they genuinely
 *     round-trip, so one instance serves all three queries by subsumption.
 *   - structural combinators (`Seq`/`List`/`Set`/`Map`/`Tuple2`, `Option`, `Either`) ship as
 *     per-direction givens mixed into the `Encoder` / `Decoder` companions, so each direction needs
 *     only the matching half of its element type (a `Listing<String|Int>` decodes fine even though
 *     `Either`/unions have no `Codec`).
 *   - composites (`derives`) derive each half independently via `Encoder.derived` / `Decoder.derived`.
 *
 * Mix-in order within a companion matters: traits later in the `extends` list are more specific by
 * linearization, so combinators/effects out-prioritize the `Mirror`-derived `derivedEncoder` /
 * `derivedDecoder`.
 */
trait Codec[T] extends Codec.Encoder[T], Codec.Decoder[T] { self =>

  /** Newtype-style profunctor map: derive a `Codec[U]` from a bijection between `T` and `U`. */
  def imap[U](f: T => U, g: U => T): Codec[U] = new Codec[U] {
    def decode(value: Any): Either[ConfigError, U] = self.decode(value).map(f)
    def encode(value: U): Any = self.encode(g(value))
  }
}

object Codec extends PrimitiveCodecs {

  // ---------- Encoder ----------

  trait Encoder[T] { self =>
    def encode(value: T): Any
    def contramap[U](f: U => T): Encoder[U] = new Encoder[U] {
      def encode(value: U): Any = self.encode(f(value))
    }
  }

  object Encoder extends LowPriorityEncoders, CollectionEncoders, EffectEncoders {
    def apply[T](using e: Encoder[T]): Encoder[T] = e

    def instance[T](f: T => Any): Encoder[T] = new Encoder[T] {
      def encode(value: T): Any = f(value)
    }

    /** Backs `derives Encoder` on case classes, sealed traits, and enums. */
    inline def derived[T](using m: Mirror.Of[T]): Encoder[T] = deriveEncoder[T]
  }

  // ---------- Decoder ----------

  trait Decoder[T] { self =>
    def decode(value: Any): Either[ConfigError, T]

    def map[U](f: T => U): Decoder[U] = new Decoder[U] {
      def decode(value: Any): Either[ConfigError, U] = self.decode(value).map(f)
    }

    def emap[U](f: T => Either[ConfigError, U]): Decoder[U] = new Decoder[U] {
      def decode(value: Any): Either[ConfigError, U] = self.decode(value).flatMap(f)
    }
  }

  object Decoder extends LowPriorityDecoders, CollectionDecoders, EffectDecoders {
    def apply[T](using d: Decoder[T]): Decoder[T] = d

    def instance[T](f: Any => Either[ConfigError, T]): Decoder[T] = new Decoder[T] {
      def decode(value: Any): Either[ConfigError, T] = f(value)
    }

    /** Backs `derives Decoder` on case classes, sealed traits, and enums. */
    inline def derived[T](using m: Mirror.Of[T]): Decoder[T] = deriveDecoder[T]
  }

  // ---------- Codec factories ----------

  def apply[T](using c: Codec[T]): Codec[T] = c

  def from[T](e: Encoder[T], d: Decoder[T]): Codec[T] = new Codec[T] {
    def encode(value: T): Any = e.encode(value)
    def decode(value: Any): Either[ConfigError, T] = d.decode(value)
  }

  def instance[T](enc: T => Any, dec: Any => Either[ConfigError, T]): Codec[T] = new Codec[T] {
    def encode(value: T): Any = enc(value)
    def decode(value: Any): Either[ConfigError, T] = dec(value)
  }

  /**
   * Backs `derives Codec`. Assembles a `Codec[T]` from the two independently-derived halves; a
   * derivable type therefore needs both an `Encoder` and a `Decoder` for every field. (A field for
   * which only one direction exists — e.g. an encode-only type — blocks `derives Codec`; ship that
   * field's missing half, or split the enclosing type's derivation.)
   */
  inline def derived[T](using m: Mirror.Of[T]): Codec[T] =
    from(Encoder.derived[T], Decoder.derived[T])

  /**
   * Sugar over [[instance]] for the common case where `dec` only handles a few specific types and
   * everything else is a [[ConfigError.TypeMismatch]] against the same expected name. The decoder
   * is supplied as a [[PartialFunction]]; unmatched inputs auto-fall-through to
   * `Left(ConfigError.TypeMismatch(typeName, value))`.
   */
  def named[T](
      typeName: String,
      enc: T => Any,
      dec: PartialFunction[Any, Either[ConfigError, T]]
  ): Codec[T] = new Codec[T] {
    def encode(value: T): Any = enc(value)
    def decode(value: Any): Either[ConfigError, T] =
      dec.applyOrElse(value, v => Left(ConfigError.TypeMismatch(typeName, v)))
  }
}

/**
 * Mid-priority `Encoder` derivation. Mixed into the `Encoder` companion BEFORE `EffectEncoders`, so
 * hand-written encoders (e.g. `encoderForEither`) win over the `Mirror`-derived sum/product encoder.
 */
sealed trait LowPriorityEncoders extends ProductTypeCodecs, SumTypeCodecs {

  inline def deriveEncoder[T](using m: Mirror.Of[T]): Encoder[T] =
    inline m match {
      case s: Mirror.SumOf[T]     => derivedSumEncoder[T](s)
      case p: Mirror.ProductOf[T] => derivedProductEncoder[T](p)
    }

  /** Auto-derives `Encoder[T]` for case classes, sealed traits, and enums; recursive on fields. */
  inline given derivedEncoder[T](using m: Mirror.Of[T]): Encoder[T] = deriveEncoder[T]
}

/**
 * Mid-priority `Decoder` derivation. Mixed into the `Decoder` companion BEFORE `EffectDecoders`
 * (so `decoderForEither` wins) and AFTER `LowestPriorityDecoders` (so a real Mirror-derived decoder
 * wins over the union/literal macro fallback).
 */
sealed trait LowPriorityDecoders extends ProductTypeCodecs, SumTypeCodecs, LowestPriorityDecoders {

  inline def deriveDecoder[T](using m: Mirror.Of[T]): Decoder[T] =
    inline m match {
      case s: Mirror.SumOf[T]     => derivedSumDecoder[T](s)
      case p: Mirror.ProductOf[T] => derivedProductDecoder[T](p)
    }

  /** Auto-derives `Decoder[T]` for case classes, sealed traits, and enums; recursive on fields. */
  inline given derivedDecoder[T](using m: Mirror.Of[T]): Decoder[T] = deriveDecoder[T]
}

/**
 * Lowest-priority `Decoder` given. The macro pattern-matches the call-site `TypeRepr`; only types
 * with no `given Codec`/`Decoder` and no `Mirror` reach this rule.
 */
sealed trait LowestPriorityDecoders {

  /**
   * Catch-all for Scala 3 unions (`A | B`) and singleton-literal types (`"hello"`, `42`, `true`).
   * Implemented as a quoted macro that pattern-matches the call-site `TypeRepr` and emits an
   * appropriate decoder. Compile-time error if `T` is none of those.
   */
  inline given derivedFromType[T]: Decoder[T] = ${ CodecMacros.derivedDecoderFromType[T] }
}

private[scala] object CodecMacros {

  def derivedDecoderFromType[T: Type](using Quotes): Expr[Decoder[T]] = {
    import quotes.reflect.*
    TypeRepr.of[T].dealias match {
      case OrType(left, right) =>
        left.asType match {
          case '[l] =>
            right.asType match {
              case '[r] =>
                val ldec = Expr.summon[Decoder[l]].getOrElse {
                  report.errorAndAbort(s"No Decoder for union branch `${left.show}`")
                }
                val rdec = Expr.summon[Decoder[r]].getOrElse {
                  report.errorAndAbort(s"No Decoder for union branch `${right.show}`")
                }
                '{
                  Decoder.instance[T] { value =>
                    $ldec.decode(value) match {
                      case Right(v) => Right(v.asInstanceOf[T])
                      case Left(le) =>
                        $rdec.decode(value) match {
                          case Right(v) => Right(v.asInstanceOf[T])
                          case Left(re) =>
                            Left(
                              ConfigError.Custom(
                                s"Cannot decode `$value` as either union branch: " +
                                  s"${le.getMessage}; ${re.getMessage}"
                              )
                            )
                        }
                    }
                  }
                }
            }
        }
      case ConstantType(StringConstant(s)) =>
        val expected = Expr(s)
        '{
          Decoder.instance[T] { value =>
            value match {
              case x: String if x == $expected => Right(x.asInstanceOf[T])
              case other                       =>
                Left(ConfigError.Custom(s"Expected singleton `${$expected}`, got `$other`"))
            }
          }
        }
      case ConstantType(BooleanConstant(b)) =>
        val expected = Expr(b)
        '{
          Decoder.instance[T] { value =>
            value match {
              case x: java.lang.Boolean if x == $expected => Right(x.asInstanceOf[T])
              case other                                  =>
                Left(ConfigError.Custom(s"Expected singleton `${$expected}`, got `$other`"))
            }
          }
        }
      case ConstantType(LongConstant(l)) =>
        val expected = Expr(l)
        '{
          Decoder.instance[T] { value =>
            value match {
              case x: java.lang.Long if x == $expected => Right(x.asInstanceOf[T])
              case other                               =>
                Left(ConfigError.Custom(s"Expected singleton `${$expected}`, got `$other`"))
            }
          }
        }
      case ConstantType(IntConstant(i)) =>
        // Scala Int singletons must compare against Pkl's java.lang.Long emit.
        val expected = Expr(i.toLong)
        '{
          Decoder.instance[T] { value =>
            value match {
              case x: java.lang.Long if x.longValue == $expected => Right(x.asInstanceOf[T])
              case other                                         =>
                Left(ConfigError.Custom(s"Expected singleton `${$expected}`, got `$other`"))
            }
          }
        }
      case other =>
        report.errorAndAbort(
          s"No given Decoder for `${other.show}`. " +
            "Provide one explicitly, or use `derives Decoder` if it's a case class."
        )
    }
  }
}
