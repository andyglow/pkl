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
 * Term-level alias so `Decoder.instance(...)` and `derives Decoder` resolve to the canonical site.
 */
val Encoder = Codec.Encoder
val Decoder = Codec.Decoder

/**
 * A round-trip-capable typeclass bundling encode and decode for a single type.
 *
 * Codec is a separate trait, NOT a subtype of `Encoder[T]` and `Decoder[T]`. Subtyping would make
 * `Codec.derived` a candidate for `Decoder[T]` queries at the same priority as hand-written
 * decoders (e.g. `Decoder.decoderForEither`), with no clean way to disambiguate. With subtyping
 * dropped, `Decoder.fromCodec[T]` (defined in `LowPriorityDecoders`) is the explicit bridge —
 * lower priority than direct decoders, higher than the macro fallback for unions/literals.
 *
 * Round-trippable types ship as `given Codec[T]` so a single instance covers both directions; types
 * for which one side is meaningless (e.g. `Either[L, R]`) ship a `given Decoder[T]` (or
 * `given Encoder[T]`) only.
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
 * Category-specific given instances live under `org.pkl.config.scala.codecs.*` and are mixed into
 * this companion (see the `extends` clause on `object Codec`). Mix-in order matters: traits later
 * in the list are more specific by linearization, so primitives/collections/effects out-prioritize
 * the Mirror-derived `LowPriorityCodecs.derived`.
 */
trait Codec[T] { self =>
  def encode(value: T): Any
  def decode(value: Any): Either[ConfigError, T]

  /** Newtype-style profunctor map: derive a `Codec[U]` from a bijection between `T` and `U`. */
  def imap[U](f: T => U, g: U => T): Codec[U] = new Codec[U] {
    def decode(value: Any): Either[ConfigError, U] = self.decode(value).map(f)
    def encode(value: U): Any = self.encode(g(value))
  }
}

object Codec extends LowPriorityCodecs, PrimitiveCodecs, CollectionCodecs, EffectCodecs {

  // ---------- Encoder ----------

  trait Encoder[T] { self =>
    def encode(value: T): Any
    def contramap[U](f: U => T): Encoder[U] = new Encoder[U] {
      def encode(value: U): Any = self.encode(f(value))
    }
  }

  object Encoder {
    def apply[T](using e: Encoder[T]): Encoder[T] = e

    def instance[T](f: T => Any): Encoder[T] = new Encoder[T] {
      def encode(value: T): Any = f(value)
    }

    /** Wraps any `Codec[T]` as an `Encoder[T]`. */
    given fromCodec[T](using c: Codec[T]): Encoder[T] = instance(c.encode)

    /** Used by `derives Encoder` on case classes, sealed traits, and enums; delegates to `Codec.derived`. */
    inline def derived[T](using m: Mirror.Of[T]): Encoder[T] = instance(Codec.derived[T].encode)
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

  object Decoder extends LowPriorityDecoders, EffectDecoders {
    def apply[T](using d: Decoder[T]): Decoder[T] = d

    def instance[T](f: Any => Either[ConfigError, T]): Decoder[T] = new Decoder[T] {
      def decode(value: Any): Either[ConfigError, T] = f(value)
    }

    /** Used by `derives Decoder` on case classes, sealed traits, and enums; delegates to `Codec.derived`. */
    inline def derived[T](using m: Mirror.Of[T]): Decoder[T] = instance(Codec.derived[T].decode)
  }

  /**
   * Mid-priority givens. Loses to hand-written direct members (e.g. `decoderForEither`); wins over
   * the macro fallback for unions/literals.
   */
  sealed trait LowPriorityDecoders extends LowestPriorityDecoders {

    /** Wraps any `Codec[T]` as a `Decoder[T]` — covers products, sealed sums, primitives, etc. */
    given fromCodec[T](using c: Codec[T]): Decoder[T] = Decoder.instance(c.decode)
  }

  /**
   * Lowest-priority given. The macro pattern-matches the call-site `TypeRepr`; only types with no
   * `Codec[T]` and no hand-written `Decoder[T]` reach this rule.
   */
  sealed trait LowestPriorityDecoders {

    /**
     * Catch-all for Scala 3 unions (`A | B`) and singleton-literal types (`"hello"`, `42`, `true`).
     * Implemented as a quoted macro that pattern-matches the call-site `TypeRepr` and emits an
     * appropriate decoder. Compile-time error if `T` is none of those.
     */
    inline given derivedFromType[T]: Decoder[T] = ${ CodecMacros.derivedDecoderFromType[T] }
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
 * `Codec.derived` lives here, one priority tier below `Codec`'s direct givens, so that hand-written
 * decoders/encoders on companions of types like `Either` win over Mirror-driven derivation. (The
 * `Codec` object is in the implicit scope of `Decoder[T]` because `Decoder` is a member of `Codec`;
 * if `derived` lived directly on `Codec` it would compete head-to-head with `Decoder.decoderForEither`
 * and produce ambiguity errors.)
 *
 * Sum and product machinery live in `codecs.{ProductTypeCodecs, SumTypeCodecs}`; this trait
 * inherits both and contributes only the dispatching `derived` given. Sealed so only `Codec`
 * extends it.
 */
sealed trait LowPriorityCodecs extends ProductTypeCodecs, SumTypeCodecs {

  /**
   * Auto-derives `Codec[T]` for case classes, sealed traits, and Scala 3 enums via `Mirror`.
   *
   *   - Product (case class, including case object as zero-field product): encode to
   *     `PObject(PClassInfo.Dynamic, ...)`; decode field-by-field from a `PObject`'s properties.
   *   - Sum (sealed trait, enum): encode by dispatching to the branch Codec and re-tagging the
   *     resulting `PObject` with the branch's name; decode by matching
   *     `PObject.getClassInfo.getSimpleName` against branch labels, falling back to a `_type`
   *     discriminator field. Pure-singleton enums (every branch a case object) currently encode as
   *     PObject too — a future step may special-case them to encode as a `String` of the case name.
   *
   * Backs `derives Codec`, `derives Decoder`, `derives Encoder`.
   */
  inline given derived[T](using m: Mirror.Of[T]): Codec[T] = {
    inline m match {
      case s: Mirror.SumOf[T]     => derivedSum[T](s)
      case p: Mirror.ProductOf[T] => derivedProduct[T](p)
    }
  }
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
