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

import org.pkl.core.{DurationUnit, PClassInfo, PNull, PObject, Duration as PDuration, Pair as PPair}

import scala.collection.mutable
import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.deriving.Mirror
import scala.jdk.CollectionConverters.*
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

object Codec extends LowPriorityCodecs {

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

  object Decoder extends LowPriorityDecoders {
    def apply[T](using d: Decoder[T]): Decoder[T] = d

    def instance[T](f: Any => Either[ConfigError, T]): Decoder[T] = new Decoder[T] {
      def decode(value: Any): Either[ConfigError, T] = f(value)
    }

    /**
     * `Either[L, R]` is decoder-only: Pkl unions decode by source dispatch (try `L`, fall back to
     * `R`); encoding back has no canonical inverse since the source union dropped the tag.
     *
     * Direct member of the companion (highest priority) so it wins over the auto-derived
     * `Codec[Either[L, R]]` reachable through `LowPriorityCodecs.derived` — Either is a Scala
     * sealed sum (so it has a `Mirror.SumOf`), but we want union-style source dispatch instead of
     * the default class-name dispatch.
     */
    given decoderForEither[L, R](using ld: Decoder[L], rd: Decoder[R]): Decoder[Either[L, R]] = {
      instance { value =>
        ld.decode(value) match {
          case Right(l) => Right(Left(l))
          case Left(_)  => rd.decode(value).map(Right(_))
        }
      }
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

  // ---------- Primitive instances ----------

  given Codec[String] = instance(
    enc = (s: String) => s,
    dec = {
      case s: String => Right(s)
      case other     => Left(ConfigError.TypeMismatch("String", other))
    }
  )

  given Codec[Boolean] = instance(
    enc = (b: Boolean) => java.lang.Boolean.valueOf(b),
    dec = {
      case b: java.lang.Boolean => Right(b.booleanValue)
      case other                => Left(ConfigError.TypeMismatch("Boolean", other))
    }
  )

  given Codec[Long] = instance(
    enc = (l: Long) => java.lang.Long.valueOf(l),
    dec = {
      case l: java.lang.Long => Right(l.longValue)
      case other             => Left(ConfigError.TypeMismatch("Long", other))
    }
  )

  given Codec[Int] = instance(
    enc = (i: Int) => java.lang.Long.valueOf(i.toLong),
    dec = {
      case l: java.lang.Long if l.longValue.isValidInt => Right(l.intValue)
      case l: java.lang.Long                           =>
        Left(ConfigError.Custom(s"Pkl Int $l does not fit in Scala Int (32-bit)."))
      case other => Left(ConfigError.TypeMismatch("Int", other))
    }
  )

  given Codec[Double] = instance(
    enc = (d: Double) => java.lang.Double.valueOf(d),
    dec = {
      case d: java.lang.Double => Right(d.doubleValue)
      case l: java.lang.Long   => Right(l.doubleValue)
      case other               => Left(ConfigError.TypeMismatch("Double", other))
    }
  )

  given Codec[Float] = instance(
    enc = (f: Float) => java.lang.Double.valueOf(f.toDouble),
    dec = {
      case d: java.lang.Double => Right(d.floatValue)
      case l: java.lang.Long   => Right(l.floatValue)
      case other               => Left(ConfigError.TypeMismatch("Float", other))
    }
  )

  // ---------- Higher-kinded instances ----------

  given [A](using c: Codec[A]): Codec[Option[A]] = instance(
    enc = {
      case Some(a) => c.encode(a)
      case None    => null
    },
    dec = {
      case null | _: PNull => Right(None)
      case other           => c.decode(other).map(Some(_))
    }
  )

  given [A](using c: Codec[A]): Codec[Seq[A]] = instance(
    enc = (xs: Seq[A]) => encodeListing(xs, c),
    dec = {
      case xs: java.util.Collection[?] => decodeCollection(xs, c).map(_.toSeq)
      case other                       => Left(ConfigError.TypeMismatch("Listing/List", other))
    }
  )

  given [A](using c: Codec[A]): Codec[List[A]] = instance(
    enc = (xs: List[A]) => encodeListing(xs, c),
    dec = {
      case xs: java.util.Collection[?] => decodeCollection(xs, c).map(_.toList)
      case other                       => Left(ConfigError.TypeMismatch("Listing/List", other))
    }
  )

  given [A](using c: Codec[A]): Codec[Vector[A]] = instance(
    enc = (xs: Vector[A]) => encodeListing(xs, c),
    dec = {
      case xs: java.util.Collection[?] => decodeCollection(xs, c).map(_.toVector)
      case other                       => Left(ConfigError.TypeMismatch("Listing/List", other))
    }
  )

  given [A](using c: Codec[A]): Codec[Set[A]] = instance(
    enc = (xs: Set[A]) => encodeListing(xs, c),
    dec = {
      case xs: java.util.Collection[?] => decodeCollection(xs, c).map(_.toSet)
      case other                       => Left(ConfigError.TypeMismatch("Listing/List/Set", other))
    }
  )

  given [K, V](using kc: Codec[K], vc: Codec[V]): Codec[Map[K, V]] = instance(
    enc = (m: Map[K, V]) => encodeMapping(m, kc, vc),
    dec = {
      case m: java.util.Map[?, ?] =>
        val results = m.asScala.iterator.map { (k, v) =>
          for {
            dk <- kc.decode(k)
            dv <- vc.decode(v)
          } yield (dk, dv)
        }.toList
        sequence(results).map(_.toMap)
      case other => Left(ConfigError.TypeMismatch("Mapping/Map", other))
    }
  )

  given [A, B](using ca: Codec[A], cb: Codec[B]): Codec[(A, B)] = instance(
    enc = (t: (A, B)) => new PPair(ca.encode(t._1), cb.encode(t._2)),
    dec = {
      case p: PPair[?, ?] =>
        for {
          a <- ca.decode(p.getFirst)
          b <- cb.decode(p.getSecond)
        } yield (a, b)
      case other => Left(ConfigError.TypeMismatch("Pair", other))
    }
  )

  // ---------- Domain instances ----------

  given Codec[scala.concurrent.duration.Duration] = instance(
    enc = {
      case fd: scala.concurrent.duration.FiniteDuration => scalaDurationToPkl(fd)
      case _                                            =>
        // Pkl has no infinite duration; lossy. Real fix: refuse at compile time via a `FiniteDuration` codec.
        null
    },
    dec = {
      case d: PDuration =>
        val v = d.getValue
        val unit = d.getUnit.toTimeUnit
        val l = v.toLong
        Right(
          if (v.isFinite && l.toDouble == v) {
            scala.concurrent.duration.FiniteDuration(l, unit)
          } else {
            scala.concurrent.duration
              .FiniteDuration(d.inWholeNanos(), java.util.concurrent.TimeUnit.NANOSECONDS)
              .toCoarsest
          }
        )
      case other => Left(ConfigError.TypeMismatch("Duration", other))
    }
  )

  given Codec[scala.concurrent.duration.FiniteDuration] = {
    val parent = summon[Codec[scala.concurrent.duration.Duration]]
    instance(
      enc = (fd: scala.concurrent.duration.FiniteDuration) => parent.encode(fd),
      dec = (v: Any) => {
        parent.decode(v).flatMap {
          case fd: scala.concurrent.duration.FiniteDuration => Right(fd)
          case _ => Left(ConfigError.Custom("Pkl Duration is not finite"))
        }
      }
    )
  }

  given Codec[java.time.Instant] = instance(
    enc = (i: java.time.Instant) => i.toString, // ISO-8601
    dec = {
      case s: String         => Right(java.time.Instant.parse(s))
      case l: java.lang.Long => Right(java.time.Instant.ofEpochMilli(l))
      case other             => Left(ConfigError.TypeMismatch("Instant", other))
    }
  )

  given Codec[scala.util.matching.Regex] = instance(
    enc = (r: scala.util.matching.Regex) => java.util.regex.Pattern.compile(r.pattern.pattern),
    dec = {
      case s: String                  => Right(s.r)
      case p: java.util.regex.Pattern => Right(p.pattern.r)
      case other                      => Left(ConfigError.TypeMismatch("Regex/String", other))
    }
  )
}

/**
 * `Codec.derived` lives here, one priority tier below `Codec`'s direct givens, so that hand-written
 * decoders/encoders on companions of types like `Either` win over Mirror-driven derivation. (The
 * `Codec` object is in the implicit scope of `Decoder[T]` because `Decoder` is a member of `Codec`;
 * if `derived` lived directly on `Codec` it would compete head-to-head with `Decoder.decoderForEither`
 * and produce ambiguity errors.)
 *
 * The derivation machinery (helpers, sum/product/singleton codecs) lives here too so the inline
 * given can call them at expansion time. Members are public to satisfy inline-call visibility at
 * the user's call site, but the trait is sealed and only `Codec` extends it.
 */
sealed trait LowPriorityCodecs {

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

  inline def derivedProduct[T](m: Mirror.ProductOf[T]): Codec[T] =
    productCodec[T](summonLabels[m.MirroredElemLabels], summonCodecs[m.MirroredElemTypes], m)

  inline def derivedSum[T](m: Mirror.SumOf[T]): Codec[T] =
    sumCodec[T](
      summonLabels[m.MirroredElemLabels],
      summonCodecs[m.MirroredElemTypes],
      m
    )

  /**
   * Non-inline assembly for Mirror-derived product codecs. Keeping this out-of-line means the
   * anonymous class is defined once, not duplicated at every `derives` call site.
   *
   * `T` is `Mirror.ProductOf.MirroredMonoType` which by the Mirror contract `<: Product`; the cast
   * inside `encode` reflects that contract — required because we can't bound `T` at the (inline)
   * entry point without forcing every caller to satisfy it.
   */
  def productCodec[T](
      labels: List[String],
      codecs: List[Codec[Any]],
      m: Mirror.ProductOf[T]
  ): Codec[T] = new Codec[T] {

    def decode(value: Any): Either[ConfigError, T] = value match {
      case obj: PObject =>
        val props = obj.getProperties.asScala
        val perField = labels.zip(codecs).map { (label, c) =>
          props.get(label) match {
            case None      => Left(ConfigError.MissingProperty(label))
            case Some(raw) => c.decode(raw).left.map(_.at(label))
          }
        }
        sequence(perField).map(values => m.fromProduct(Tuple.fromArray(values.toArray)))
      case other =>
        Left(ConfigError.TypeMismatch("PObject", other))
    }

    /**
     * Encode as `PObject` with `PClassInfo.Dynamic` — a `Composite` that `PcfRenderer` /
     * `JsonRenderer` / etc. render as a Pkl object.
     */
    def encode(value: T): Any = {
      val product = value.asInstanceOf[Product]
      val props = mutable.LinkedHashMap.empty[String, Any]
      labels.iterator.zip(codecs.iterator).zipWithIndex.foreach { case ((label, c), i) =>
        props.update(label, c.encode(product.productElement(i)))
      }
      // PObject's Java constructor takes `Map<String, Object>`. The cast bridges Scala `Any` and
      // Java `Object` — same erasure at runtime.
      new PObject(PClassInfo.Dynamic, props.asJava.asInstanceOf[java.util.Map[String, Object]])
    }
  }

  /**
   * Sum codec for sealed traits / enums. Encode picks the branch via `m.ordinal`, dispatches to the
   * branch's Codec, then re-tags the resulting `PObject` with a `PClassInfo` carrying the branch's
   * name. Decode dispatches by class name (primary) or `_type` discriminator (fallback).
   *
   * Case-object branches go through the `Mirror.ProductOf` (singleton extends product) path, so
   * their Codec encodes/decodes an empty PObject — the branch identity lives in the surrounding
   * `PClassInfo`. A future step may special-case all-singleton sums to encode as a String of the
   * branch name (more idiomatic for pure enums); deferred because the compile-time singleton check
   * via `inline if + summonFrom` doesn't reduce to a literal Boolean in the current Scala 3.
   */
  def sumCodec[T](
      branchLabels: List[String],
      branchCodecs: List[Codec[Any]],
      m: Mirror.SumOf[T]
  ): Codec[T] = new Codec[T] {

    def decode(value: Any): Either[ConfigError, T] = value match {
      case obj: PObject =>
        val byClassName = branchLabels.indexOf(obj.getClassInfo.getSimpleName)
        val branchIndex =
          if (byClassName >= 0) Right(byClassName)
          else
            obj.getProperties.asScala.get(DiscriminatorField) match {
              case Some(s: String) =>
                branchLabels.indexOf(s) match {
                  case -1 =>
                    Left(
                      ConfigError.Custom(
                        s"Unknown sum branch `$s`. Expected one of: ${branchLabels.mkString(", ")}"
                      )
                    )
                  case i => Right(i)
                }
              case _ =>
                Left(
                  ConfigError.Custom(
                    s"Cannot determine sum branch from class `${obj.getClassInfo.getDisplayName}` " +
                      s"and no `$DiscriminatorField` discriminator was found. " +
                      s"Expected one of: ${branchLabels.mkString(", ")}"
                  )
                )
            }
        branchIndex.flatMap(i => branchCodecs(i).decode(obj).map(_.asInstanceOf[T]))
      case other =>
        Left(ConfigError.TypeMismatch("PObject (sealed sum)", other))
    }

    def encode(value: T): Any = {
      val i = m.ordinal(value)
      val branchName = branchLabels(i)
      branchCodecs(i).encode(value) match {
        case obj: PObject =>
          new PObject(brandedClassInfo(branchName), obj.getProperties)
        case other => other
      }
    }
  }

  /** Discriminator field name for sum decode fallback. */
  val DiscriminatorField = "_type"

  /**
   * Synthesize a `PClassInfo` carrying the branch's name. We don't have a real Pkl module to
   * attach it to, so we pick a synthetic moduleName/URI; the value is only consumed by our own
   * sum-decoder dispatch, which only reads `getSimpleName`.
   */
  def brandedClassInfo(branchName: String): PClassInfo[?] =
    PClassInfo.get("$scala.sum", branchName, java.net.URI.create("scala:sum"))

  inline def summonLabels[Labels <: Tuple]: List[String] = {
    inline erasedValue[Labels] match {
      case _: EmptyTuple => Nil
      case _: (h *: t)   => constValue[h].asInstanceOf[String] :: summonLabels[t]
    }
  }

  inline def summonCodecs[Types <: Tuple]: List[Codec[Any]] = {
    inline erasedValue[Types] match {
      case _: EmptyTuple => Nil
      case _: (h *: t)   =>
        summonInline[Codec[h]].asInstanceOf[Codec[Any]] :: summonCodecs[t]
    }
  }

  // ---------- Shared helpers (used by both derivation and primitive instances) ----------

  /**
   * Pkl Listings/Lists are rendered via `instanceof java.util.List`, so the encoded form must be a
   * Java List. Build with a Scala `ArrayBuffer` (O(1) append, ordered) and convert at the boundary.
   */
  def encodeListing[A](xs: Iterable[A], c: Codec[A]): java.util.List[Any] = {
    val buf = mutable.ArrayBuffer.empty[Any]
    xs.foreach(x => buf += c.encode(x))
    buf.asJava
  }

  /** Pkl Mapping is a `LinkedHashMap` (insertion-ordered); use the Scala equivalent and convert. */
  def encodeMapping[K, V](
      m: Map[K, V],
      kc: Codec[K],
      vc: Codec[V]
  ): java.util.Map[Any, Any] = {
    val out = mutable.LinkedHashMap.empty[Any, Any]
    m.foreach { (k, v) => out.update(kc.encode(k), vc.encode(v)) }
    out.asJava
  }

  def decodeCollection[A](
      xs: java.util.Collection[?],
      c: Codec[A]
  ): Either[ConfigError, IndexedSeq[A]] = {
    val results = xs.asScala.iterator.map(c.decode).toList
    sequence(results).map(_.toIndexedSeq)
  }

  /** First-error-wins; returns the list of decoded values if all succeeded. */
  def sequence[A](
      results: Iterable[Either[ConfigError, A]]
  ): Either[ConfigError, List[A]] = {
    results.iterator
      .foldLeft[Either[ConfigError, List[A]]](Right(Nil)) { (acc, next) =>
        acc.flatMap(xs => next.map(_ :: xs))
      }
      .map(_.reverse)
  }

  def scalaDurationToPkl(fd: scala.concurrent.duration.FiniteDuration): PDuration = {
    import java.util.concurrent.TimeUnit
    val unit = fd.unit match {
      case TimeUnit.NANOSECONDS  => DurationUnit.NANOS
      case TimeUnit.MICROSECONDS => DurationUnit.MICROS
      case TimeUnit.MILLISECONDS => DurationUnit.MILLIS
      case TimeUnit.SECONDS      => DurationUnit.SECONDS
      case TimeUnit.MINUTES      => DurationUnit.MINUTES
      case TimeUnit.HOURS        => DurationUnit.HOURS
      case TimeUnit.DAYS         => DurationUnit.DAYS
    }
    new PDuration(fd.length.toDouble, unit)
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
