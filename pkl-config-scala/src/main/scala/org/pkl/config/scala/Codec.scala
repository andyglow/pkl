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

import scala.compiletime.{constValue, erasedValue, summonInline}
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
 * A round-trip-capable typeclass: `Encoder[T] with Decoder[T]`.
 *
 * Round-trippable types ship as `given Codec[T]` so a single instance covers both directions; types
 * for which one side is meaningless (e.g. `Either[L, R]` — Pkl has no native Either, encoding has
 * to pick a side) ship a `given Decoder[T]` (or `given Encoder[T]`) only.
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
trait Codec[T] extends Codec.Encoder[T], Codec.Decoder[T] { self =>

  /** Newtype-style profunctor map: derive a `Codec[U]` from a bijection between `T` and `U`. */
  def imap[U](f: T => U, g: U => T): Codec[U] = new Codec[U] {
    def decode(value: Any): Either[ConfigError, U] = self.decode(value).map(f)
    def encode(value: U): Any = self.encode(g(value))
  }
}

object Codec {

  import scala.collection.mutable
  import scala.jdk.CollectionConverters.*

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

    /** Subsumes any `Codec[T]` as `Encoder[T]`. */
    given fromCodec[T](using c: Codec[T]): Encoder[T] = c

    /** Used by `derives Encoder` on case classes; delegates to `Codec.derived`. */
    inline def derived[T <: Product](using m: Mirror.ProductOf[T]): Encoder[T] = Codec.derived[T]
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

    /** Subsumes any `Codec[T]` as `Decoder[T]`. */
    given fromCodec[T](using c: Codec[T]): Decoder[T] = c

    /**
     * `Either[L, R]` is decoder-only: Pkl unions decode by source dispatch (try `L`, fall back to
     * `R`); encoding back has no canonical inverse since the source union dropped the tag.
     */
    given decoderForEither[L, R](using ld: Decoder[L], rd: Decoder[R]): Decoder[Either[L, R]] = {
      instance { value =>
        ld.decode(value) match {
          case Right(l) => Right(Left(l))
          case Left(_)  => rd.decode(value).map(Right(_))
        }
      }
    }

    /** Used by `derives Decoder` on case classes; delegates to `Codec.derived`. */
    inline def derived[T <: Product](using m: Mirror.ProductOf[T]): Decoder[T] = Codec.derived[T]
  }

  /**
   * Lower-priority givens. Members defined here lose to direct members of `Decoder` companion, so
   * `fromCodec[T]` (specific) wins over the macro fallback for any type with a `Codec[T]`.
   */
  sealed trait LowPriorityDecoders {

    /**
     * Catch-all for Scala 3 unions (`A | B`) and singleton-literal types (`"hello"`, `42`, `true`).
     * Implemented as a quoted macro that pattern-matches the call-site `TypeRepr` and emits an
     * appropriate decoder. Compile-time error if `T` is none of those (and has no other in-scope
     * decoder, since this rule is the lowest priority).
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

  // ---------- Mirror-based derivation for products ----------

  /**
   * Auto-derives `Codec[T]` for any case class whose every field has a `Codec`. Encodes to a
   * `PObject(PClassInfo.Dynamic, ...)` so renderers emit a Pkl object; decodes from a `PObject`
   * (which is what evaluation produces).
   *
   * Backs `derives Codec`, `derives Decoder`, `derives Encoder` on products. Sum types (sealed
   * traits, Scala 3 enums) need a `Mirror.SumOf`-driven companion derivation — TODO.
   */
  inline given derived[T <: Product](using m: Mirror.ProductOf[T]): Codec[T] = {
    productCodec[T](summonLabels[m.MirroredElemLabels], summonCodecs[m.MirroredElemTypes], m)
  }

  /**
   * Non-inline assembly for Mirror-derived product codecs. Keeping this out-of-line means the
   * anonymous class is defined once, not duplicated at every `derives` call site.
   */
  private def productCodec[T <: Product](
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
     * `JsonRenderer` / etc. render as a Pkl object. We don't carry the original Scala class name
     * forward (we'd need a synthesized `PClassInfo` that round-trips through Pkl evaluation, which
     * is out of scope), so all derived products land as Dynamic.
     */
    def encode(value: T): Any = {
      val props = mutable.LinkedHashMap.empty[String, Any]
      labels.iterator.zip(codecs.iterator).zipWithIndex.foreach { case ((label, c), i) =>
        props.update(label, c.encode(value.productElement(i)))
      }
      // PObject's Java constructor takes `Map<String, Object>`. The cast bridges Scala `Any` and
      // Java `Object` — same erasure at runtime.
      new PObject(PClassInfo.Dynamic, props.asJava.asInstanceOf[java.util.Map[String, Object]])
    }
  }

  inline private def summonLabels[Labels <: Tuple]: List[String] = {
    inline erasedValue[Labels] match {
      case _: EmptyTuple => Nil
      case _: (h *: t)   => constValue[h].asInstanceOf[String] :: summonLabels[t]
    }
  }

  inline private def summonCodecs[Types <: Tuple]: List[Codec[Any]] = {
    inline erasedValue[Types] match {
      case _: EmptyTuple => Nil
      case _: (h *: t)   =>
        summonInline[Codec[h]].asInstanceOf[Codec[Any]] :: summonCodecs[t]
    }
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

  // ---------- Helpers ----------

  /**
   * Pkl Listings/Lists are rendered via `instanceof java.util.List`, so the encoded form must be a
   * Java List. Build with a Scala `ArrayBuffer` (O(1) append, ordered) and convert at the boundary.
   */
  private def encodeListing[A](xs: Iterable[A], c: Codec[A]): java.util.List[Any] = {
    val buf = mutable.ArrayBuffer.empty[Any]
    xs.foreach(x => buf += c.encode(x))
    buf.asJava
  }

  /** Pkl Mapping is a `LinkedHashMap` (insertion-ordered); use the Scala equivalent and convert. */
  private def encodeMapping[K, V](
      m: Map[K, V],
      kc: Codec[K],
      vc: Codec[V]
  ): java.util.Map[Any, Any] = {
    val out = mutable.LinkedHashMap.empty[Any, Any]
    m.foreach { (k, v) => out.update(kc.encode(k), vc.encode(v)) }
    out.asJava
  }

  private def decodeCollection[A](
      xs: java.util.Collection[?],
      d: Decoder[A]
  ): Either[ConfigError, IndexedSeq[A]] = {
    val results = xs.asScala.iterator.map(d.decode).toList
    sequence(results).map(_.toIndexedSeq)
  }

  /** First-error-wins; returns the list of decoded values if all succeeded. */
  private def sequence[A](
      results: Iterable[Either[ConfigError, A]]
  ): Either[ConfigError, List[A]] = {
    results.iterator
      .foldLeft[Either[ConfigError, List[A]]](Right(Nil)) { (acc, next) =>
        acc.flatMap(xs => next.map(_ :: xs))
      }
      .map(_.reverse)
  }

  private def scalaDurationToPkl(fd: scala.concurrent.duration.FiniteDuration): PDuration = {
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
