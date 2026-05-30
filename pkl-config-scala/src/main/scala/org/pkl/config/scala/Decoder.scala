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

import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTagRef
import izumi.reflect.macrortti.LightTypeTagRef.{NameReference, SymName, UnionReference}
import org.pkl.core.{PNull, PObject}

import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.deriving.Mirror

/**
 * Decodes a Pkl runtime value into a Scala value of type `T`.
 *
 * Pkl evaluates to a small set of JVM types: boxed primitives (`java.lang.Long`,
 * `java.lang.Double`, `java.lang.String`, `java.lang.Boolean`), `null` / `PNull` for nullability,
 * `PObject` (and `PModule`) for typed objects, `java.util.List` / `java.util.ArrayList` for
 * Listings and Lists, `java.util.Map` / `java.util.LinkedHashMap` for Mappings and Maps, plus
 * domain values (`Duration`, `DataSize`, `Pair`, `byte[]`, `java.util.regex.Pattern`).
 *
 * A `Decoder[T]` walks that runtime model and produces either a typed `T` or a `ConfigError`
 * carrying a path for navigation.
 *
 * Implementations are typeclass instances: provided as `given`s in this companion for primitives
 * and the standard collection/option/either shapes; auto-derived via `Mirror.ProductOf` for case
 * classes and `Mirror.SumOf` for Scala 3 enums; auto-derived via izumi `Tag` for Scala 3 union and
 * singleton-literal types.
 */
trait Decoder[T] {
  def decode(value: Any): Either[ConfigError, T]
}

object Decoder {

  def apply[T](implicit d: Decoder[T]): Decoder[T] = d

  def instance[T](f: Any => Either[ConfigError, T]): Decoder[T] = {
    new Decoder[T] { def decode(value: Any): Either[ConfigError, T] = f(value) }
  }

  // ---------- Primitive instances ----------

  given Decoder[String] = instance {
    case s: String => Right(s)
    case other     => Left(ConfigError.TypeMismatch("String", other))
  }

  given Decoder[Boolean] = instance {
    case b: java.lang.Boolean => Right(b.booleanValue)
    case other                => Left(ConfigError.TypeMismatch("Boolean", other))
  }

  given Decoder[Long] = instance {
    case l: java.lang.Long => Right(l.longValue)
    case other             => Left(ConfigError.TypeMismatch("Long", other))
  }

  given Decoder[Int] = instance {
    case l: java.lang.Long if l.longValue.isValidInt => Right(l.intValue)
    case l: java.lang.Long                           =>
      Left(ConfigError.Custom(s"Pkl Int $l does not fit in Scala Int (32-bit)."))
    case other => Left(ConfigError.TypeMismatch("Int", other))
  }

  given Decoder[Double] = instance {
    case d: java.lang.Double => Right(d.doubleValue)
    case l: java.lang.Long   => Right(l.doubleValue)
    case other               => Left(ConfigError.TypeMismatch("Double", other))
  }

  given Decoder[Float] = instance {
    case d: java.lang.Double => Right(d.floatValue)
    case l: java.lang.Long   => Right(l.floatValue)
    case other               => Left(ConfigError.TypeMismatch("Float", other))
  }

  // ---------- Higher-kinded instances ----------

  given [A](using d: Decoder[A]): Decoder[Option[A]] = instance {
    case null | _: PNull => Right(None)
    case other           => d.decode(other).map(Some(_))
  }

  given [A](using d: Decoder[A]): Decoder[Seq[A]] = instance {
    case xs: java.util.Collection[?] => decodeCollection(xs, d).map(_.toSeq)
    case other                       => Left(ConfigError.TypeMismatch("Listing/List", other))
  }

  given [A](using d: Decoder[A]): Decoder[List[A]] = instance {
    case xs: java.util.Collection[?] => decodeCollection(xs, d).map(_.toList)
    case other                       => Left(ConfigError.TypeMismatch("Listing/List", other))
  }

  given [A](using d: Decoder[A]): Decoder[Vector[A]] = instance {
    case xs: java.util.Collection[?] => decodeCollection(xs, d).map(_.toVector)
    case other                       => Left(ConfigError.TypeMismatch("Listing/List", other))
  }

  given [A](using d: Decoder[A]): Decoder[Set[A]] = instance {
    case xs: java.util.Collection[?] => decodeCollection(xs, d).map(_.toSet)
    case other                       => Left(ConfigError.TypeMismatch("Listing/List/Set", other))
  }

  given [K, V](using kd: Decoder[K], vd: Decoder[V]): Decoder[Map[K, V]] = {
    instance {
      case m: java.util.Map[?, ?] =>
        import scala.jdk.CollectionConverters.*
        val results = m.asScala.iterator.map { (k, v) =>
          for {
            dk <- kd.decode(k)
            dv <- vd.decode(v)
          } yield (dk, dv)
        }.toList
        firstError(results).map(_.toMap)
      case other => Left(ConfigError.TypeMismatch("Mapping/Map", other))
    }
  }

  given [A, B](using da: Decoder[A], db: Decoder[B]): Decoder[(A, B)] = {
    instance {
      case p: org.pkl.core.Pair[?, ?] =>
        for {
          a <- da.decode(p.getFirst)
          b <- db.decode(p.getSecond)
        } yield (a, b)
      case other => Left(ConfigError.TypeMismatch("Pair", other))
    }
  }

  given [L, R](using ld: Decoder[L], rd: Decoder[R]): Decoder[Either[L, R]] = {
    instance { value =>
      ld.decode(value) match {
        case Right(l) => Right(Left(l))
        case Left(_)  => rd.decode(value).map(Right(_))
      }
    }
  }

  // ---------- Domain instances (porting the most common pkl-config-java conversions) ----------

  given Decoder[scala.concurrent.duration.Duration] = instance {
    case d: org.pkl.core.Duration =>
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

  given Decoder[scala.concurrent.duration.FiniteDuration] = instance { value =>
    summon[Decoder[scala.concurrent.duration.Duration]].decode(value).flatMap {
      case fd: scala.concurrent.duration.FiniteDuration => Right(fd)
      case _ => Left(ConfigError.Custom("Pkl Duration is not finite"))
    }
  }

  given Decoder[java.time.Instant] = instance {
    case s: String         => Right(java.time.Instant.parse(s))
    case l: java.lang.Long => Right(java.time.Instant.ofEpochMilli(l))
    case other             => Left(ConfigError.TypeMismatch("Instant", other))
  }

  given Decoder[scala.util.matching.Regex] = instance {
    case s: String                  => Right(s.r)
    case p: java.util.regex.Pattern => Right(p.pattern.r)
    case other                      => Left(ConfigError.TypeMismatch("Regex/String", other))
  }

  // ---------- Mirror-driven product (case class) derivation ----------

  inline given derived[T <: Product](using m: Mirror.ProductOf[T]): Decoder[T] = {
    val labels = summonLabels[m.MirroredElemLabels]
    val decoders = summonDecoders[m.MirroredElemTypes]
    instance {
      case obj: PObject =>
        val results: List[Either[ConfigError, Any]] = labels.zip(decoders).map { (label, dec) =>
          val raw = obj.getProperties.get(label)
          if (raw == null && !obj.getProperties.containsKey(label)) {
            Left(ConfigError.MissingProperty(label))
          } else {
            dec.decode(raw).left.map(_.at(label))
          }
        }
        firstError(results).map { values =>
          m.fromProduct(Tuple.fromArray(values.toArray))
        }
      case other => Left(ConfigError.TypeMismatch("PObject", other))
    }
  }

  inline private def summonLabels[Labels <: Tuple]: List[String] = {
    inline erasedValue[Labels] match {
      case _: EmptyTuple => Nil
      case _: (h *: t)   => constValue[h].asInstanceOf[String] :: summonLabels[t]
    }
  }

  inline private def summonDecoders[Types <: Tuple]: List[Decoder[Any]] = {
    inline erasedValue[Types] match {
      case _: EmptyTuple => Nil
      case _: (h *: t)   =>
        summonInline[Decoder[h]].asInstanceOf[Decoder[Any]] :: summonDecoders[t]
    }
  }

  // ---------- izumi Tag-based: union types and singleton literals ----------

  /**
   * Catch-all for types that Mirror cannot derive: Scala 3 unions (`A | B`) and singleton literals
   * (`"hello"`). Lower priority than Mirror-derived instances by virtue of being on the companion's
   * outer scope; the implicit-search rules pick Mirror first when applicable.
   */
  inline given fromTag[T](using tag: Tag[T]): Decoder[T] = {
    val ref = tag.tag.ref
    ref match {
      case u: UnionReference =>
        unionDecoder[T](u)
      case n: NameReference =>
        n.ref match {
          case lit: SymName.SymLiteral =>
            singletonDecoder[T](lit.name)
          case _ =>
            throw new IllegalStateException(
              s"No Decoder for type `${tag.tag}`. " +
                s"Provide a `given Decoder[${tag.tag}]` or `derives Decoder`."
            )
        }
      case _ =>
        throw new IllegalStateException(s"No Decoder for type `${tag.tag}`.")
    }
  }

  private def unionDecoder[T](u: UnionReference): Decoder[T] = instance { value =>
    // Build a per-member decoder via Tag-recursion. We can't reify a Tag for an arbitrary
    // AbstractReference at runtime, so we materialize a thin probe: try each branch by name.
    // For the spike, we delegate to izumi's branch list and expect each branch to be a
    // type that has its own Decoder visible in this companion's implicit scope.
    val errors = scala.collection.mutable.ListBuffer.empty[ConfigError]
    u.refs.iterator
      .flatMap { branch =>
        decodeForBranch(branch, value) match {
          case Right(v) => Iterator.single(v)
          case Left(e)  => errors += e; Iterator.empty
        }
      }
      .nextOption() match {
      case Some(v) => Right(v.asInstanceOf[T])
      case None    =>
        Left(
          ConfigError.Custom(
            s"Cannot decode `${describe(value)}` as any union branch: ${errors.map(_.getMessage).mkString("; ")}"
          )
        )
    }
  }

  /**
   * Decode a single union branch. Limited to scalar branches in the spike — full recursion into
   * arbitrary parametrized branches would require materializing typeclass instances at runtime from
   * a `LightTypeTagRef`, which is a separate design problem.
   */
  private def decodeForBranch(
      branch: LightTypeTagRef.AppliedReferenceExceptUnion,
      value: Any
  ): Either[ConfigError, Any] = branch match {
    case n: NameReference =>
      n.ref match {
        case lit: SymName.SymLiteral =>
          val (expected, _) = parseLiteral(lit.name)
          summon[Decoder[String]].decode(value).flatMap { decoded =>
            if (decoded == expected) Right(decoded)
            else {
              Left(ConfigError.Custom(s"Singleton mismatch: expected `$expected`, got `$decoded`"))
            }
          }
        case sym: SymName.NamedSymbol =>
          sym.name match {
            case "scala.Int"           => summon[Decoder[Int]].decode(value)
            case "scala.Long"          => summon[Decoder[Long]].decode(value)
            case "scala.Boolean"       => summon[Decoder[Boolean]].decode(value)
            case "scala.Double"        => summon[Decoder[Double]].decode(value)
            case "scala.Float"         => summon[Decoder[Float]].decode(value)
            case "java.lang.String"    => summon[Decoder[String]].decode(value)
            case "scala.Predef.String" => summon[Decoder[String]].decode(value)
            case other                 =>
              Left(ConfigError.Custom(s"Union branch `$other` not supported in spike"))
          }
        case _ => Left(ConfigError.Custom(s"Unsupported union branch: $branch"))
      }
    case _ => Left(ConfigError.Custom(s"Unsupported union branch: $branch"))
  }

  private def singletonDecoder[T](rendered: String): Decoder[T] = instance { value =>
    val (expected, _) = parseLiteral(rendered)
    value match {
      case s: String if s == expected            => Right(s.asInstanceOf[T])
      case b: java.lang.Boolean if b == expected => Right(b.asInstanceOf[T])
      case l: java.lang.Long if l == expected    => Right(l.asInstanceOf[T])
      case other => Left(ConfigError.Custom(s"Expected singleton `$expected`, got `$other`"))
    }
  }

  private def parseLiteral(rendered: String): (Any, String) = {
    if (rendered.length >= 2 && rendered.startsWith("\"") && rendered.endsWith("\"")) {
      (rendered.substring(1, rendered.length - 1), "String")
    } else if (rendered == "true" || rendered == "false") {
      (java.lang.Boolean.valueOf(rendered), "Boolean")
    } else {
      try (java.lang.Long.valueOf(rendered), "Long")
      catch case _: NumberFormatException => (rendered, "String")
    }
  }

  // ---------- helpers ----------

  private def decodeCollection[A](
      xs: java.util.Collection[?],
      d: Decoder[A]
  ): Either[ConfigError, IndexedSeq[A]] = {
    import scala.jdk.CollectionConverters.*
    val results = xs.asScala.iterator.map(d.decode).toList
    firstError(results).map(_.toIndexedSeq)
  }

  /** First-error-wins; returns the list of decoded values if all succeeded. */
  private def firstError[A](results: Seq[Either[ConfigError, A]]): Either[ConfigError, List[A]] = {
    val out = List.newBuilder[A]
    val it = results.iterator
    while (it.hasNext) {
      it.next() match {
        case Right(v) => out += v
        case Left(e)  => return Left(e)
      }
    }
    Right(out.result())
  }

  private def describe(v: Any): String = v match {
    case null      => "null"
    case s: String => s""""$s""""
    case _         => s"${v.getClass.getSimpleName}($v)"
  }
}
