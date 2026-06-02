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
package org.pkl.config.scala.codecs

import org.pkl.config.scala.{ConfigError, Decoder, Encoder}
import org.pkl.core.Pair as PPair

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
 * Per-direction givens for Scala collections (`Seq`, `List`, `Vector`, `Set`, `Map`) and
 * `Tuple2` ↔ Pkl `Pair`, split into [[CollectionEncoders]] (mixed into the `Encoder` companion) and
 * [[CollectionDecoders]] (mixed into the `Decoder` companion).
 *
 * Splitting is what lets a collection of a one-directional element resolve: `Decoder[Seq[A | B]]`,
 * `Decoder[List[Either[L, R]]]`, `Decoder[Map[K, V | W]]` etc. need only a `Decoder` per element,
 * never a full round-trippable `Codec` — so union / `Either` elements (which ship a `Decoder` but no
 * `Codec`) work. The bundled `Codec[Seq[A]]` form required `Codec[A]` and could not express that.
 *
 * The encode helpers (`encodeListing`, `encodeMapping`) materialize `mutable.ArrayBuffer` /
 * `mutable.LinkedHashMap` and hand back their `.asJava` views — Pkl's renderers dispatch on
 * `java.util.List` / `java.util.Map` instances. `decodeCollection` is the matching inverse for
 * `java.util.Collection` decode payloads.
 */
private[scala] trait CollectionEncoders {

  given [A](using e: Encoder[A]): Encoder[Seq[A]] =
    Encoder.instance(xs => encodeListing(xs, e))

  given [A](using e: Encoder[A]): Encoder[List[A]] =
    Encoder.instance(xs => encodeListing(xs, e))

  given [A](using e: Encoder[A]): Encoder[Vector[A]] =
    Encoder.instance(xs => encodeListing(xs, e))

  given [A](using e: Encoder[A]): Encoder[Set[A]] =
    Encoder.instance(xs => encodeListing(xs, e))

  given [K, V](using ke: Encoder[K], ve: Encoder[V]): Encoder[Map[K, V]] =
    Encoder.instance(m => encodeMapping(m, ke, ve))

  given [A, B](using ea: Encoder[A], eb: Encoder[B]): Encoder[(A, B)] =
    Encoder.instance(t => new PPair(ea.encode(t._1), eb.encode(t._2)))

  /**
   * Pkl Listings/Lists are rendered via `instanceof java.util.List`, so the encoded form must be a
   * Java List. Build with a Scala `ArrayBuffer` (O(1) append, ordered) and convert at the boundary.
   */
  def encodeListing[A](xs: Iterable[A], e: Encoder[A]): java.util.List[Any] = {
    val buf = mutable.ArrayBuffer.empty[Any]
    xs.foreach(x => buf += e.encode(x))
    buf.asJava
  }

  /** Pkl Mapping is a `LinkedHashMap` (insertion-ordered); use the Scala equivalent and convert. */
  def encodeMapping[K, V](
      m: Map[K, V],
      ke: Encoder[K],
      ve: Encoder[V]
  ): java.util.Map[Any, Any] = {
    val out = mutable.LinkedHashMap.empty[Any, Any]
    m.foreach { (k, v) => out.update(ke.encode(k), ve.encode(v)) }
    out.asJava
  }
}

private[scala] trait CollectionDecoders extends CodecHelpers {

  given [A](using d: Decoder[A]): Decoder[Seq[A]] = Decoder.instance {
    case xs: java.util.Collection[?] => decodeCollection(xs, d).map(_.toSeq)
    case other                       => Left(ConfigError.TypeMismatch("Listing/List", other))
  }

  given [A](using d: Decoder[A]): Decoder[List[A]] = Decoder.instance {
    case xs: java.util.Collection[?] => decodeCollection(xs, d).map(_.toList)
    case other                       => Left(ConfigError.TypeMismatch("Listing/List", other))
  }

  given [A](using d: Decoder[A]): Decoder[Vector[A]] = Decoder.instance {
    case xs: java.util.Collection[?] => decodeCollection(xs, d).map(_.toVector)
    case other                       => Left(ConfigError.TypeMismatch("Listing/List", other))
  }

  given [A](using d: Decoder[A]): Decoder[Set[A]] = Decoder.instance {
    case xs: java.util.Collection[?] => decodeCollection(xs, d).map(_.toSet)
    case other                       => Left(ConfigError.TypeMismatch("Listing/List/Set", other))
  }

  given [K, V](using kd: Decoder[K], vd: Decoder[V]): Decoder[Map[K, V]] = Decoder.instance {
    case m: java.util.Map[?, ?] =>
      val results = m.asScala.iterator.map { (k, v) =>
        for {
          dk <- kd.decode(k)
          dv <- vd.decode(v)
        } yield (dk, dv)
      }.toList
      sequence(results).map(_.toMap)
    case other => Left(ConfigError.TypeMismatch("Mapping/Map", other))
  }

  given [A, B](using ad: Decoder[A], bd: Decoder[B]): Decoder[(A, B)] = Decoder.instance {
    case p: PPair[?, ?] =>
      for {
        a <- ad.decode(p.getFirst)
        b <- bd.decode(p.getSecond)
      } yield (a, b)
    case other => Left(ConfigError.TypeMismatch("Pair", other))
  }

  def decodeCollection[A](
      xs: java.util.Collection[?],
      d: Decoder[A]
  ): Either[ConfigError, IndexedSeq[A]] = {
    val results = xs.asScala.iterator.map(d.decode).toList
    sequence(results).map(_.toIndexedSeq)
  }
}
