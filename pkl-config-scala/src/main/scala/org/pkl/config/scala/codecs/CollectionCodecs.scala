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

import org.pkl.config.scala.{Codec, ConfigError}
import org.pkl.core.Pair as PPair

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
 * Codecs for Scala collections (`Seq`, `List`, `Vector`, `Set`, `Map`) and `Tuple2` ↔ Pkl `Pair`.
 * Mixed into the `Codec` companion at top priority.
 *
 * The encode helpers (`encodeListing`, `encodeMapping`) materialize `mutable.ArrayBuffer` /
 * `mutable.LinkedHashMap` and hand back their `.asJava` views — Pkl's renderers dispatch on
 * `java.util.List` / `java.util.Map` instances. `decodeCollection` is the matching inverse for
 * `java.util.Collection` decode payloads.
 */
private[scala] trait CollectionCodecs extends CodecHelpers {

  given [A](using c: Codec[A]): Codec[Seq[A]] = Codec.named(
    "Listing/List",
    enc = (xs: Seq[A]) => encodeListing(xs, c),
    dec = { case xs: java.util.Collection[?] => decodeCollection(xs, c).map(_.toSeq) }
  )

  given [A](using c: Codec[A]): Codec[List[A]] = Codec.named(
    "Listing/List",
    enc = (xs: List[A]) => encodeListing(xs, c),
    dec = { case xs: java.util.Collection[?] => decodeCollection(xs, c).map(_.toList) }
  )

  given [A](using c: Codec[A]): Codec[Vector[A]] = Codec.named(
    "Listing/List",
    enc = (xs: Vector[A]) => encodeListing(xs, c),
    dec = { case xs: java.util.Collection[?] => decodeCollection(xs, c).map(_.toVector) }
  )

  given [A](using c: Codec[A]): Codec[Set[A]] = Codec.named(
    "Listing/List/Set",
    enc = (xs: Set[A]) => encodeListing(xs, c),
    dec = { case xs: java.util.Collection[?] => decodeCollection(xs, c).map(_.toSet) }
  )

  given [K, V](using kc: Codec[K], vc: Codec[V]): Codec[Map[K, V]] = Codec.named(
    "Mapping/Map",
    enc = (m: Map[K, V]) => encodeMapping(m, kc, vc),
    dec = { case m: java.util.Map[?, ?] =>
      val results = m.asScala.iterator.map { (k, v) =>
        for {
          dk <- kc.decode(k)
          dv <- vc.decode(v)
        } yield (dk, dv)
      }.toList
      sequence(results).map(_.toMap)
    }
  )

  given [A, B](using ca: Codec[A], cb: Codec[B]): Codec[(A, B)] = Codec.named(
    "Pair",
    enc = (t: (A, B)) => new PPair(ca.encode(t._1), cb.encode(t._2)),
    dec = { case p: PPair[?, ?] =>
      for {
        a <- ca.decode(p.getFirst)
        b <- cb.decode(p.getSecond)
      } yield (a, b)
    }
  )

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
}
