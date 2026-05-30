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

import scala.compiletime.{constValue, erasedValue, summonInline}

/**
 * Shared internals used by category-specific codec traits. `sequence` is the first-error-wins
 * `List[Either[E, A]] => Either[E, List[A]]` combinator that products, collections, and the Map
 * codec all need; `summonLabels` / `summonCodecs` walk the `Mirror`-derived `Tuple` types into
 * runtime lists used by the product / sum derivation paths.
 *
 * Members are public so inline expansion of `Codec.derived` at user call sites can still reach
 * them through the `Codec` object's inheritance chain. The trait itself is `private[scala]` to
 * keep the surface internal.
 */
private[scala] trait CodecHelpers {

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
}
