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

import scala.compiletime.{constValue, erasedValue, summonInline}

/**
 * Shared internals used by the derivation traits. `sequence` is the first-error-wins
 * `List[Either[E, A]] => Either[E, List[A]]` combinator that products, collections, and the Map
 * codec all need; `summonLabels` / `summonEncoders` / `summonDecoders` walk the `Mirror`-derived
 * `Tuple` types into runtime lists used by the product / sum derivation paths.
 *
 * In the bottom-up design encode and decode are derived independently (each summoning its own half),
 * so there is a separate summoner per direction — no `summonCodecs`. A field's `Encoder`/`Decoder`
 * resolves either to a concrete `given Codec[F]` (which IS both, via `Codec <: Encoder, Decoder`) or
 * to the recursive `derivedEncoder`/`derivedDecoder` given.
 *
 * Members are public so inline expansion of `derived` at user call sites can still reach them
 * through the `Encoder`/`Decoder` objects' inheritance chain. The trait itself is `private[scala]`
 * to keep the surface internal.
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

  inline def summonEncoders[Types <: Tuple]: List[Encoder[Any]] = {
    inline erasedValue[Types] match {
      case _: EmptyTuple => Nil
      case _: (h *: t)   =>
        summonInline[Encoder[h]].asInstanceOf[Encoder[Any]] :: summonEncoders[t]
    }
  }

  inline def summonDecoders[Types <: Tuple]: List[Decoder[Any]] = {
    inline erasedValue[Types] match {
      case _: EmptyTuple => Nil
      case _: (h *: t)   =>
        summonInline[Decoder[h]].asInstanceOf[Decoder[Any]] :: summonDecoders[t]
    }
  }
}
