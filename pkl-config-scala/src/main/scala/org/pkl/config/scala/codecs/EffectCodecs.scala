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

import org.pkl.config.scala.{Codec, ConfigError, Decoder}
import org.pkl.core.PNull

/**
 * Codec for `Option[A]`: encodes `None` as `null` and `Some(a)` via the underlying `Codec[A]`.
 * Decode also accepts Pkl's `PNull` sentinel. Mixed into the `Codec` companion at top priority.
 *
 * `Either` does NOT live here — see [[EffectDecoders]] in the same file for why.
 */
private[scala] trait EffectCodecs {

  given [A](using c: Codec[A]): Codec[Option[A]] = Codec.instance(
    enc = {
      case Some(a) => c.encode(a)
      case None    => null
    },
    dec = {
      case null | _: PNull => Right(None)
      case other           => c.decode(other).map(Some(_))
    }
  )
}

/**
 * `Either[L, R]` is decoder-only and lives in the `Decoder` companion (top decoder tier) so it
 * wins over the auto-derived `Codec[Either[L, R]]` reachable via `LowPriorityCodecs.derived` —
 * Either is a Scala sealed sum (so Mirror.SumOf would derive a class-name-dispatched Codec for
 * it), but we want union-style source dispatch (try `L`, fall back to `R`) instead.
 *
 * Encoding has no canonical inverse since the source union dropped the tag; that's why this is
 * a `Decoder` rather than a `Codec`.
 *
 * Mixed into `Codec.Decoder` rightmost in the parents list so its given out-prioritizes
 * `LowPriorityDecoders.fromCodec[Either[L, R]]`.
 */
private[scala] trait EffectDecoders {

  given decoderForEither[L, R](using ld: Decoder[L], rd: Decoder[R]): Decoder[Either[L, R]] = {
    Decoder.instance { value =>
      ld.decode(value) match {
        case Right(l) => Right(Left(l))
        case Left(_)  => rd.decode(value).map(Right(_))
      }
    }
  }
}
