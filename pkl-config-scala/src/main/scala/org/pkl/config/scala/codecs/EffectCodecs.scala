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

import org.pkl.config.scala.{Decoder, Encoder}
import org.pkl.core.PNull

/**
 * `Option` and `Either` decoders, split into the per-direction effect traits.
 *
 *   - `Option[A]` decodes `null` / Pkl's `PNull` sentinel to `None`, anything else via `Decoder[A]`.
 *   - `Either[L, R]` decodes by **source dispatch**: try `L`, fall back to `R`. Pkl unions carry no
 *     tag, so there is no class-name to match on — this is deliberately NOT the `Mirror.SumOf`-derived
 *     decoder (which would dispatch on a `Left`/`Right` class tag). Mixed into the `Decoder` companion
 *     rightmost (most specific by linearization) so it out-prioritizes the recursive `derivedDecoder`
 *     given that `Mirror.SumOf[Either]` would otherwise satisfy.
 */
private[scala] trait EffectDecoders {

  given [A](using d: Decoder[A]): Decoder[Option[A]] = Decoder.instance {
    case null | _: PNull => Right(None)
    case other           => d.decode(other).map(Some(_))
  }

  given decoderForEither[L, R](using ld: Decoder[L], rd: Decoder[R]): Decoder[Either[L, R]] = {
    Decoder.instance { value =>
      ld.decode(value) match {
        case Right(l) => Right(Left(l))
        case Left(_)  => rd.decode(value).map(Right(_))
      }
    }
  }
}

/**
 * `Option` and `Either` encoders, the matching encode halves of [[EffectDecoders]].
 *
 *   - `Option[A]` encodes `None` as `null` and `Some(a)` via `Encoder[A]`.
 *   - `Either[L, R]` encodes by dispatching on the Scala-side constructor tag: `Left(l)` emits the
 *     encoding of `l`, `Right(r)` emits the encoding of `r`. This is total and lossless on its own —
 *     the "dropped tag" only bites on decode (Pkl→Scala), never on encode — and matches the
 *     source-union semantics of [[EffectDecoders.decoderForEither]] (encode then decode round-trips
 *     whenever `L` and `R` are distinguishable Pkl shapes). Without it, encode would fall through to
 *     the `Mirror.SumOf`-derived sum encoder, whose `Left`/`Right`-tagged `PObject` would NOT round-trip
 *     through the union decoder. Mixed into the `Encoder` companion rightmost so it out-prioritizes
 *     `derivedEncoder`.
 */
private[scala] trait EffectEncoders {

  given [A](using e: Encoder[A]): Encoder[Option[A]] = Encoder.instance {
    case Some(a) => e.encode(a)
    case None    => null
  }

  given encoderForEither[L, R](using le: Encoder[L], re: Encoder[R]): Encoder[Either[L, R]] = {
    Encoder.instance {
      case Left(l)  => le.encode(l)
      case Right(r) => re.encode(r)
    }
  }
}
