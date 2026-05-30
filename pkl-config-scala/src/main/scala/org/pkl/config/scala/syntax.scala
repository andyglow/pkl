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

import org.pkl.core.{PModule, PObject}

/**
 * Extension methods for the spike: lift any Pkl runtime value (typically a `PModule` after
 * evaluation, or any nested property) into a typed Scala value via an in-scope `Decoder[T]`.
 *
 * The shape mirrors circe / pureconfig: the user calls `value.to[T]`, the typeclass machinery
 * derives or summons a decoder, and the result is either the typed value or a thrown `ConfigError`
 * carrying a path.
 */
object syntax {

  extension (m: PModule) {
    inline def to[T](using d: Decoder[T]): T = {
      d.decode(m).fold(throw _, identity)
    }
  }

  extension (o: PObject) {
    inline def to[T](using d: Decoder[T]): T = {
      d.decode(o).fold(throw _, identity)
    }

    /**
     * Navigate into a child property and decode it. (`get` would clash with `Composite.get(name)`
     * which returns raw Object.)
     */
    inline def as[T](name: String)(using d: Decoder[T]): T = {
      d.decode(o.getProperty(name)).fold(throw _, identity)
    }
  }

  /** For ad-hoc decoding of a property value pulled out by name (typed Pkl primitives, etc.). */
  extension (raw: Any) {
    inline def decodeAs[T](using d: Decoder[T]): Either[ConfigError, T] = {
      d.decode(raw)
    }
  }
}
