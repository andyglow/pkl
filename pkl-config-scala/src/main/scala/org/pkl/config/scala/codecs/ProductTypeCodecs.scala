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
import org.pkl.core.{PClassInfo, PObject}

import scala.collection.mutable
import scala.deriving.Mirror
import scala.jdk.CollectionConverters.*

/**
 * `Mirror.ProductOf`-driven derivation for case classes (and case-object branches of sealed sums,
 * which are zero-field products). Encode and decode are derived independently (the bottom-up
 * design): `productEncoder` builds an `Encoder[T]`, `productDecoder` builds a `Decoder[T]`, each
 * summoning only its own half per field.
 *
 *   - Encode produces `PObject(PClassInfo.Dynamic, ...)`.
 *   - Decode is field-by-field from a `PObject`'s properties, first-error-wins via `sequence`.
 *
 * Mixed into `LowPriorityEncoders` / `LowPriorityDecoders` so the inline `deriveEncoder` /
 * `deriveDecoder` dispatchers can reach the `Mirror.ProductOf` branch.
 */
private[scala] trait ProductTypeCodecs extends CodecHelpers {

  inline def derivedProductEncoder[T](m: Mirror.ProductOf[T]): Encoder[T] =
    productEncoder[T](summonLabels[m.MirroredElemLabels], summonEncoders[m.MirroredElemTypes])

  inline def derivedProductDecoder[T](m: Mirror.ProductOf[T]): Decoder[T] =
    productDecoder[T](summonLabels[m.MirroredElemLabels], summonDecoders[m.MirroredElemTypes], m)

  /**
   * Non-inline assembly for the encode half. Keeping this out-of-line means the anonymous class is
   * defined once, not duplicated at every `derives` call site.
   *
   * `T` is `Mirror.ProductOf.MirroredMonoType` which by the Mirror contract `<: Product`; the cast
   * reflects that contract — required because we can't bound `T` at the inline entry point without
   * forcing every caller to satisfy it.
   */
  def productEncoder[T](labels: List[String], encoders: List[Encoder[Any]]): Encoder[T] =
    Encoder.instance { value =>
      val product = value.asInstanceOf[Product]
      val props = mutable.LinkedHashMap.empty[String, Any]
      labels.iterator.zip(encoders.iterator).zipWithIndex.foreach { case ((label, e), i) =>
        props.update(label, e.encode(product.productElement(i)))
      }
      // PObject's Java constructor takes `Map<String, Object>`. The cast bridges Scala `Any` and
      // Java `Object` — same erasure at runtime.
      new PObject(PClassInfo.Dynamic, props.asJava.asInstanceOf[java.util.Map[String, Object]])
    }

  /** Non-inline assembly for the decode half. */
  def productDecoder[T](
      labels: List[String],
      decoders: List[Decoder[Any]],
      m: Mirror.ProductOf[T]
  ): Decoder[T] = Decoder.instance {
    case obj: PObject =>
      val props = obj.getProperties.asScala
      val perField = labels.zip(decoders).map { (label, d) =>
        props.get(label) match {
          case None      => Left(ConfigError.MissingProperty(label))
          case Some(raw) => d.decode(raw).left.map(_.at(label))
        }
      }
      sequence(perField).map(values => m.fromProduct(Tuple.fromArray(values.toArray)))
    case other =>
      Left(ConfigError.TypeMismatch("PObject", other))
  }
}
