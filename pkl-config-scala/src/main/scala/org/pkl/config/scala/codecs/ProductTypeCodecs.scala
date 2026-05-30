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
import org.pkl.core.{PClassInfo, PObject}

import scala.collection.mutable
import scala.deriving.Mirror
import scala.jdk.CollectionConverters.*

/**
 * `Mirror.ProductOf`-driven derivation: case classes (and case-object branches of sealed sums,
 * which are zero-field products). Encode produces `PObject(PClassInfo.Dynamic, ...)`; decode is
 * field-by-field from a `PObject`'s properties, with first-error-wins via `sequence`.
 *
 * Mixed into `LowPriorityCodecs` so the inline `Codec.derived` can dispatch into `derivedProduct`
 * at its `Mirror.ProductOf` branch.
 */
private[scala] trait ProductTypeCodecs extends CodecHelpers {

  inline def derivedProduct[T](m: Mirror.ProductOf[T]): Codec[T] =
    productCodec[T](summonLabels[m.MirroredElemLabels], summonCodecs[m.MirroredElemTypes], m)

  /**
   * Non-inline assembly for Mirror-derived product codecs. Keeping this out-of-line means the
   * anonymous class is defined once, not duplicated at every `derives` call site.
   *
   * `T` is `Mirror.ProductOf.MirroredMonoType` which by the Mirror contract `<: Product`; the cast
   * inside `encode` reflects that contract — required because we can't bound `T` at the (inline)
   * entry point without forcing every caller to satisfy it.
   */
  def productCodec[T](
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
     * `JsonRenderer` / etc. render as a Pkl object.
     */
    def encode(value: T): Any = {
      val product = value.asInstanceOf[Product]
      val props = mutable.LinkedHashMap.empty[String, Any]
      labels.iterator.zip(codecs.iterator).zipWithIndex.foreach { case ((label, c), i) =>
        props.update(label, c.encode(product.productElement(i)))
      }
      // PObject's Java constructor takes `Map<String, Object>`. The cast bridges Scala `Any` and
      // Java `Object` — same erasure at runtime.
      new PObject(PClassInfo.Dynamic, props.asJava.asInstanceOf[java.util.Map[String, Object]])
    }
  }
}
