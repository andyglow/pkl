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

import scala.deriving.Mirror
import scala.jdk.CollectionConverters.*

/**
 * `Mirror.SumOf`-driven derivation for sealed traits and Scala 3 enums, split into independent
 * encode and decode halves (the bottom-up design).
 *
 *   - Encode (`sumEncoder`): `m.ordinal(value)` picks the branch, dispatches to that branch's
 *     `Encoder`, then re-tags the resulting `PObject` with a synthesized `PClassInfo` carrying the
 *     branch's name.
 *   - Decode (`sumDecoder`): dispatches by class name (primary) or `_type` discriminator (fallback).
 *
 * Case-object branches go through the `Mirror.ProductOf` (singleton extends product) path, so their
 * encode/decode is an empty PObject — the branch identity lives in the surrounding `PClassInfo`.
 * A future step may special-case all-singleton sums to encode as a String of the branch name (more
 * idiomatic for pure enums); deferred because the compile-time singleton check via
 * `inline if + summonFrom` doesn't reduce to a literal Boolean in the current Scala 3.
 *
 * Mixed into `LowPriorityEncoders` / `LowPriorityDecoders` so the inline `deriveEncoder` /
 * `deriveDecoder` dispatchers can reach the `Mirror.SumOf` branch.
 */
private[scala] trait SumTypeCodecs extends CodecHelpers {

  /** Discriminator field name for sum decode fallback. */
  val DiscriminatorField = "_type"

  inline def derivedSumEncoder[T](m: Mirror.SumOf[T]): Encoder[T] =
    sumEncoder[T](summonLabels[m.MirroredElemLabels], summonEncoders[m.MirroredElemTypes], m)

  inline def derivedSumDecoder[T](m: Mirror.SumOf[T]): Decoder[T] =
    sumDecoder[T](summonLabels[m.MirroredElemLabels], summonDecoders[m.MirroredElemTypes])

  def sumEncoder[T](
      branchLabels: List[String],
      branchEncoders: List[Encoder[Any]],
      m: Mirror.SumOf[T]
  ): Encoder[T] = Encoder.instance { value =>
    val i = m.ordinal(value)
    val branchName = branchLabels(i)
    branchEncoders(i).encode(value) match {
      case obj: PObject =>
        new PObject(brandedClassInfo(branchName), obj.getProperties)
      case other => other
    }
  }

  def sumDecoder[T](
      branchLabels: List[String],
      branchDecoders: List[Decoder[Any]]
  ): Decoder[T] = Decoder.instance {
    case obj: PObject =>
      val byClassName = branchLabels.indexOf(obj.getClassInfo.getSimpleName)
      val branchIndex =
        if (byClassName >= 0) Right(byClassName)
        else
          obj.getProperties.asScala.get(DiscriminatorField) match {
            case Some(s: String) =>
              branchLabels.indexOf(s) match {
                case -1 =>
                  Left(
                    ConfigError.Custom(
                      s"Unknown sum branch `$s`. Expected one of: ${branchLabels.mkString(", ")}"
                    )
                  )
                case i => Right(i)
              }
            case _ =>
              Left(
                ConfigError.Custom(
                  s"Cannot determine sum branch from class `${obj.getClassInfo.getDisplayName}` " +
                    s"and no `$DiscriminatorField` discriminator was found. " +
                    s"Expected one of: ${branchLabels.mkString(", ")}"
                )
              )
          }
      branchIndex.flatMap(i => branchDecoders(i).decode(obj).map(_.asInstanceOf[T]))
    case other =>
      Left(ConfigError.TypeMismatch("PObject (sealed sum)", other))
  }

  /**
   * Synthesize a `PClassInfo` carrying the branch's name. We don't have a real Pkl module to
   * attach it to, so we pick a synthetic moduleName/URI; the value is only consumed by our own
   * sum-decoder dispatch, which only reads `getSimpleName`.
   */
  def brandedClassInfo(branchName: String): PClassInfo[?] =
    PClassInfo.get("$scala.sum", branchName, java.net.URI.create("scala:sum"))
}
