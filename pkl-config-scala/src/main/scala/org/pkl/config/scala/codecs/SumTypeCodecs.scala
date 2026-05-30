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

import scala.deriving.Mirror
import scala.jdk.CollectionConverters.*

/**
 * `Mirror.SumOf`-driven derivation: sealed traits and Scala 3 enums. Encode picks the branch via
 * `m.ordinal`, dispatches to the branch's Codec, then re-tags the resulting `PObject` with a
 * synthesized `PClassInfo` carrying the branch's name. Decode dispatches by class name (primary)
 * or `_type` discriminator (fallback).
 *
 * Case-object branches go through the `Mirror.ProductOf` (singleton extends product) path, so
 * their Codec encodes/decodes an empty PObject — the branch identity lives in the surrounding
 * `PClassInfo`. A future step may special-case all-singleton sums to encode as a String of the
 * branch name (more idiomatic for pure enums); deferred because the compile-time singleton check
 * via `inline if + summonFrom` doesn't reduce to a literal Boolean in the current Scala 3.
 *
 * Mixed into `LowPriorityCodecs` so the inline `Codec.derived` can dispatch into `derivedSum`
 * at its `Mirror.SumOf` branch.
 */
private[scala] trait SumTypeCodecs extends CodecHelpers {

  /** Discriminator field name for sum decode fallback. */
  val DiscriminatorField = "_type"

  inline def derivedSum[T](m: Mirror.SumOf[T]): Codec[T] =
    sumCodec[T](
      summonLabels[m.MirroredElemLabels],
      summonCodecs[m.MirroredElemTypes],
      m
    )

  def sumCodec[T](
      branchLabels: List[String],
      branchCodecs: List[Codec[Any]],
      m: Mirror.SumOf[T]
  ): Codec[T] = new Codec[T] {

    def decode(value: Any): Either[ConfigError, T] = value match {
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
        branchIndex.flatMap(i => branchCodecs(i).decode(obj).map(_.asInstanceOf[T]))
      case other =>
        Left(ConfigError.TypeMismatch("PObject (sealed sum)", other))
    }

    def encode(value: T): Any = {
      val i = m.ordinal(value)
      val branchName = branchLabels(i)
      branchCodecs(i).encode(value) match {
        case obj: PObject =>
          new PObject(brandedClassInfo(branchName), obj.getProperties)
        case other => other
      }
    }
  }

  /**
   * Synthesize a `PClassInfo` carrying the branch's name. We don't have a real Pkl module to
   * attach it to, so we pick a synthetic moduleName/URI; the value is only consumed by our own
   * sum-decoder dispatch, which only reads `getSimpleName`.
   */
  def brandedClassInfo(branchName: String): PClassInfo[?] =
    PClassInfo.get("$scala.sum", branchName, java.net.URI.create("scala:sum"))
}
