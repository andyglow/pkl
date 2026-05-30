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
import org.pkl.core.{DurationUnit, Duration as PDuration}

/**
 * Scalar / leaf-type codecs: numeric and string primitives plus single-value domain types
 * (`Duration`, `FiniteDuration`, `Instant`, `Regex`). Mixed into the `Codec` companion at
 * top priority — these win over `LowPriorityCodecs.derived`.
 */
private[scala] trait PrimitiveCodecs {

  given Codec[String] = Codec.named(
    "String",
    enc = (s: String) => s,
    dec = { case s: String => Right(s) }
  )

  given Codec[Boolean] = Codec.named(
    "Boolean",
    enc = (b: Boolean) => java.lang.Boolean.valueOf(b),
    dec = { case b: java.lang.Boolean => Right(b.booleanValue) }
  )

  given Codec[Long] = Codec.named(
    "Long",
    enc = (l: Long) => java.lang.Long.valueOf(l),
    dec = { case l: java.lang.Long => Right(l.longValue) }
  )

  given Codec[Int] = Codec.named(
    "Int",
    enc = (i: Int) => java.lang.Long.valueOf(i.toLong),
    dec = {
      case l: java.lang.Long if l.longValue.isValidInt => Right(l.intValue)
      case l: java.lang.Long                           =>
        Left(ConfigError.Custom(s"Pkl Int $l does not fit in Scala Int (32-bit)."))
    }
  )

  given Codec[Double] = Codec.named(
    "Double",
    enc = (d: Double) => java.lang.Double.valueOf(d),
    dec = {
      case d: java.lang.Double => Right(d.doubleValue)
      case l: java.lang.Long   => Right(l.doubleValue)
    }
  )

  given Codec[Float] = Codec.named(
    "Float",
    enc = (f: Float) => java.lang.Double.valueOf(f.toDouble),
    dec = {
      case d: java.lang.Double => Right(d.floatValue)
      case l: java.lang.Long   => Right(l.floatValue)
    }
  )

  given Codec[scala.concurrent.duration.Duration] = Codec.named(
    "Duration",
    enc = {
      case fd: scala.concurrent.duration.FiniteDuration => scalaDurationToPkl(fd)
      case _                                            =>
        // Pkl has no infinite duration; lossy. Real fix: refuse at compile time via a `FiniteDuration` codec.
        null
    },
    dec = {
      case d: PDuration =>
        val v = d.getValue
        val unit = d.getUnit.toTimeUnit
        val l = v.toLong
        Right(
          if (v.isFinite && l.toDouble == v) {
            scala.concurrent.duration.FiniteDuration(l, unit)
          } else {
            scala.concurrent.duration
              .FiniteDuration(d.inWholeNanos(), java.util.concurrent.TimeUnit.NANOSECONDS)
              .toCoarsest
          }
        )
    }
  )

  given Codec[scala.concurrent.duration.FiniteDuration] = {
    val parent = summon[Codec[scala.concurrent.duration.Duration]]
    Codec.instance(
      enc = (fd: scala.concurrent.duration.FiniteDuration) => parent.encode(fd),
      dec = (v: Any) => {
        parent.decode(v).flatMap {
          case fd: scala.concurrent.duration.FiniteDuration => Right(fd)
          case _ => Left(ConfigError.Custom("Pkl Duration is not finite"))
        }
      }
    )
  }

  given Codec[java.time.Instant] = Codec.named(
    "Instant",
    enc = (i: java.time.Instant) => i.toString, // ISO-8601
    dec = {
      case s: String         => Right(java.time.Instant.parse(s))
      case l: java.lang.Long => Right(java.time.Instant.ofEpochMilli(l))
    }
  )

  given Codec[scala.util.matching.Regex] = Codec.named(
    "Regex/String",
    enc = (r: scala.util.matching.Regex) => java.util.regex.Pattern.compile(r.pattern.pattern),
    dec = {
      case s: String                  => Right(s.r)
      case p: java.util.regex.Pattern => Right(p.pattern.r)
    }
  )

  private def scalaDurationToPkl(fd: scala.concurrent.duration.FiniteDuration): PDuration = {
    import java.util.concurrent.TimeUnit
    val unit = fd.unit match {
      case TimeUnit.NANOSECONDS  => DurationUnit.NANOS
      case TimeUnit.MICROSECONDS => DurationUnit.MICROS
      case TimeUnit.MILLISECONDS => DurationUnit.MILLIS
      case TimeUnit.SECONDS      => DurationUnit.SECONDS
      case TimeUnit.MINUTES      => DurationUnit.MINUTES
      case TimeUnit.HOURS        => DurationUnit.HOURS
      case TimeUnit.DAYS         => DurationUnit.DAYS
    }
    new PDuration(fd.length.toDouble, unit)
  }
}
