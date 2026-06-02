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

/**
 * Errors raised when a Pkl runtime value cannot be decoded as the requested Scala type.
 *
 * Carries an optional path (case-class field chain like `address.zip`) so the failure can be
 * located in the original Pkl source.
 */
sealed trait ConfigError extends RuntimeException {
  def path: List[String]
  def message: String
  override def getMessage: String = {
    val where = if (path.isEmpty) "" else s" at `${path.mkString(".")}`"
    s"$message$where"
  }
  def at(segment: String): ConfigError
}

object ConfigError {

  final case class TypeMismatch(
      expected: String,
      actual: Any,
      path: List[String] = Nil
  ) extends ConfigError {
    val message: String = s"Expected `$expected` but got `${describe(actual)}`"
    def at(segment: String): TypeMismatch = copy(path = segment :: path)
  }

  final case class MissingProperty(name: String, path: List[String] = Nil) extends ConfigError {
    val message: String = s"Required property `$name` is missing"
    def at(segment: String): MissingProperty = copy(path = segment :: path)
  }

  final case class Custom(message: String, path: List[String] = Nil) extends ConfigError {
    def at(segment: String): Custom = copy(path = segment :: path)
  }

  final case class Aggregate(errors: List[ConfigError]) extends ConfigError {
    val path: List[String] = Nil
    val message: String = errors.map(_.getMessage).mkString("; ")
    def at(segment: String): Aggregate = Aggregate(errors.map(_.at(segment)))
  }

  private def describe(v: Any): String = v match {
    case null      => "null"
    case s: String => s""""$s""""
    case _         => s"${v.getClass.getSimpleName}($v)"
  }
}
