// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.Functor

/** A wrapper class to implicit resolution prioritize explicitly created instances over automatically derived ones.
  */
final case class Derived[+Inst](instance: Inst) extends AnyVal

object Derived {
  implicit val f: Functor[Derived] = new Functor[Derived] {
    override def map[A, B](fa: Derived[A])(f: A => B): Derived[B] = Derived(f(fa.instance))
  }
}
