// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package example

import cats._
import cats.syntax.all._
import doobie._
import doobie.implicits._

/** Example interpreter that fails if it encounters any constructors. This lets you run `ConnectionIO` programs that
  * rely only on pure values, for example.
  */
object Noop {

  def noop[S[_]]: S ~> Either[String, *] =
    new (S ~> Either[String, *]) {
      def apply[A](c: S[A]) = Left(s"Can't interpret $c")
    }

  val prog1: ConnectionIO[Int] =
    for {
      a <- 42.pure[ConnectionIO]
      b <- 3.pure[ConnectionIO]
    } yield a + b

  val prog2: ConnectionIO[Int] =
    sql"select 42".query[Int].unique

  def main(args: Array[String]): Unit = {
    println(prog1.foldMap(noop))
    println(prog2.foldMap(noop))
  }

}
