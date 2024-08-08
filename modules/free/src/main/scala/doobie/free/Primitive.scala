// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.free

import cats.data.Kleisli
import cats.effect.Async
import doobie.WeakAsync

sealed trait Primitive[M[_]] {
  def primitive[J, A](f: J => A): Kleisli[M, J, A]
}

object Primitive {
  case class Default[M[_]]()(implicit asyncM: WeakAsync[M]) extends Primitive[M] {

    override def primitive[J, A](f: J => A): Kleisli[M, J, A] = Kleisli { a =>
      // primitive JDBC methods throw exceptions and so do we when reading values
      // so catch any non-fatal exceptions and lift them into the effect
      try {
        asyncM.blocking(f(a))
      } catch {
        case scala.util.control.NonFatal(e) => asyncM.raiseError(e)
      }
    }
  }

  case class Cancellable[M[_]]()(implicit asyncM: Async[M]) extends Primitive[M] {

    override def primitive[J, A](f: J => A): Kleisli[M, J, A] = Kleisli { a =>
      import cats.syntax.all._
      import cats.effect.syntax.all._

      asyncM.uncancelable { unmask =>
        for {
          jdbcBlockedFiber <- asyncM.blocking(f(a)).attempt.start
          a <- unmask(jdbcBlockedFiber.join).flatMap(_.embedNever.rethrow)
        } yield a
      }
    }
  }
}
