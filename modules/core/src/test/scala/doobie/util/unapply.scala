// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie
package util

import org.specs2.mutable.Specification

import cats.Monad
import cats.free.{ Free, Coyoneda }

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
object unapplyspec extends Specification {

  "Partial Unification" should {

    "allow inference of Monad[Free[Coyoneda[F, ?], ?]]" in {
      trait Foo[A]
      Monad[Free[Coyoneda[Foo, ?], ?]]
      true
    }

  }

}
