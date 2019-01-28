// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import doobie._, doobie.implicits._
import doobie.postgres.implicits._
import org.specs2.mutable.Specification

import cats.implicits._

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
object unapplyspec extends Specification {

  "Partial Unification" should {

    "allow use of sqlstate syntax" in {
      1.pure[ConnectionIO].map(_ + 1).void
      1.pure[ConnectionIO].map(_ + 1).onPrivilegeNotRevoked(2.pure[ConnectionIO])
      true
    }

  }

}
