// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package org.typelevel.doobie.util

import cats.Applicative
import cats.implicits.*
import cats.effect.IO
import cats.kernel.Monoid
import org.typelevel.doobie.*
import org.typelevel.doobie.implicits.*

class ConnectionIOSuite extends munit.CatsEffectSuite {

  val xa = Transactor.fromDriverManager[IO](
    driver = "org.h2.Driver",
    url = "jdbc:h2:mem:queryspec;DB_CLOSE_DELAY=-1",
    user = "sa",
    password = "",
    logHandler = None
  )

  test("Semigroup ConnectionIO") {
    val prg = Applicative[ConnectionIO].pure(List(1, 2, 3)) `combine` Applicative[ConnectionIO].pure(List(4, 5, 6))
    prg.transact(xa).assertEquals(List(1, 2, 3, 4, 5, 6))
  }

  test("Semigroup ConnectionIO raw") {
    val prg = Applicative[ConnectionIO].pure(List(1, 2, 3)) `combine` Applicative[ConnectionIO].pure(List(4, 5, 6))
    prg.transactRaw(xa).assertEquals(List(1, 2, 3, 4, 5, 6))
  }

  test("Monoid ConnectionIO") {
    Monoid[ConnectionIO[List[Int]]].empty.transact(xa).assertEquals(Nil)
  }

  test("Monoid ConnectionIO raw") {
    Monoid[ConnectionIO[List[Int]]].empty.transactRaw(xa).assertEquals(Nil)
  }

}
