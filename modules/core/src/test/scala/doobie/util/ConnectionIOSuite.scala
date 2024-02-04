// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.Applicative
import cats.implicits._
import cats.effect.IO
import cats.kernel.Monoid
import doobie._
import doobie.implicits._

class ConnectionIOSuite extends munit.FunSuite {

  import cats.effect.unsafe.implicits.global

  val xa = Transactor.fromDriverManager[IO](
    driver = "org.h2.Driver",
    url = "jdbc:h2:mem:queryspec;DB_CLOSE_DELAY=-1",
    user = "sa",
    password = "",
    logHandler = None
  )

  test("Semigroup ConnectionIO") {
    val prg = Applicative[ConnectionIO].pure(List(1, 2, 3)) combine Applicative[ConnectionIO].pure(List(4, 5, 6))
    assertEquals(prg.transact(xa).unsafeRunSync(), List(1, 2, 3, 4, 5, 6))
  }

  test("Monoid ConnectionIO") {
    assertEquals(Monoid[ConnectionIO[List[Int]]].empty.transact(xa).unsafeRunSync(), Nil)
  }

}
