// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.scalatest

import cats.effect.IO
import doobie.syntax.string._
import doobie.util.transactor.Transactor
import org.scalatest._

trait CheckerChecks[M[_]] extends funsuite.AnyFunSuite with matchers.should.Matchers with Checker[M] {

  lazy val transactor = Transactor.fromDriverManager[M](
    driver = "org.h2.Driver",
    url = "jdbc:h2:mem:queryspec;DB_CLOSE_DELAY=-1",
    user = "sa",
    password = "",
    logHandler = None
  )

  test("trivial") { check(sql"select 1".query[Int]) }

}

class IOCheckerCheck extends CheckerChecks[IO] with IOChecker
