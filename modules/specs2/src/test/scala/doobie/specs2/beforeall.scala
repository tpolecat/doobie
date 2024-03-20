// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.specs2

import cats.effect.IO
import doobie.syntax.connectionio._
import doobie.syntax.string._
import doobie.util.transactor.Transactor
import org.specs2.mutable.Specification
import org.specs2.specification.BeforeAll

// Check that AnalysisSpec plays nice with Specs2 execution flow (issue #454)

class beforeall extends Specification with IOChecker with BeforeAll {

  import cats.effect.unsafe.implicits.global

  // Setup
  val initQ = sql"create table some_table (value varchar not null)".update

  val targetQ = sql"select value from some_table".query[String]

  val transactor = Transactor.fromDriverManager[IO](
    driver = "org.h2.Driver",
    url = "jdbc:h2:mem:beforeall;DB_CLOSE_DELAY=-1",
    user = "sa",
    password = "",
    logHandler = None
  )

  // The test itself
  check(targetQ)

  // A hook for database initialization
  def beforeAll() = {
    initQ.run
      .transact(transactor)
      .unsafeRunSync()
    ()
  }
}
