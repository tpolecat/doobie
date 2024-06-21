// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.specs2

import cats.Id
import cats.effect.IO
import doobie.syntax.string._
import doobie.util.transactor.Transactor
import org.specs2.mutable.Specification

trait CheckerChecks[M[_]] extends Specification with Checker[M] {

  lazy val transactor = Transactor.fromDriverManager[M](
    driver = "org.h2.Driver",
    url = "jdbc:h2:mem:queryspec;DB_CLOSE_DELAY=-1",
    user = "sa",
    password = "",
    logHandler = None
  )

  check(sql"select 1".query[Int])

  // Abstract type parameters should be handled correctly
  {
    import doobie.generic.auto._

    final case class Foo[F[_]](x: Int)
    check(sql"select 1".query[Foo[Id]])
  }
}

class IOCheckerCheck extends CheckerChecks[IO] with IOChecker
