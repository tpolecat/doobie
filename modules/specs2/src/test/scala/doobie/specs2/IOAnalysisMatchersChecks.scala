// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.specs2

import cats.effect.IO
import org.specs2.mutable.Specification

import doobie.Transactor

class IOAnalysisMatchersChecks extends Specification with IOAnalysisMatchers {

  lazy val transactor = Transactor.fromDriverManager[IO](
    driver = "org.h2.Driver",
    url = "jdbc:h2:mem:queryspec;DB_CLOSE_DELAY=-1",
    user = "sa",
    password = "",
    logHandler = None
  )

  "IOAnalysisMatchers should not crash during initialization" >> ok
}
