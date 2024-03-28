// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.mysql

import cats.effect.IO
import doobie.Transactor

object MySQLTestTransactor {

  val xa = Transactor.fromDriverManager[IO](
    "com.mysql.cj.jdbc.Driver",
    // args from solution 2a https://docs.oracle.com/cd/E17952_01/connector-j-8.0-en/connector-j-time-instants.html
    "jdbc:mysql://localhost:3306/world?preserveInstants=true&connectionTimeZone=SERVER",
    "root", "password",
    logHandler = None
  )
}
