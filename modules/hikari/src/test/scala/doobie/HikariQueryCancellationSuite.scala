// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie

import cats.effect.*
import cats.effect.unsafe.implicits.global
import com.zaxxer.hikari.HikariConfig
import doobie.hikari.HikariTransactor
import doobie.implicits.*
import doobie.util.transactor

import scala.concurrent.duration.DurationInt

class HikariQueryCancellationSuite extends munit.FunSuite {

  // Typically you construct a transactor this way, using lifetime-managed thread pools.
  val transactorRes: Resource[IO, Transactor[IO]] =
    (for {
      hikariConfig <- Resource.pure {
        val config = new HikariConfig()
        config.setDriverClassName("org.postgresql.Driver")
        config.setJdbcUrl("jdbc:postgresql://localhost:5432/postgres")
        config.setUsername("postgres")
        config.setPassword("mysecretpassword")
        config.setMaximumPoolSize(2)
        config
      }
      transactor <- HikariTransactor.fromHikariConfig[IO](hikariConfig)
    } yield transactor)
      .map(_.copy(strategy0 = transactor.Strategy.void))

  test("Query cancel with Hikari") {
    val insert = for {
      _ <- sql"CREATE TABLE if not exists blah (i text)".update.run
      _ <- sql"truncate table blah".update.run
      _ <- sql"INSERT INTO blah values ('1')".update.run
      _ <- sql"INSERT INTO blah select concat(2, pg_sleep(1))".update.run
    } yield ()
    val scenario = transactorRes.use { xa =>
      for {
        fiber <- insert.transact(xa).start
        _ <- IO.sleep(200.millis) *> fiber.cancel
        _ <- IO.sleep(3.second)
        _ <- fiber.join.attempt
        result <- sql"select * from blah order by i".query[String].to[List].transact(xa)
      } yield {
        assertEquals(result, List("1"))
      }
    }

    scenario.unsafeRunSync()
  }

}
