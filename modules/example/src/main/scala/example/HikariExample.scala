// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package example

import cats.effect._
import doobie._
import doobie.hikari._
import doobie.implicits._
import com.zaxxer.hikari.HikariConfig

object HikariExample extends IOApp.Simple {

  // Typically you construct a transactor this way, using lifetime-managed thread pools.
  val transactorRes: Resource[IO, Transactor[IO]] =
    for {
      hikariConfig <- Resource.pure {
        val config = new HikariConfig()
        config.setDriverClassName("org.postgresql.Driver")
        config.setJdbcUrl("jdbc:postgresql:world")
        config.setUsername("postgres")
        config.setPassword("password")
        config
      }
      transactor <- HikariTransactor.fromHikariConfig[IO](hikariConfig)
    } yield transactor

  def run: IO[Unit] =
    // Note how we allocate the Transactor once (`use`) and reuse it
    // throughout our application
    transactorRes.use { transactor =>
      val personService = new UserService(transactor)

      for {
        _ <- sql"DROP TABLE if exists person".update.run.transact(transactor)
        _ <- sql"CREATE TABLE person(id INT NOT NULL PRIMARY KEY, name TEXT NOT NULL)".update.run.transact(transactor)
        _ <- personService.insertPerson(1, "Alice")
        _ <- personService.insertPerson(2, "Bob")
        name1 <- personService.getPersonName(1)
        name2 <- personService.getPersonName(2)
        name3 <- personService.getPersonName(3)
        _ <- IO {
          println(s"person 1: $name1")
          println(s"person 2: $name2")
          println(s"person 3: $name3")
        }
      } yield ()
    }

}

class UserService(transactor: Transactor[IO]) {
  def insertPerson(id: Int, name: String): IO[Unit] = {
    sql"insert into person (id, name) values ($id, $name)"
      .update
      .run
      .transact(transactor)
      .as(())
  }

  def getPersonName(id: Int): IO[Option[String]] = {
    sql"select name from person where id = $id"
      .query[String]
      .option
      .transact(transactor)
  }
}
