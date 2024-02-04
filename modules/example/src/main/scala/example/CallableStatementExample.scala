// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package example

import cats.effect.{IO, IOApp}
import cats.effect.syntax.monadCancel._
import cats.syntax.all._
import doobie._
import doobie.enumerated.JdbcType.Other
import doobie.free.connection.ConnectionIO
import doobie.implicits._
import doobie.util.transactor.Transactor

object CallableStatementExample extends IOApp.Simple {

  // cant be in world.sql as H2 does not support SQL functions
  def createFunc: ConnectionIO[Int] = {
    val sql = """
      CREATE OR REPLACE FUNCTION getCountries(n integer, OUT c refcursor) AS '
        BEGIN
          OPEN c FOR SELECT name FROM country LIMIT n;
        END;
      ' LANGUAGE plpgsql;
    """
    HC.createStatement(HS.executeUpdate(sql))
  }

  def names(limit: Int): ConnectionIO[List[String]] =
    HC.prepareCall("{ call getCountries(?, ?) }") {
      for {
        _ <- FCS.setInt(1, limit)
        _ <- FCS.registerOutParameter(2, Other.toInt)
        _ <- FCS.execute
        ns <- FCS.getObject(2).map(_.asInstanceOf[java.sql.ResultSet]).bracket { rs =>
          FCS.embed(rs, HRS.list[String])
        }(FCS.embed(_, FRS.close))
      } yield ns
    }

  val xa: Transactor[IO] =
    Transactor.fromDriverManager[IO](
      driver = "org.postgresql.Driver",
      url = "jdbc:postgresql:world",
      user = "postgres",
      password = "password",
      logHandler = None
    )

  def run: IO[Unit] =
    for {
      _ <- createFunc.transact(xa)
      ns <- names(10).transact(xa)
      _ <- ns.traverse(s => IO(println(s)))
    } yield ()

}
