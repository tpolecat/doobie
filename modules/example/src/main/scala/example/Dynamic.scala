// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package example

import doobie._
import doobie.implicits._
import cats.effect.{IO, IOApp}
import cats.syntax.all._

// Sketch of a program to run a query and get the output without knowing how many columns will
// come back, or their types. This can be useful for building query tools, etc.
object Dynamic extends IOApp.Simple {

  type Headers = List[String]
  type Data = List[List[Object]]

  val xa = Transactor.fromDriverManager[IO](
    driver = "org.postgresql.Driver",
    url = "jdbc:postgresql:world",
    user = "postgres",
    password = "password",
    logHandler = None
  )

  // Entry point. Run a query and print the results out.
  def run: IO[Unit] =
    connProg("U%").transact(xa).flatMap { case (headers, data) =>
      for {
        _ <- IO(println(headers))
        _ <- data.traverse(d => IO(println(d)))
      } yield ()
    }

  // Construct a parameterized query and execute it with a custom program.
  def connProg(pattern: String): ConnectionIO[(Headers, Data)] =
    sql"select code, name, population from country where code like $pattern".execWith(exec)

  // Exec our PreparedStatement, examining metadata to figure out column count.
  def exec: PreparedStatementIO[(Headers, Data)] =
    for {
      md <- HPS.getMetaData // lots of useful info here
      cols = (1 to md.getColumnCount).toList
      data <- HPS.executeQuery(readAll(cols))
    } yield (cols.map(md.getColumnName), data)

  // Read the specified columns from the resultset.
  def readAll(cols: List[Int]): ResultSetIO[Data] =
    readOne(cols).whileM[List](HRS.next)

  // Take a list of column offsets and read a parallel list of values.
  def readOne(cols: List[Int]): ResultSetIO[List[Object]] =
    cols.traverse(FRS.getObject) // always works

}
