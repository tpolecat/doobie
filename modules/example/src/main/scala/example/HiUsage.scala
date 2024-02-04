// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package example

import cats.Show
import cats.effect.{IO, IOApp}
import cats.syntax.all._
import fs2.Stream
import doobie._
import doobie.implicits._

// JDBC program using the high-level API
object HiUsage extends IOApp.Simple {

  // A very simple data type we will read
  final case class CountryCode(code: Option[String])
  object CountryCode {
    implicit val show: Show[CountryCode] = Show.fromToString
  }

  // Program entry point
  def run: IO[Unit] = {
    val db = Transactor.fromDriverManager[IO](
      driver = "org.postgresql.Driver",
      url = "jdbc:postgresql:world",
      user = "postgres",
      password = "password",
      logHandler = None
    )
    example.transact(db)
  }

  // An example action. Streams results to stdout
  lazy val example: ConnectionIO[Unit] =
    speakerQuery("English", 10).evalMap(c => FC.delay(println(show"~> $c"))).compile.drain

  // Construct an action to find countries where more than `pct` of the population speaks `lang`.
  // The result is a fs2.Stream that can be further manipulated by the caller.
  def speakerQuery(lang: String, pct: Double): Stream[ConnectionIO, CountryCode] =
    sql"SELECT COUNTRYCODE FROM COUNTRYLANGUAGE WHERE LANGUAGE = $lang AND PERCENTAGE > $pct".query[CountryCode].stream

}
