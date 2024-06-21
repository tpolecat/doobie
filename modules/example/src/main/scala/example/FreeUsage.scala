// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package example

import java.io.File

import cats.effect.{IO, IOApp}
import cats.effect.syntax.monadCancel._
import cats.syntax.all._
import doobie._
import doobie.implicits._

// JDBC program using the low-level API
object FreeUsage extends IOApp.Simple {

  final case class CountryCode(code: String)

  def run: IO[Unit] = {
    val db = Transactor.fromDriverManager[IO](
      "org.h2.Driver",
      "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1",
      "sa",
      "",
      logHandler = None
    )
    db.trans.apply(examples.void)
  }

  def examples: ConnectionIO[String] =
    for {
      _ <- FC.delay(println("Loading database..."))
      _ <- loadDatabase(new File("example/world.sql"))
      s <- speakerQuery("English", 10)
      _ <- s.traverse(a => FC.delay(println(a)))
    } yield "Ok"

  def loadDatabase(f: File): ConnectionIO[Boolean] =
    FC.prepareStatement("RUNSCRIPT FROM ? CHARSET 'UTF-8'").bracket { ps =>
      FC.embed(ps, FPS.setString(1, f.getName) *> FPS.execute)
    }(FC.embed(_, FPS.close))

  def speakerQuery(s: String, p: Double): ConnectionIO[List[CountryCode]] =
    FC.prepareStatement("SELECT COUNTRYCODE FROM COUNTRYLANGUAGE WHERE LANGUAGE = ? AND PERCENTAGE > ?").bracket { ps =>
      FC.embed(ps, speakerPS(s, p))
    }(FC.embed(_, FPS.close))

  def speakerPS(s: String, p: Double): PreparedStatementIO[List[CountryCode]] =
    for {
      _ <- FPS.setString(1, s)
      _ <- FPS.setDouble(2, p)
      l <- FPS.executeQuery.bracket { rs =>
        FPS.embed(rs, unroll(FRS.getString(1).map(CountryCode(_))))
      }(FPS.embed(_, FRS.close))
    } yield l

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def unroll[A](a: ResultSetIO[A]): ResultSetIO[List[A]] = {
    def unroll0(as: List[A]): ResultSetIO[List[A]] =
      FRS.next >>= {
        case false => as.pure[ResultSetIO]
        case true  => a >>= { a => unroll0(a :: as) }
      }
    unroll0(Nil).map(_.reverse)
  }

}
