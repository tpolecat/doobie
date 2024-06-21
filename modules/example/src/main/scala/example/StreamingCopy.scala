// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package example

import java.sql.Connection
import scala.util.control.NonFatal

import cats.data._
import cats.effect._
import cats.syntax.all._
import doobie._
import doobie.implicits._
import fs2.Stream

/** Example of resource-safe transactional database-to-database copy with fs2. If you induce failures on either side (by
  * putting a typo in the `read` or `write` statements) both transactions will roll back.
  */
object StreamingCopy extends IOApp.Simple {

  /** Cross-transactor streaming when the `source` and `sink` have the same schema.
    */
  def fuseMap[F[_], A, B](
      source: Stream[ConnectionIO, A],
      sink: A => ConnectionIO[B]
  )(
      sourceXA: Transactor[F],
      sinkXA: Transactor[F]
  )(
      implicit ev: MonadCancelThrow[F]
  ): Stream[F, B] =
    fuseMapGeneric(source, identity[A], sink)(sourceXA, sinkXA)

  /** Stream from `source` through `sink`, where source and sink run on distinct transactors. To do this we have to wrap
    * one transactor around the other. The source output and sink input types can differ. This enables data
    * transformations involving potentially different database schemas.
    */
  def fuseMapGeneric[F[_], A, B, C](
      source: Stream[ConnectionIO, A],
      sourceToSink: A => B,
      sink: B => ConnectionIO[C]
  )(
      sourceXA: Transactor[F],
      sinkXA: Transactor[F]
  )(
      implicit ev: MonadCancelThrow[F]
  ): Stream[F, C] = {

    // Interpret a ConnectionIO into a Kleisli arrow for F via the sink interpreter.
    def interpS[T](f: ConnectionIO[T]): Connection => F[T] =
      f.foldMap(sinkXA.interpret).run

    // Open a connection in `F` via the sink transactor. Need patmat due to the existential.
    val conn: Resource[F, Connection] =
      sinkXA match { case xa => xa.connect(xa.kernel) }

    // Given a Connection we can construct the stream we want.
    def mkStream(c: Connection): Stream[F, C] = {

      // Now we can interpret a ConnectionIO into a Stream of F via the sink interpreter.
      def evalS(f: ConnectionIO[_]): Stream[F, Nothing] =
        Stream.eval(interpS(f)(c)).drain

      // And can thus lift all the sink operations into Stream of F
      val sinkʹ = (a: A) => evalS(sink(sourceToSink(a)))
      val before = evalS(sinkXA.strategy.before)
      val after = evalS(sinkXA.strategy.after)
      def oops(t: Throwable) = evalS(sinkXA.strategy.oops <* FC.raiseError(t))

      // And construct our final stream.
      (before ++ source.transact(sourceXA).flatMap(sinkʹ) ++ after).onError {
        case NonFatal(e) => oops(e)
      }

    }

    // And we're done!
    Stream.resource(conn).flatMap(mkStream)

  }

  // Everything below is code to demonstrate the combinator above.

  /** Prepend a ConnectionIO program with a log message. */
  def printBefore(tag: String, s: String): ConnectionIO[Unit] => ConnectionIO[Unit] =
    HC.delay(Console.println(s"$tag: $s")) <* _

  /** Derive a new transactor that logs stuff. */
  def addLogging[F[_], A](name: String)(xa: Transactor[F]): Transactor[F] = {
    import Transactor._ // bring the lenses into scope
    val update: State[Transactor[F], Unit] =
      for {
        _ <- before %= printBefore(name, "before - setting up the connection")
        _ <- after %= printBefore(name, "after - committing")
        _ <- oops %= printBefore(name, "oops - rolling back")
        _ <- always %= printBefore(name, "always - closing")
      } yield ()
    update.runS(xa).value
  }

  // A data type to move.
  final case class City(id: Int, name: String, countrycode: String, district: String, population: Int)

  // A producer of cities, to be run on database 1
  def read: Stream[ConnectionIO, City] =
    sql"""
      SELECT id, name, countrycode, district, population
      FROM city
      WHERE name like 'X%'
    """.query[City].stream

  // A consumer of cities, to be run on database 2
  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  def write(c: City): ConnectionIO[Unit] =
    printBefore("write", c.toString)(
      sql"""
        INSERT INTO city (id, name, countrycode, district, population)
        VALUES (${c.id}, ${c.name}, ${c.countrycode}, ${c.district}, ${c.population})
      """.update.run.void
    )

  /** Table creation for our destination DB. We assume the source is populated. */
  val ddl: ConnectionIO[Unit] =
    sql"""
      CREATE TABLE IF NOT EXISTS city (
          id integer NOT NULL,
          name varchar NOT NULL,
          countrycode character(3) NOT NULL,
          district varchar NOT NULL,
          population integer NOT NULL
      )
    """.update.run.void

  // A postges transactor for our source. We assume the WORLD database is set up already.
  val pg = addLogging("Postgres")(Transactor.fromDriverManager[IO](
    driver = "org.postgresql.Driver",
    url = "jdbc:postgresql:world",
    user = "postgres",
    password = "password",
    logHandler = None
  ))

  // An h2 transactor for our sink.
  val h2 = addLogging("H2") {
    val xa = Transactor.fromDriverManager[IO](
      driver = "org.h2.Driver",
      url = "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1",
      user = "sa",
      password = "",
      logHandler = None
    )
    Transactor.before.modify(xa, _ *> ddl) // Run our DDL on every connection
  }

  // Our main program
  def run: IO[Unit] =
    for {
      _ <- fuseMap(read, write)(pg, h2).compile.drain // do the copy with fuseMap
      n <- sql"select count(*) from city".query[Int].unique.transact(h2)
      _ <- IO(Console.println(show"Copied $n cities!"))
    } yield ()

}
