// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

// relies on streaming, so no cats for now
package example

import cats.effect._
import cats.syntax.all._
import doobie._
import doobie.implicits._
import doobie.postgres._
import org.postgresql._
import fs2.{Stream, Pipe}
import fs2.Stream._
import scala.concurrent.duration._

/** Example exposing PostgreSQL NOTIFY as a Process[ConnectionIO, PGNotification]. This will likely be provided as a
  * standard service in doobie-postgres in a future version. To play with this program, run it and then in another
  * window do:
  *
  * > psql -d world -U postgres -c "notify foo, 'abc'"
  *
  * to send a notification. The program will exit after reading five notifications.
  */
object PostgresNotify extends IOApp.Simple {

  /** A resource that listens on a channel and unlistens when we're done. */
  def channel(name: String): Resource[ConnectionIO, Unit] =
    Resource.make(PHC.pgListen(name) *> HC.commit)(_ => PHC.pgUnlisten(name) *> HC.commit)

  /** Stream of PGNotifications on the specified channel, polling at the specified rate. Note that this stream, when
    * run, will commit the current transaction.
    */
  def notificationStream(
      channelName: String,
      pollingInterval: FiniteDuration
  ): Stream[IO, PGNotification] = {
    val inner: Pipe[ConnectionIO, FiniteDuration, PGNotification] = ticks =>
      for {
        _ <- resource(channel(channelName))
        _ <- ticks
        ns <- eval(PHC.pgGetNotifications <* HC.commit)
        n <- emits(ns)
      } yield n
    awakeEvery[IO](pollingInterval).through(inner.transact(xa))
  }

  /** A transactor that knows how to connect to a PostgreSQL database. */
  val xa = Transactor.fromDriverManager[IO](
    driver = "org.postgresql.Driver",
    url = "jdbc:postgresql:world",
    user = "postgres",
    password = "password",
    logHandler = None
  )

  /** Construct a stream of PGNotifications that prints to the console. Transform it to a runnable process using the
    * transactor above, and run it.
    */
  def run: IO[Unit] =
    notificationStream("foo", 1.second)
      .map(n => show"${n.getPID} ${n.getName} ${n.getParameter}")
      .take(5)
      .evalMap(s => IO.delay(Console.println(s))).compile.drain

}
