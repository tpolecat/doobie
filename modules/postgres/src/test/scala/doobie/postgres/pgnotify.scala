// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import cats.effect.{ IO, ContextShift, Sync }
import cats.implicits._
import doobie._, doobie.implicits._
import org.postgresql.PGNotification
import org.specs2.mutable.Specification
import scala.concurrent.ExecutionContext

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
object pgnotifyspec extends Specification {

  import FC.{commit, delay}

  implicit def contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  val xa = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:world",
    "postgres", ""
  )

  // Listen on the given channel, notify on another connection
  def listen[A](channel: String, notify: ConnectionIO[A]): IO[List[PGNotification]] =
    (PHC.pgListen(channel) *> commit *>
     delay { Thread.sleep(50) } *>
     Sync[ConnectionIO].delay(notify.transact(xa).unsafeRunSync) *>
     delay { Thread.sleep(50) } *>
     PHC.pgGetNotifications).transact(xa)

  "LISTEN/NOTIFY" should {

    "allow cross-connection notification" in  {
      val channel = "cha" + System.nanoTime.toString
      val notify  = PHC.pgNotify(channel)
      val test    = listen(channel, notify).map(_.length)
      test.unsafeRunSync must_== 1
    }

    "allow cross-connection notification with parameter" in  {
      val channel  = "chb" + System.nanoTime.toString
      val messages = List("foo", "bar", "baz", "qux")
      val notify   = messages.traverse(PHC.pgNotify(channel, _))
      val test     = listen(channel, notify).map(_.map(_.getParameter))
      test.unsafeRunSync must_== messages
    }

    "collapse identical notifications" in  {
      val channel  = "chc" + System.nanoTime.toString
      val messages = List("foo", "bar", "bar", "baz", "qux", "foo")
      val notify   = messages.traverse(PHC.pgNotify(channel, _))
      val test     = listen(channel, notify).map(_.map(_.getParameter))
      test.unsafeRunSync must_== messages.distinct
    }

  }

}
