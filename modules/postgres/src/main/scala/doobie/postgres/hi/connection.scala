// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres.hi

import cats.~>
import cats.data.Kleisli
import org.postgresql.{ PGConnection, PGNotification }
import doobie._, doobie.implicits._
import doobie.postgres.free.{ Env, KleisliInterpreter }

/** Module of safe `PGConnectionIO` operations lifted into `ConnectionIO`. */
object connection {

  // An intepreter for lifting PGConnectionIO into ConnectionIO
  val defaultInterpreter: PFPC.PGConnectionOp ~> Kleisli[ConnectionIO, Env[PGConnection], ?] =
    KleisliInterpreter[ConnectionIO].PGConnectionInterpreter

  val pgGetBackendPID: ConnectionIO[Int] =
    pgGetConnection(PFPC.getBackendPID)

  def pgGetConnection[A](k: PGConnectionIO[A]): ConnectionIO[A] =
    FC.liftE[PGConnectionIO] { env =>
      λ[PGConnectionIO ~> ConnectionIO] { pg =>
        pg.foldMap(defaultInterpreter).run(env.map(_.unwrap(classOf[PGConnection])))
      }
    } flatMap { interp =>
      interp(k)
    }

  def pgGetCopyAPI[A](k: CopyManagerIO[A]): ConnectionIO[A] =
    pgGetConnection(PHPC.getCopyAPI(k))

  def pgGetFastpathAPI[A](k: FastpathIO[A]): ConnectionIO[A] =
    pgGetConnection(PHPC.getFastpathAPI(k))

  def pgGetLargeObjectAPI[A](k: LargeObjectManagerIO[A]): ConnectionIO[A] =
    pgGetConnection(PHPC.getLargeObjectAPI(k))

  val pgGetNotifications: ConnectionIO[List[PGNotification]] =
    pgGetConnection(PHPC.getNotifications)

  val pgGetPrepareThreshold: ConnectionIO[Int] =
    pgGetConnection(PHPC.getPrepareThreshold)

  def pgSetPrepareThreshold(threshold: Int): ConnectionIO[Unit] =
    pgGetConnection(PHPC.setPrepareThreshold(threshold))

  /**
   * Construct a program that notifies on the given channel. Note that the channel is NOT sanitized;
   * it cannot be passed as a parameter and is simply interpolated into the statement. DO NOT pass
   * user input here.
   */
  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def pgNotify(channel: String): ConnectionIO[Unit] =
    execVoid("NOTIFY " + channel)

  /**
   * Construct a program that notifies on the given channel, with a payload. Note that neither the
   * channel nor the payload are sanitized; neither can be passed as parameters and are simply
   * interpolated into the statement. DO NOT pass user input here.
   */
  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def pgNotify(channel: String, payload: String): ConnectionIO[Unit] =
    execVoid(s"NOTIFY $channel, '$payload'")

  /**
   * Construct a program that starts listening on the given channel. Note that the channel is NOT
   * sanitized; it cannot be passed as a parameter and is simply interpolated into the statement.
   * DO NOT pass user input here.
   */
  def pgListen(channel: String): ConnectionIO[Unit] =
    execVoid("LISTEN " + channel)

  /**
   * Construct a program that stops listening on the given channel. Note that the channel is NOT
   * sanitized; it cannot be passed as a parameter and is simply interpolated into the statement.
   * DO NOT pass user input here.
   */
  def pgUnlisten(channel: String): ConnectionIO[Unit] =
    execVoid("UNLISTEN " + channel)


  // a helper
  private def execVoid(sql: String): ConnectionIO[Unit] =
    HC.prepareStatement(sql)(HPS.executeUpdate).map(_ => ())

}
