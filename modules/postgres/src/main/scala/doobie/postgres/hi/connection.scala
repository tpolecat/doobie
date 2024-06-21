// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres.hi

import cats.~>
import cats.data.Kleisli
import cats.free.Free
import org.postgresql.{PGConnection, PGNotification}
import doobie._, doobie.implicits._
import doobie.postgres.free.{Embeddable, KleisliInterpreter}
import doobie.postgres.free.{pgconnection => IPFPC}
import doobie.postgres.hi.{pgconnection => IPHPC}

/** Module of safe `PGConnectionIO` operations lifted into `ConnectionIO`. */
object connection {

  // An intepreter for lifting PGConnectionIO into ConnectionIO
  val defaultInterpreter: IPFPC.PGConnectionOp ~> Kleisli[ConnectionIO, PGConnection, *] =
    KleisliInterpreter[ConnectionIO](LogHandler.noop).PGConnectionInterpreter

  val pgGetBackendPID: ConnectionIO[Int] =
    pgGetConnection(IPFPC.getBackendPID)

  def pgGetConnection[A](k: PGConnectionIO[A]): ConnectionIO[A] =
    FC.unwrap(classOf[PGConnection]).flatMap(k.foldMap(defaultInterpreter).run)

  def embed[F[_], J, B](j: J, op: Free[F, B])(implicit ev: Embeddable[F, J]): ConnectionIO[B] =
    pgGetConnection(IPFPC.embed(j, op))

  def pgGetCopyAPI[A](k: CopyManagerIO[A]): ConnectionIO[A] =
    pgGetConnection(IPHPC.getCopyAPI(k))

  def pgGetLargeObjectAPI[A](k: LargeObjectManagerIO[A]): ConnectionIO[A] =
    pgGetConnection(IPHPC.getLargeObjectAPI(k))

  val pgGetNotifications: ConnectionIO[List[PGNotification]] =
    pgGetConnection(IPHPC.getNotifications)

  val pgGetPrepareThreshold: ConnectionIO[Int] =
    pgGetConnection(IPHPC.getPrepareThreshold)

  def pgSetPrepareThreshold(threshold: Int): ConnectionIO[Unit] =
    pgGetConnection(IPHPC.setPrepareThreshold(threshold))

  /** Construct a program that notifies on the given channel. Note that the channel is NOT sanitized; it cannot be
    * passed as a parameter and is simply interpolated into the statement. DO NOT pass user input here.
    */
  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def pgNotify(channel: String): ConnectionIO[Unit] =
    execVoid("NOTIFY " + channel)

  /** Construct a program that notifies on the given channel, with a payload. Note that neither the channel nor the
    * payload are sanitized; neither can be passed as parameters and are simply interpolated into the statement. DO NOT
    * pass user input here.
    */
  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def pgNotify(channel: String, payload: String): ConnectionIO[Unit] =
    execVoid(s"NOTIFY $channel, '$payload'")

  /** Construct a program that starts listening on the given channel. Note that the channel is NOT sanitized; it cannot
    * be passed as a parameter and is simply interpolated into the statement. DO NOT pass user input here.
    */
  def pgListen(channel: String): ConnectionIO[Unit] =
    execVoid("LISTEN " + channel)

  /** Construct a program that stops listening on the given channel. Note that the channel is NOT sanitized; it cannot
    * be passed as a parameter and is simply interpolated into the statement. DO NOT pass user input here.
    */
  def pgUnlisten(channel: String): ConnectionIO[Unit] =
    execVoid("UNLISTEN " + channel)

  // a helper
  private def execVoid(sql: String): ConnectionIO[Unit] =
    HC.prepareStatement(sql)(HPS.executeUpdate).map(_ => ())

}
