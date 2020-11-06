// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.effect.Sync
import _root_.io.chrisdavenport.log4cats.MessageLogger
import _root_.io.chrisdavenport.log4cats.slf4j.Slf4jLogger

final class DefaultMessageLogger[F[_]](delegate: MessageLogger[F]) extends MessageLogger[F] {
  def error(message: => String): F[Unit] = delegate.error(message)
  def warn(message: => String): F[Unit] = delegate.warn(message)
  def info(message: => String): F[Unit] = delegate.info(message)
  def debug(message: => String): F[Unit] = delegate.debug(message)
  def trace(message: => String): F[Unit] = delegate.trace(message)
}

object DefaultMessageLogger extends DefaultMessageLoggerLow {

  def apply[F[_]](implicit ev: DefaultMessageLogger[F]): ev.type = ev

  implicit def provided[F[_]](implicit ev: MessageLogger[F]): DefaultMessageLogger[F] =
    new DefaultMessageLogger(ev)

}

trait DefaultMessageLoggerLow {

  implicit def defaulted[F[_]: Sync]: DefaultMessageLogger[F] =
    new DefaultMessageLogger(Slf4jLogger.getLoggerFromName("doobie"))

}