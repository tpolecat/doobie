// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package example

import cats.effect._
import doobie._
import doobie.implicits._
import org.slf4j.{ Marker, Logger, LoggerFactory }

/** Add arbitrary stuff to the log output by wrapping the logger your transactor uses. */
object CorrelationId extends IOApp {

  val loggerName ="example.CorrelationId"

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  val baseLogger: Logger = {
    System.getProperties.put(s"org.slf4j.simpleLogger.log.$loggerName", "trace")
    LoggerFactory.getLogger(loggerName)
  }

  val baseTransactor: Transactor[IO] =
    Transactor.fromDriverManager[IO](
      "org.postgresql.Driver",
      "jdbc:postgresql:world",
      "postgres", ""
    )

  def select(pattern: String): ConnectionIO[List[String]] =
    sql"select name from country where name like $pattern".query[String].to[List]

  def prog(pattern: String, cid: String): IO[Unit] = {
    val logger = CorrelatedLogger(baseLogger, cid, true)
    val xa     = baseTransactor.copy(logger = logger)
    for {
      ss <- select(pattern).transact(xa)
      _  <- IO(println(s"*** Answers were: ${ss.mkString(", ")}"))
    } yield ()
  }

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- prog("U%", "foo")
      _ <- prog("T%", "bar")
    } yield ExitCode.Success

}

/** A logger that adds a correlation id and optionally colors the output randomly. */
@SuppressWarnings(Array("org.wartremover.warts.Overloading"))
final case class CorrelatedLogger(delegate: Logger, cid: String, colored: Boolean) extends Logger {
  import CorrelatedLogger.colors

  private lazy val color: String =
    colors(cid.hashCode.abs % colors.length)

  private def format(message: String): String = {
    val text = s"[$cid] $message"
    if (colored) s"$color$text${Console.RESET}"
    else text
  }

  def debug(marker: Marker, message: String, throwable: Throwable): Unit =
    delegate.debug(marker, format(message), throwable)

  def debug(marker: Marker, message: String, args: Object*): Unit =
    delegate.debug(marker, format(message), args: _*)

  def debug(marker: Marker, message: String, arg1: Any, arg2: Any): Unit =
    delegate.debug(marker, format(message), arg1, arg2)

  def debug(marker: Marker, message: String, arg: Any): Unit =
    delegate.debug(marker, format(message), arg)

  def debug(marker: Marker, message: String): Unit =
    delegate.debug(marker, message)

  def debug(message: String, throwable: Throwable): Unit =
    delegate.debug(format(message), throwable)

  def debug(message: String, args: Object*): Unit =
    delegate.debug(format(message), args: _*)

  def debug(message: String, arg1: Any, arg2: Any): Unit =
    delegate.debug(format(message), arg1, arg2)

  def debug(message: String, arg: Any): Unit =
    delegate.debug(format(message), arg)

  def debug(message: String): Unit =
    delegate.debug(format(message))

  def error(marker: Marker, message: String, throwable: Throwable): Unit =
    delegate.error(marker, format(message), throwable)

  def error(marker: Marker, message: String, args: Object*): Unit =
    delegate.error(marker, format(message), args: _*)

  def error(marker: Marker, message: String, arg1: Any, arg2: Any): Unit =
    delegate.error(marker, format(message), arg1, arg2)

  def error(marker: Marker, message: String, x$3: Any): Unit =
    delegate.error(marker, format(message), x$3)

  def error(marker: Marker, message: String): Unit =
    delegate.error(marker, message)

  def error(message: String, throwable: Throwable): Unit =
    delegate.error(format(message), throwable)

  def error(message: String, args: Object*): Unit =
    delegate.error(format(message), args: _*)

  def error(message: String, arg1: Any, arg2: Any): Unit =
    delegate.error(format(message), arg1, arg2)

  def error(message: String, arg: Any): Unit =
    delegate.error(format(message), arg)

  def error(message: String): Unit =
    delegate.error(format(message))

  def getName(): String =
    delegate.getName()

  def info(marker: Marker, message: String, throwable: Throwable): Unit =
    delegate.info(marker, format(message), throwable)

  def info(marker: Marker, message: String, args: Object*): Unit =
    delegate.info(marker, format(message), args: _*)

  def info(marker: Marker, message: String, arg1: Any, arg2: Any): Unit =
    delegate.info(marker, format(message), arg1, arg2)

  def info(marker: Marker, message: String, x$3: Any): Unit =
    delegate.info(marker, format(message), x$3)

  def info(marker: Marker, message: String): Unit =
    delegate.info(marker, message)

  def info(message: String, throwable: Throwable): Unit =
    delegate.info(format(message), throwable)

  def info(message: String, args: Object*): Unit =
    delegate.info(format(message), args: _*)

  def info(message: String, arg1: Any, arg2: Any): Unit =
    delegate.info(format(message), arg1, arg2)

  def info(message: String, arg: Any): Unit =
    delegate.info(format(message), arg)

  def info(message: String): Unit =
    delegate.info(format(message))

  def isDebugEnabled(marker: Marker): Boolean =
    delegate.isDebugEnabled(marker: Marker)

  def isDebugEnabled(): Boolean =
    delegate.isDebugEnabled()

  def isErrorEnabled(marker: Marker): Boolean =
    delegate.isErrorEnabled(marker: Marker)

  def isErrorEnabled(): Boolean =
    delegate.isErrorEnabled()

  def isInfoEnabled(marker: Marker): Boolean =
    delegate.isInfoEnabled(marker: Marker)

  def isInfoEnabled(): Boolean =
    delegate.isInfoEnabled()

  def isTraceEnabled(marker: Marker): Boolean =
    delegate.isTraceEnabled(marker: Marker)

  def isTraceEnabled(): Boolean =
    delegate.isTraceEnabled()

  def isWarnEnabled(marker: Marker): Boolean =
    delegate.isWarnEnabled(marker: Marker)

  def isWarnEnabled(): Boolean =
    delegate.isWarnEnabled()

  def trace(marker: Marker, message: String, throwable: Throwable): Unit =
    delegate.trace(marker, format(message), throwable)

  def trace(marker: Marker, message: String, args: Object*): Unit =
    delegate.trace(marker, format(message), args: _*)

  def trace(marker: Marker, message: String, arg1: Any, arg2: Any): Unit =
    delegate.trace(marker, format(message), arg1, arg2)

  def trace(marker: Marker, message: String, x$3: Any): Unit =
    delegate.trace(marker, format(message), x$3)

  def trace(marker: Marker, message: String): Unit =
    delegate.trace(marker, message)

  def trace(message: String, throwable: Throwable): Unit =
    delegate.trace(format(message), throwable)

  def trace(message: String, args: Object*): Unit =
    delegate.trace(format(message), args: _*)

  def trace(message: String, arg1: Any, arg2: Any): Unit =
    delegate.trace(format(message), arg1, arg2)

  def trace(message: String, arg: Any): Unit =
    delegate.trace(format(message), arg)

  def trace(message: String): Unit =
    delegate.trace(format(message))

  def warn(marker: Marker, message: String, throwable: Throwable): Unit =
    delegate.warn(marker, format(message), throwable)

  def warn(marker: Marker, message: String, args: Object*): Unit =
    delegate.warn(marker, format(message), args: _*)

  def warn(marker: Marker, message: String, arg1: Any, arg2: Any): Unit =
    delegate.warn(marker, format(message), arg1, arg2)

  def warn(marker: Marker, message: String, x$3: Any): Unit =
    delegate.warn(marker, format(message), x$3)

  def warn(marker: Marker, message: String): Unit =
    delegate.warn(marker, message)

  def warn(message: String, throwable: Throwable): Unit =
    delegate.warn(format(message), throwable)

  def warn(message: String, arg1: Any, arg2: Any): Unit =
    delegate.warn(format(message), arg1, arg2)

  def warn(message: String, args: Object*): Unit =
    delegate.warn(format(message), args: _*)

  def warn(message: String, arg: Any): Unit =
    delegate.warn(format(message), arg)

  def warn(message: String): Unit =
    delegate.warn(format(message))

}

object CorrelatedLogger {

  private val colors: Array[String] = {
    import Console._
    val cs = Array(RED, GREEN, BLUE, YELLOW, MAGENTA, CYAN)
    cs ++ cs.map(BOLD + _)
  }

}