// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.Applicative
import cats.effect.Sync

import java.util.logging.Logger
import scala.concurrent.duration.{FiniteDuration => FD}
import scala.Predef.augmentString

/** A module of types and instances for logged statements. */
object log {

  /** Algebraic type of events that can be passed to a `LogHandler`, both parameterized by the argument type of the SQL
    * input parameters (this is typically an `HList`).
    * @group Events
    */
  sealed abstract class LogEvent extends Product with Serializable {

    /** The complete SQL string as seen by JDBC. */
    def sql: String

    /** The query arguments. */
    def args: List[Any]

    def label: String

  }

  /** @group Events */
  final case class Success(sql: String, args: List[Any], label: String, exec: FD, processing: FD) extends LogEvent

  /** @group Events */
  final case class ProcessingFailure(
      sql: String,
      args: List[Any],
      label: String,
      exec: FD,
      processing: FD,
      failure: Throwable
  ) extends LogEvent

  /** @group Events */
  final case class ExecFailure(sql: String, args: List[Any], label: String, exec: FD, failure: Throwable)
      extends LogEvent

  /** Provides additional processing for Doobie `LogEvent`s.
    * @group Handlers
    */
  trait LogHandler[M[_]] {
    def run(logEvent: LogEvent): M[Unit]
  }

  /** Module of instances and constructors for `LogHandler`.
    * @group Handlers
    */
  object LogHandler {
    private val jdkLogger = Logger.getLogger(getClass.getName)

    /** A LogHandler that doesn't do anything */
    def noop[M[_]: Applicative]: LogHandler[M] = {
      new LogHandler[M] {
        private val unit = Applicative[M].unit
        override def run(logEvent: LogEvent): M[Unit] = unit
      }
    }

    /** A LogHandler that writes a default format to a JDK Logger. This is provided for demonstration purposes and is
      * not intended for production use.
      * @group Constructors
      */
    def jdkLogHandler[M[_]: Sync]: LogHandler[M] = new LogHandler[M] {
      override def run(logEvent: LogEvent): M[Unit] = Sync[M].delay(
        logEvent match {
          case Success(s, a, l, e1, e2) =>
            jdkLogger.info(s"""Successful Statement Execution:
                              |
                              |  ${s.linesIterator.dropWhile(_.trim.isEmpty).mkString("\n  ")}
                              |
                              | arguments = [${a.mkString(", ")}]
                              | label     = $l
                              |   elapsed = ${e1.toMillis.toString} ms exec + ${e2.toMillis
                               .toString} ms processing (${(e1 + e2).toMillis.toString} ms total)
              """.stripMargin)

          case ProcessingFailure(s, a, l, e1, e2, t) =>
            jdkLogger.severe(s"""Failed Resultset Processing:
                                |
                                |  ${s.linesIterator.dropWhile(_.trim.isEmpty).mkString("\n  ")}
                                |
                                | arguments = [${a.mkString(", ")}]
                                | label     = $l
                                |   elapsed = ${e1.toMillis.toString} ms exec + ${e2.toMillis
                                 .toString} ms processing (failed) (${(e1 + e2).toMillis.toString} ms total)
                                |   failure = ${t.getMessage}
              """.stripMargin)

          case ExecFailure(s, a, l, e1, t) =>
            jdkLogger.severe(s"""Failed Statement Execution:
                                |
                                |  ${s.linesIterator.dropWhile(_.trim.isEmpty).mkString("\n  ")}
                                |
                                | arguments = [${a.mkString(", ")}]
                                | label     = $l
                                |   elapsed = ${e1.toMillis.toString} ms exec (failed)
                                |   failure = ${t.getMessage}
              """.stripMargin)
        }
      )
    }
  }
}
