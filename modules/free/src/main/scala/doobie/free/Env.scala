// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.free

import cats.Functor
import org.slf4j.Logger
import scala.concurrent.ExecutionContext

/**
 * Interpreter environment.
 * @param jdbc The JDBC managed object (Connection, Statement, etc.)
 * @param logger A logger for tracing execution.
 * @param blockingContext A blocking execution context for primitive operations.
 */
final case class Env[J](
  jdbc:            J,
  logger:          Logger,
  blockingContext: ExecutionContext
) {
  import Predef._

  private lazy val cname  = jdbc.getClass.getSimpleName // TODO: make this more robust
  private lazy val hash   = System.identityHashCode(jdbc).toHexString //.padTo(8, ' ')
  private lazy val prefix = s"$cname<$hash>".padTo(30, ' ').take(30)

  private def format(elapsed: Long, message: String): String = {
    f"$elapsed%4d ms $prefix $message"
  }

  /**
   * Log an operation on `jdbc` yielding a value of type `A`. The logged message will be at `TRACE`
   * level and will include the execution time, class and hash of `jdbc`.
   * {{{
   *   14 ms PgPreparedStatement<13438d6>   executeQuery()
   * }}}
   */
  def unsafeTrace[A](msg: => String)(f: J => A): A =
    if (logger.isTraceEnabled) {
      val t0 = System.currentTimeMillis
      try {
        f(jdbc)
      } finally {
        val t1 = System.currentTimeMillis
        logger.trace(format(t1 - t0, msg))
      }
    } else f(jdbc)

  /** Env is a covariant functor. */
  def map[B](f: J => B): Env[B] =
    Env(f(jdbc), logger, blockingContext)

}

object Env {

  /** Env is a covariant functor. */
  implicit val EnvFunctor: Functor[Env] =
    new Functor[Env] {
      def map[A, B](fa: Env[A])(f: A => B): Env[B] =
        fa.map(f)
    }

}