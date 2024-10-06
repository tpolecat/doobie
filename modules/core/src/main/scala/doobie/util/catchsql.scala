// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.ApplicativeError
import cats.syntax.applicativeError.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.functor.*
import doobie.enumerated.SqlState
import java.sql.SQLException

/** Module of additional combinators for `ApplicativeError`, specific to `SQLException`.
  */
object catchsql {

  /** Like `attempt` but catches only `SQLException`. */
  def attemptSql[M[_], A](ma: M[A])(implicit F: ApplicativeError[M, Throwable]): M[Either[SQLException, A]] =
    ma.map(_.asRight[SQLException]).recover {
      case e: SQLException => e.asLeft
    }

  /** Like `attempt` but catches only the defined `SQLException`. */
  def attemptSomeSql[M[_], A, B](ma: M[A])(f: PartialFunction[SQLException, B])(implicit AE: ApplicativeError[M, Throwable]): M[Either[B, A]] =
    ma.map(_.asRight[B]).recoverWith { case e: SQLException =>
      f.lift(e).fold(AE.raiseError[Either[B, A]](e))(b => AE.pure(b.asLeft))
    }

  /** Like `attemptSql` but yields only the exception's `SqlState`. */
  def attemptSqlState[M[_], A](ma: M[A])(
      implicit ev: ApplicativeError[M, Throwable]
  ): M[Either[SqlState, A]] =
    attemptSql(ma).map(_.leftMap(e => SqlState(e.getSQLState)))

  def attemptSomeSqlState[M[_], A, B](ma: M[A])(f: PartialFunction[SqlState, B])(implicit
      AE: ApplicativeError[M, Throwable]
  ): M[Either[B, A]] =
    ma.map(_.asRight[B]).recoverWith {
      case e: SQLException =>
        f.lift(SqlState(e.getSQLState)).fold(AE.raiseError[Either[B, A]](e))(b => AE.pure(b.asLeft))
    }

  /** Executes the handler, for exceptions propagating from `ma`. */
  def exceptSql[M[_], A](ma: M[A])(handler: SQLException => M[A])(implicit F: ApplicativeError[M, Throwable]): M[A] =
    ma.recoverWith {
      case e: SQLException => handler(e)
    }

  /** Executes the handler, for exceptions propagating from `ma`. */
  def exceptSqlState[M[_], A](ma: M[A])(handler: SqlState => M[A])(
      implicit ev: ApplicativeError[M, Throwable]
  ): M[A] =
    exceptSql(ma)(e => handler(SqlState(e.getSQLState)))

  /** Executes the handler where defined, for exceptions propagating from `ma`. */
  def exceptSomeSqlState[M[_], A](ma: M[A])(pf: PartialFunction[SqlState, M[A]])(implicit
      AE: ApplicativeError[M, Throwable]
  ): M[A] =
    exceptSql(ma) { e =>
      pf.lift(SqlState(e.getSQLState)).fold(AE.raiseError[A](e))(a => a)
    }

  /** Like "finally", but only performs the final action if there was an exception. */
  def onSqlException[M[_], A, B](ma: M[A])(action: M[B])(implicit F: ApplicativeError[M, Throwable]): M[A] =
    exceptSql(ma)(e => action *> F.raiseError(e))

}
