// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.syntax

import cats.ApplicativeError
import doobie.util.{catchsql as C}
import doobie.enumerated.SqlState
import java.sql.SQLException

class ApplicativeErrorOps[M[_], A](self: M[A])(implicit ev: ApplicativeError[M, Throwable]) {
  def attemptSql: M[Either[SQLException, A]] = C.attemptSql(self)
  def attemptSomeSql[B](f: PartialFunction[SQLException, B]): M[Either[B, A]] = C.attemptSomeSql(self)(f)
  def attemptSqlState: M[Either[SqlState, A]] = C.attemptSqlState(self)
  def attemptSomeSqlState[B](f: PartialFunction[SqlState, B]): M[Either[B, A]] = C.attemptSomeSqlState(self)(f)
  def exceptSql(handler: SQLException => M[A]): M[A] = C.exceptSql(self)(handler)
  def exceptSqlState(handler: SqlState => M[A]): M[A] = C.exceptSqlState(self)(handler)
  def exceptSomeSqlState(pf: PartialFunction[SqlState, M[A]]): M[A] = C.exceptSomeSqlState(self)(pf)
  def onSqlException[B](action: M[B]): M[A] = C.onSqlException(self)(action)
}

trait ToApplicativeErrorOps {
  implicit def toDoobieApplicativeErrorOps[M[_], A](ma: M[A])(
      implicit ev: ApplicativeError[M, Throwable]
  ): ApplicativeErrorOps[M, A] =
    new ApplicativeErrorOps(ma)
}

object applicativeerror extends ToApplicativeErrorOps
