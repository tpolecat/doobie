// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

// format: off

package doobie.free

import cats.{~>, Applicative, Semigroup, Monoid}
import cats.effect.kernel.{ CancelScope, Poll, Sync }
import cats.free.{ Free as FF } // alias because some algebras have an op called Free
import doobie.util.log.LogEvent
import doobie.WeakAsync
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

import java.lang.Class
import java.lang.String
import java.sql.Connection
import java.sql.ResultSet
import java.sql.SQLWarning
import java.sql.Statement

// This file is Auto-generated using FreeGen2.scala
object statement { module =>

  // Algebra of operations for Statement. Each accepts a visitor as an alternative to pattern-matching.
  sealed trait StatementOp[A] {
    def visit[F[_]](v: StatementOp.Visitor[F]): F[A]
  }

  // Free monad over StatementOp.
  type StatementIO[A] = FF[StatementOp, A]

  // Module of instances and constructors of StatementOp.
  object StatementOp {

    // Given a Statement we can embed a StatementIO program in any algebra that understands embedding.
    implicit val StatementOpEmbeddable: Embeddable[StatementOp, Statement] =
      new Embeddable[StatementOp, Statement] {
        def embed[A](j: Statement, fa: FF[StatementOp, A]): Embedded.Statement[A] = Embedded.Statement(j, fa)
      }

    // Interface for a natural transformation StatementOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (StatementOp ~> F) {
      final def apply[A](fa: StatementOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](f: Statement => A): F[A]
      def embed[A](e: Embedded[A]): F[A]
      def raiseError[A](e: Throwable): F[A]
      def handleErrorWith[A](fa: StatementIO[A])(f: Throwable => StatementIO[A]): F[A]
      def monotonic: F[FiniteDuration]
      def realTime: F[FiniteDuration]
      def delay[A](thunk: => A): F[A]
      def suspend[A](hint: Sync.Type)(thunk: => A): F[A]
      def forceR[A, B](fa: StatementIO[A])(fb: StatementIO[B]): F[B]
      def uncancelable[A](body: Poll[StatementIO] => StatementIO[A]): F[A]
      def poll[A](poll: Any, fa: StatementIO[A]): F[A]
      def canceled: F[Unit]
      def onCancel[A](fa: StatementIO[A], fin: StatementIO[Unit]): F[A]
      def fromFuture[A](fut: StatementIO[Future[A]]): F[A]
      def fromFutureCancelable[A](fut: StatementIO[(Future[A], StatementIO[Unit])]): F[A]
      def cancelable[A](fa: StatementIO[A], fin: StatementIO[Unit]): F[A]
      def performLogging(event: LogEvent): F[Unit]

      // Statement
      def addBatch(a: String): F[Unit]
      def cancel: F[Unit]
      def clearBatch: F[Unit]
      def clearWarnings: F[Unit]
      def close: F[Unit]
      def closeOnCompletion: F[Unit]
      def enquoteIdentifier(a: String, b: Boolean): F[String]
      def enquoteLiteral(a: String): F[String]
      def enquoteNCharLiteral(a: String): F[String]
      def execute(a: String): F[Boolean]
      def execute(a: String, b: Array[Int]): F[Boolean]
      def execute(a: String, b: Array[String]): F[Boolean]
      def execute(a: String, b: Int): F[Boolean]
      def executeBatch: F[Array[Int]]
      def executeLargeBatch: F[Array[Long]]
      def executeLargeUpdate(a: String): F[Long]
      def executeLargeUpdate(a: String, b: Array[Int]): F[Long]
      def executeLargeUpdate(a: String, b: Array[String]): F[Long]
      def executeLargeUpdate(a: String, b: Int): F[Long]
      def executeQuery(a: String): F[ResultSet]
      def executeUpdate(a: String): F[Int]
      def executeUpdate(a: String, b: Array[Int]): F[Int]
      def executeUpdate(a: String, b: Array[String]): F[Int]
      def executeUpdate(a: String, b: Int): F[Int]
      def getConnection: F[Connection]
      def getFetchDirection: F[Int]
      def getFetchSize: F[Int]
      def getGeneratedKeys: F[ResultSet]
      def getLargeMaxRows: F[Long]
      def getLargeUpdateCount: F[Long]
      def getMaxFieldSize: F[Int]
      def getMaxRows: F[Int]
      def getMoreResults: F[Boolean]
      def getMoreResults(a: Int): F[Boolean]
      def getQueryTimeout: F[Int]
      def getResultSet: F[ResultSet]
      def getResultSetConcurrency: F[Int]
      def getResultSetHoldability: F[Int]
      def getResultSetType: F[Int]
      def getUpdateCount: F[Int]
      def getWarnings: F[SQLWarning]
      def isCloseOnCompletion: F[Boolean]
      def isClosed: F[Boolean]
      def isPoolable: F[Boolean]
      def isSimpleIdentifier(a: String): F[Boolean]
      def isWrapperFor(a: Class[?]): F[Boolean]
      def setCursorName(a: String): F[Unit]
      def setEscapeProcessing(a: Boolean): F[Unit]
      def setFetchDirection(a: Int): F[Unit]
      def setFetchSize(a: Int): F[Unit]
      def setLargeMaxRows(a: Long): F[Unit]
      def setMaxFieldSize(a: Int): F[Unit]
      def setMaxRows(a: Int): F[Unit]
      def setPoolable(a: Boolean): F[Unit]
      def setQueryTimeout(a: Int): F[Unit]
      def unwrap[T](a: Class[T]): F[T]

    }

    // Common operations for all algebras.
    final case class Raw[A](f: Statement => A) extends StatementOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(f)
    }
    final case class Embed[A](e: Embedded[A]) extends StatementOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class RaiseError[A](e: Throwable) extends StatementOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raiseError(e)
    }
    final case class HandleErrorWith[A](fa: StatementIO[A], f: Throwable => StatementIO[A]) extends StatementOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa)(f)
    }
    case object Monotonic extends StatementOp[FiniteDuration] {
      def visit[F[_]](v: Visitor[F]) = v.monotonic
    }
    case object Realtime extends StatementOp[FiniteDuration] {
      def visit[F[_]](v: Visitor[F]) = v.realTime
    }
    case class Suspend[A](hint: Sync.Type, thunk: () => A) extends StatementOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.suspend(hint)(thunk())
    }
    case class ForceR[A, B](fa: StatementIO[A], fb: StatementIO[B]) extends StatementOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.forceR(fa)(fb)
    }
    case class Uncancelable[A](body: Poll[StatementIO] => StatementIO[A]) extends StatementOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.uncancelable(body)
    }
    case class Poll1[A](poll: Any, fa: StatementIO[A]) extends StatementOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.poll(poll, fa)
    }
    case object Canceled extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.canceled
    }
    case class OnCancel[A](fa: StatementIO[A], fin: StatementIO[Unit]) extends StatementOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.onCancel(fa, fin)
    }
    case class FromFuture[A](fut: StatementIO[Future[A]]) extends StatementOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.fromFuture(fut)
    }
    case class FromFutureCancelable[A](fut: StatementIO[(Future[A], StatementIO[Unit])]) extends StatementOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.fromFutureCancelable(fut)
    }
    case class Cancelable[A](fa: StatementIO[A], fin: StatementIO[Unit]) extends StatementOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.cancelable(fa, fin)
    }
    case class PerformLogging(event: LogEvent) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.performLogging(event)
    }

    // Statement-specific operations.
    final case class AddBatch(a: String) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.addBatch(a)
    }
    case object Cancel extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.cancel
    }
    case object ClearBatch extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.clearBatch
    }
    case object ClearWarnings extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.clearWarnings
    }
    case object Close extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.close
    }
    case object CloseOnCompletion extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.closeOnCompletion
    }
    final case class EnquoteIdentifier(a: String, b: Boolean) extends StatementOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.enquoteIdentifier(a, b)
    }
    final case class EnquoteLiteral(a: String) extends StatementOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.enquoteLiteral(a)
    }
    final case class EnquoteNCharLiteral(a: String) extends StatementOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.enquoteNCharLiteral(a)
    }
    final case class Execute(a: String) extends StatementOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.execute(a)
    }
    final case class Execute1(a: String, b: Array[Int]) extends StatementOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.execute(a, b)
    }
    final case class Execute2(a: String, b: Array[String]) extends StatementOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.execute(a, b)
    }
    final case class Execute3(a: String, b: Int) extends StatementOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.execute(a, b)
    }
    case object ExecuteBatch extends StatementOp[Array[Int]] {
      def visit[F[_]](v: Visitor[F]) = v.executeBatch
    }
    case object ExecuteLargeBatch extends StatementOp[Array[Long]] {
      def visit[F[_]](v: Visitor[F]) = v.executeLargeBatch
    }
    final case class ExecuteLargeUpdate(a: String) extends StatementOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.executeLargeUpdate(a)
    }
    final case class ExecuteLargeUpdate1(a: String, b: Array[Int]) extends StatementOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.executeLargeUpdate(a, b)
    }
    final case class ExecuteLargeUpdate2(a: String, b: Array[String]) extends StatementOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.executeLargeUpdate(a, b)
    }
    final case class ExecuteLargeUpdate3(a: String, b: Int) extends StatementOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.executeLargeUpdate(a, b)
    }
    final case class ExecuteQuery(a: String) extends StatementOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.executeQuery(a)
    }
    final case class ExecuteUpdate(a: String) extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.executeUpdate(a)
    }
    final case class ExecuteUpdate1(a: String, b: Array[Int]) extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.executeUpdate(a, b)
    }
    final case class ExecuteUpdate2(a: String, b: Array[String]) extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.executeUpdate(a, b)
    }
    final case class ExecuteUpdate3(a: String, b: Int) extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.executeUpdate(a, b)
    }
    case object GetConnection extends StatementOp[Connection] {
      def visit[F[_]](v: Visitor[F]) = v.getConnection
    }
    case object GetFetchDirection extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getFetchDirection
    }
    case object GetFetchSize extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getFetchSize
    }
    case object GetGeneratedKeys extends StatementOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getGeneratedKeys
    }
    case object GetLargeMaxRows extends StatementOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.getLargeMaxRows
    }
    case object GetLargeUpdateCount extends StatementOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.getLargeUpdateCount
    }
    case object GetMaxFieldSize extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxFieldSize
    }
    case object GetMaxRows extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxRows
    }
    case object GetMoreResults extends StatementOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.getMoreResults
    }
    final case class GetMoreResults1(a: Int) extends StatementOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.getMoreResults(a)
    }
    case object GetQueryTimeout extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getQueryTimeout
    }
    case object GetResultSet extends StatementOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getResultSet
    }
    case object GetResultSetConcurrency extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getResultSetConcurrency
    }
    case object GetResultSetHoldability extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getResultSetHoldability
    }
    case object GetResultSetType extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getResultSetType
    }
    case object GetUpdateCount extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getUpdateCount
    }
    case object GetWarnings extends StatementOp[SQLWarning] {
      def visit[F[_]](v: Visitor[F]) = v.getWarnings
    }
    case object IsCloseOnCompletion extends StatementOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isCloseOnCompletion
    }
    case object IsClosed extends StatementOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isClosed
    }
    case object IsPoolable extends StatementOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isPoolable
    }
    final case class IsSimpleIdentifier(a: String) extends StatementOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isSimpleIdentifier(a)
    }
    final case class IsWrapperFor(a: Class[?]) extends StatementOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isWrapperFor(a)
    }
    final case class SetCursorName(a: String) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setCursorName(a)
    }
    final case class SetEscapeProcessing(a: Boolean) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setEscapeProcessing(a)
    }
    final case class SetFetchDirection(a: Int) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setFetchDirection(a)
    }
    final case class SetFetchSize(a: Int) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setFetchSize(a)
    }
    final case class SetLargeMaxRows(a: Long) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setLargeMaxRows(a)
    }
    final case class SetMaxFieldSize(a: Int) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setMaxFieldSize(a)
    }
    final case class SetMaxRows(a: Int) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setMaxRows(a)
    }
    final case class SetPoolable(a: Boolean) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setPoolable(a)
    }
    final case class SetQueryTimeout(a: Int) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setQueryTimeout(a)
    }
    final case class Unwrap[T](a: Class[T]) extends StatementOp[T] {
      def visit[F[_]](v: Visitor[F]) = v.unwrap(a)
    }

  }
  import StatementOp.*

  // Smart constructors for operations common to all algebras.
  val unit: StatementIO[Unit] = FF.pure[StatementOp, Unit](())
  def pure[A](a: A): StatementIO[A] = FF.pure[StatementOp, A](a)
  def raw[A](f: Statement => A): StatementIO[A] = FF.liftF(Raw(f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[StatementOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def raiseError[A](err: Throwable): StatementIO[A] = FF.liftF[StatementOp, A](RaiseError(err))
  def handleErrorWith[A](fa: StatementIO[A])(f: Throwable => StatementIO[A]): StatementIO[A] = FF.liftF[StatementOp, A](HandleErrorWith(fa, f))
  val monotonic = FF.liftF[StatementOp, FiniteDuration](Monotonic)
  val realtime = FF.liftF[StatementOp, FiniteDuration](Realtime)
  def delay[A](thunk: => A) = FF.liftF[StatementOp, A](Suspend(Sync.Type.Delay, () => thunk))
  def suspend[A](hint: Sync.Type)(thunk: => A) = FF.liftF[StatementOp, A](Suspend(hint, () => thunk))
  def forceR[A, B](fa: StatementIO[A])(fb: StatementIO[B]) = FF.liftF[StatementOp, B](ForceR(fa, fb))
  def uncancelable[A](body: Poll[StatementIO] => StatementIO[A]) = FF.liftF[StatementOp, A](Uncancelable(body))
  def capturePoll[M[_]](mpoll: Poll[M]) = new Poll[StatementIO] {
    def apply[A](fa: StatementIO[A]) = FF.liftF[StatementOp, A](Poll1(mpoll, fa))
  }
  val canceled = FF.liftF[StatementOp, Unit](Canceled)
  def onCancel[A](fa: StatementIO[A], fin: StatementIO[Unit]) = FF.liftF[StatementOp, A](OnCancel(fa, fin))
  def fromFuture[A](fut: StatementIO[Future[A]]) = FF.liftF[StatementOp, A](FromFuture(fut))
  def fromFutureCancelable[A](fut: StatementIO[(Future[A], StatementIO[Unit])]) = FF.liftF[StatementOp, A](FromFutureCancelable(fut))
  def cancelable[A](fa: StatementIO[A], fin: StatementIO[Unit]) = FF.liftF[StatementOp, A](Cancelable(fa, fin))
  def performLogging(event: LogEvent) = FF.liftF[StatementOp, Unit](PerformLogging(event))

  // Smart constructors for Statement-specific operations.
  def addBatch(a: String): StatementIO[Unit] = FF.liftF(AddBatch(a))
  val cancel: StatementIO[Unit] = FF.liftF(Cancel)
  val clearBatch: StatementIO[Unit] = FF.liftF(ClearBatch)
  val clearWarnings: StatementIO[Unit] = FF.liftF(ClearWarnings)
  val close: StatementIO[Unit] = FF.liftF(Close)
  val closeOnCompletion: StatementIO[Unit] = FF.liftF(CloseOnCompletion)
  def enquoteIdentifier(a: String, b: Boolean): StatementIO[String] = FF.liftF(EnquoteIdentifier(a, b))
  def enquoteLiteral(a: String): StatementIO[String] = FF.liftF(EnquoteLiteral(a))
  def enquoteNCharLiteral(a: String): StatementIO[String] = FF.liftF(EnquoteNCharLiteral(a))
  def execute(a: String): StatementIO[Boolean] = FF.liftF(Execute(a))
  def execute(a: String, b: Array[Int]): StatementIO[Boolean] = FF.liftF(Execute1(a, b))
  def execute(a: String, b: Array[String]): StatementIO[Boolean] = FF.liftF(Execute2(a, b))
  def execute(a: String, b: Int): StatementIO[Boolean] = FF.liftF(Execute3(a, b))
  val executeBatch: StatementIO[Array[Int]] = FF.liftF(ExecuteBatch)
  val executeLargeBatch: StatementIO[Array[Long]] = FF.liftF(ExecuteLargeBatch)
  def executeLargeUpdate(a: String): StatementIO[Long] = FF.liftF(ExecuteLargeUpdate(a))
  def executeLargeUpdate(a: String, b: Array[Int]): StatementIO[Long] = FF.liftF(ExecuteLargeUpdate1(a, b))
  def executeLargeUpdate(a: String, b: Array[String]): StatementIO[Long] = FF.liftF(ExecuteLargeUpdate2(a, b))
  def executeLargeUpdate(a: String, b: Int): StatementIO[Long] = FF.liftF(ExecuteLargeUpdate3(a, b))
  def executeQuery(a: String): StatementIO[ResultSet] = FF.liftF(ExecuteQuery(a))
  def executeUpdate(a: String): StatementIO[Int] = FF.liftF(ExecuteUpdate(a))
  def executeUpdate(a: String, b: Array[Int]): StatementIO[Int] = FF.liftF(ExecuteUpdate1(a, b))
  def executeUpdate(a: String, b: Array[String]): StatementIO[Int] = FF.liftF(ExecuteUpdate2(a, b))
  def executeUpdate(a: String, b: Int): StatementIO[Int] = FF.liftF(ExecuteUpdate3(a, b))
  val getConnection: StatementIO[Connection] = FF.liftF(GetConnection)
  val getFetchDirection: StatementIO[Int] = FF.liftF(GetFetchDirection)
  val getFetchSize: StatementIO[Int] = FF.liftF(GetFetchSize)
  val getGeneratedKeys: StatementIO[ResultSet] = FF.liftF(GetGeneratedKeys)
  val getLargeMaxRows: StatementIO[Long] = FF.liftF(GetLargeMaxRows)
  val getLargeUpdateCount: StatementIO[Long] = FF.liftF(GetLargeUpdateCount)
  val getMaxFieldSize: StatementIO[Int] = FF.liftF(GetMaxFieldSize)
  val getMaxRows: StatementIO[Int] = FF.liftF(GetMaxRows)
  val getMoreResults: StatementIO[Boolean] = FF.liftF(GetMoreResults)
  def getMoreResults(a: Int): StatementIO[Boolean] = FF.liftF(GetMoreResults1(a))
  val getQueryTimeout: StatementIO[Int] = FF.liftF(GetQueryTimeout)
  val getResultSet: StatementIO[ResultSet] = FF.liftF(GetResultSet)
  val getResultSetConcurrency: StatementIO[Int] = FF.liftF(GetResultSetConcurrency)
  val getResultSetHoldability: StatementIO[Int] = FF.liftF(GetResultSetHoldability)
  val getResultSetType: StatementIO[Int] = FF.liftF(GetResultSetType)
  val getUpdateCount: StatementIO[Int] = FF.liftF(GetUpdateCount)
  val getWarnings: StatementIO[SQLWarning] = FF.liftF(GetWarnings)
  val isCloseOnCompletion: StatementIO[Boolean] = FF.liftF(IsCloseOnCompletion)
  val isClosed: StatementIO[Boolean] = FF.liftF(IsClosed)
  val isPoolable: StatementIO[Boolean] = FF.liftF(IsPoolable)
  def isSimpleIdentifier(a: String): StatementIO[Boolean] = FF.liftF(IsSimpleIdentifier(a))
  def isWrapperFor(a: Class[?]): StatementIO[Boolean] = FF.liftF(IsWrapperFor(a))
  def setCursorName(a: String): StatementIO[Unit] = FF.liftF(SetCursorName(a))
  def setEscapeProcessing(a: Boolean): StatementIO[Unit] = FF.liftF(SetEscapeProcessing(a))
  def setFetchDirection(a: Int): StatementIO[Unit] = FF.liftF(SetFetchDirection(a))
  def setFetchSize(a: Int): StatementIO[Unit] = FF.liftF(SetFetchSize(a))
  def setLargeMaxRows(a: Long): StatementIO[Unit] = FF.liftF(SetLargeMaxRows(a))
  def setMaxFieldSize(a: Int): StatementIO[Unit] = FF.liftF(SetMaxFieldSize(a))
  def setMaxRows(a: Int): StatementIO[Unit] = FF.liftF(SetMaxRows(a))
  def setPoolable(a: Boolean): StatementIO[Unit] = FF.liftF(SetPoolable(a))
  def setQueryTimeout(a: Int): StatementIO[Unit] = FF.liftF(SetQueryTimeout(a))
  def unwrap[T](a: Class[T]): StatementIO[T] = FF.liftF(Unwrap(a))

  // Typeclass instances for StatementIO
  implicit val WeakAsyncStatementIO: WeakAsync[StatementIO] =
    new WeakAsync[StatementIO] {
      val monad = FF.catsFreeMonadForFree[StatementOp]
      override val applicative: Applicative[StatementIO] = monad
      override val rootCancelScope: CancelScope = CancelScope.Cancelable
      override def pure[A](x: A): StatementIO[A] = monad.pure(x)
      override def flatMap[A, B](fa: StatementIO[A])(f: A => StatementIO[B]): StatementIO[B] = monad.flatMap(fa)(f)
      override def tailRecM[A, B](a: A)(f: A => StatementIO[Either[A, B]]): StatementIO[B] = monad.tailRecM(a)(f)
      override def raiseError[A](e: Throwable): StatementIO[A] = module.raiseError(e)
      override def handleErrorWith[A](fa: StatementIO[A])(f: Throwable => StatementIO[A]): StatementIO[A] = module.handleErrorWith(fa)(f)
      override def monotonic: StatementIO[FiniteDuration] = module.monotonic
      override def realTime: StatementIO[FiniteDuration] = module.realtime
      override def suspend[A](hint: Sync.Type)(thunk: => A): StatementIO[A] = module.suspend(hint)(thunk)
      override def forceR[A, B](fa: StatementIO[A])(fb: StatementIO[B]): StatementIO[B] = module.forceR(fa)(fb)
      override def uncancelable[A](body: Poll[StatementIO] => StatementIO[A]): StatementIO[A] = module.uncancelable(body)
      override def canceled: StatementIO[Unit] = module.canceled
      override def onCancel[A](fa: StatementIO[A], fin: StatementIO[Unit]): StatementIO[A] = module.onCancel(fa, fin)
      override def fromFuture[A](fut: StatementIO[Future[A]]): StatementIO[A] = module.fromFuture(fut)
      override def fromFutureCancelable[A](fut: StatementIO[(Future[A], StatementIO[Unit])]): StatementIO[A] = module.fromFutureCancelable(fut)
      override def cancelable[A](fa: StatementIO[A], fin: StatementIO[Unit]): StatementIO[A] = module.cancelable(fa, fin)
    }
    
  implicit def MonoidStatementIO[A : Monoid]: Monoid[StatementIO[A]] = new Monoid[StatementIO[A]] {
    override def empty: StatementIO[A] = Applicative[StatementIO].pure(Monoid[A].empty)
    override def combine(x: StatementIO[A], y: StatementIO[A]): StatementIO[A] =
      Applicative[StatementIO].product(x, y).map { case (x, y) => Monoid[A].combine(x, y) }
  }
 
  implicit def SemigroupStatementIO[A : Semigroup]: Semigroup[StatementIO[A]] = new Semigroup[StatementIO[A]] {
    override def combine(x: StatementIO[A], y: StatementIO[A]): StatementIO[A] =
      Applicative[StatementIO].product(x, y).map { case (x, y) => Semigroup[A].combine(x, y) }
  }
}

