// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.free

import cats.~>
import cats.effect.{ Async, ContextShift, ExitCase }
import cats.free.{ Free => FF } // alias because some algebras have an op called Free
import scala.concurrent.ExecutionContext
import com.github.ghik.silencer.silent
import io.chrisdavenport.log4cats.MessageLogger

import java.lang.Class
import java.lang.String
import java.sql.Connection
import java.sql.ResultSet
import java.sql.SQLWarning
import java.sql.Statement

@silent("deprecated")
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
        def embed[A](j: Statement, fa: FF[StatementOp, A]) = Embedded.Statement(j, fa)
      }

    // Interface for a natural transformation StatementOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (StatementOp ~> F) {
      final def apply[A](fa: StatementOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](message: => String, f: Statement => A): F[A]
      def embed[A](e: Embedded[A]): F[A]

      // Async
      def delay[A](a: () => A): F[A]
      def handleErrorWith[A](fa: StatementIO[A], f: Throwable => StatementIO[A]): F[A]
      def raiseError[A](e: Throwable): F[A]
      def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]
      def asyncF[A](k: (Either[Throwable, A] => Unit) => StatementIO[Unit]): F[A]
      def bracketCase[A, B](acquire: StatementIO[A])(use: A => StatementIO[B])(release: (A, ExitCase[Throwable]) => StatementIO[Unit]): F[B]
      def shift: F[Unit]
      def evalOn[A](ec: ExecutionContext)(fa: StatementIO[A]): F[A]

      // Logger
      def error(message: => String): F[Unit]
      def warn(message: => String): F[Unit]
      def info(message: => String): F[Unit]
      def debug(message: => String): F[Unit]
      def trace(message: => String): F[Unit]

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
      def isWrapperFor(a: Class[_]): F[Boolean]
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
    final case class Raw[A](message: () => String, f: Statement => A) extends StatementOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(message(), f)
    }
    final case class Embed[A](e: Embedded[A]) extends StatementOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class Delay[A](a: () => A) extends StatementOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.delay(a)
    }
    final case class HandleErrorWith[A](fa: StatementIO[A], f: Throwable => StatementIO[A]) extends StatementOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa, f)
    }
    final case class RaiseError[A](e: Throwable) extends StatementOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raiseError(e)
    }
    final case class Async1[A](k: (Either[Throwable, A] => Unit) => Unit) extends StatementOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.async(k)
    }
    final case class AsyncF[A](k: (Either[Throwable, A] => Unit) => StatementIO[Unit]) extends StatementOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.asyncF(k)
    }
    final case class BracketCase[A, B](acquire: StatementIO[A], use: A => StatementIO[B], release: (A, ExitCase[Throwable]) => StatementIO[Unit]) extends StatementOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.bracketCase(acquire)(use)(release)
    }
    final case object Shift extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.shift
    }
    final case class EvalOn[A](ec: ExecutionContext, fa: StatementIO[A]) extends StatementOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.evalOn(ec)(fa)
    }
    final case class LogError(message: () => String) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.error(message()): F[Unit]
    }
    final case class LogWarn(message: () => String) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.warn(message()): F[Unit]
    }
    final case class LogInfo(message: () => String) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.info(message()): F[Unit]
    }
    final case class LogDebug(message: () => String) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.debug(message()): F[Unit]
    }
    final case class LogTrace(message: () => String) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.trace(message()): F[Unit]
    }

    // Statement-specific operations.
    final case class  AddBatch(a: String) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.addBatch(a)
    }
    final case object Cancel extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.cancel
    }
    final case object ClearBatch extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.clearBatch
    }
    final case object ClearWarnings extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.clearWarnings
    }
    final case object Close extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.close
    }
    final case object CloseOnCompletion extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.closeOnCompletion
    }
    final case class  EnquoteIdentifier(a: String, b: Boolean) extends StatementOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.enquoteIdentifier(a, b)
    }
    final case class  EnquoteLiteral(a: String) extends StatementOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.enquoteLiteral(a)
    }
    final case class  EnquoteNCharLiteral(a: String) extends StatementOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.enquoteNCharLiteral(a)
    }
    final case class  Execute(a: String) extends StatementOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.execute(a)
    }
    final case class  Execute1(a: String, b: Array[Int]) extends StatementOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.execute(a, b)
    }
    final case class  Execute2(a: String, b: Array[String]) extends StatementOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.execute(a, b)
    }
    final case class  Execute3(a: String, b: Int) extends StatementOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.execute(a, b)
    }
    final case object ExecuteBatch extends StatementOp[Array[Int]] {
      def visit[F[_]](v: Visitor[F]) = v.executeBatch
    }
    final case object ExecuteLargeBatch extends StatementOp[Array[Long]] {
      def visit[F[_]](v: Visitor[F]) = v.executeLargeBatch
    }
    final case class  ExecuteLargeUpdate(a: String) extends StatementOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.executeLargeUpdate(a)
    }
    final case class  ExecuteLargeUpdate1(a: String, b: Array[Int]) extends StatementOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.executeLargeUpdate(a, b)
    }
    final case class  ExecuteLargeUpdate2(a: String, b: Array[String]) extends StatementOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.executeLargeUpdate(a, b)
    }
    final case class  ExecuteLargeUpdate3(a: String, b: Int) extends StatementOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.executeLargeUpdate(a, b)
    }
    final case class  ExecuteQuery(a: String) extends StatementOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.executeQuery(a)
    }
    final case class  ExecuteUpdate(a: String) extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.executeUpdate(a)
    }
    final case class  ExecuteUpdate1(a: String, b: Array[Int]) extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.executeUpdate(a, b)
    }
    final case class  ExecuteUpdate2(a: String, b: Array[String]) extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.executeUpdate(a, b)
    }
    final case class  ExecuteUpdate3(a: String, b: Int) extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.executeUpdate(a, b)
    }
    final case object GetConnection extends StatementOp[Connection] {
      def visit[F[_]](v: Visitor[F]) = v.getConnection
    }
    final case object GetFetchDirection extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getFetchDirection
    }
    final case object GetFetchSize extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getFetchSize
    }
    final case object GetGeneratedKeys extends StatementOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getGeneratedKeys
    }
    final case object GetLargeMaxRows extends StatementOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.getLargeMaxRows
    }
    final case object GetLargeUpdateCount extends StatementOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.getLargeUpdateCount
    }
    final case object GetMaxFieldSize extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxFieldSize
    }
    final case object GetMaxRows extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxRows
    }
    final case object GetMoreResults extends StatementOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.getMoreResults
    }
    final case class  GetMoreResults1(a: Int) extends StatementOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.getMoreResults(a)
    }
    final case object GetQueryTimeout extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getQueryTimeout
    }
    final case object GetResultSet extends StatementOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getResultSet
    }
    final case object GetResultSetConcurrency extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getResultSetConcurrency
    }
    final case object GetResultSetHoldability extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getResultSetHoldability
    }
    final case object GetResultSetType extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getResultSetType
    }
    final case object GetUpdateCount extends StatementOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getUpdateCount
    }
    final case object GetWarnings extends StatementOp[SQLWarning] {
      def visit[F[_]](v: Visitor[F]) = v.getWarnings
    }
    final case object IsCloseOnCompletion extends StatementOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isCloseOnCompletion
    }
    final case object IsClosed extends StatementOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isClosed
    }
    final case object IsPoolable extends StatementOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isPoolable
    }
    final case class  IsSimpleIdentifier(a: String) extends StatementOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isSimpleIdentifier(a)
    }
    final case class  IsWrapperFor(a: Class[_]) extends StatementOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isWrapperFor(a)
    }
    final case class  SetCursorName(a: String) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setCursorName(a)
    }
    final case class  SetEscapeProcessing(a: Boolean) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setEscapeProcessing(a)
    }
    final case class  SetFetchDirection(a: Int) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setFetchDirection(a)
    }
    final case class  SetFetchSize(a: Int) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setFetchSize(a)
    }
    final case class  SetLargeMaxRows(a: Long) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setLargeMaxRows(a)
    }
    final case class  SetMaxFieldSize(a: Int) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setMaxFieldSize(a)
    }
    final case class  SetMaxRows(a: Int) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setMaxRows(a)
    }
    final case class  SetPoolable(a: Boolean) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setPoolable(a)
    }
    final case class  SetQueryTimeout(a: Int) extends StatementOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setQueryTimeout(a)
    }
    final case class  Unwrap[T](a: Class[T]) extends StatementOp[T] {
      def visit[F[_]](v: Visitor[F]) = v.unwrap(a)
    }

  }
  import StatementOp._

  // Smart constructors for operations common to all algebras.
  val unit: StatementIO[Unit] = FF.pure[StatementOp, Unit](())
  def pure[A](a: A): StatementIO[A] = FF.pure[StatementOp, A](a)
  def raw[A](message: => String)(f: Statement => A): StatementIO[A] = FF.liftF(Raw(() => message, f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[StatementOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def delay[A](a: => A): StatementIO[A] = FF.liftF(Delay(() => a))
  def handleErrorWith[A](fa: StatementIO[A], f: Throwable => StatementIO[A]): StatementIO[A] = FF.liftF[StatementOp, A](HandleErrorWith(fa, f))
  def raiseError[A](err: Throwable): StatementIO[A] = FF.liftF[StatementOp, A](RaiseError(err))
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): StatementIO[A] = FF.liftF[StatementOp, A](Async1(k))
  def asyncF[A](k: (Either[Throwable, A] => Unit) => StatementIO[Unit]): StatementIO[A] = FF.liftF[StatementOp, A](AsyncF(k))
  def bracketCase[A, B](acquire: StatementIO[A])(use: A => StatementIO[B])(release: (A, ExitCase[Throwable]) => StatementIO[Unit]): StatementIO[B] = FF.liftF[StatementOp, B](BracketCase(acquire, use, release))
  val shift: StatementIO[Unit] = FF.liftF[StatementOp, Unit](Shift)
  def evalOn[A](ec: ExecutionContext)(fa: StatementIO[A]) = FF.liftF[StatementOp, A](EvalOn(ec, fa))

  // Logger
  def error(message: => String): StatementIO[Unit] = FF.liftF[StatementOp, Unit](LogError(() => message))
  def warn(message: => String): StatementIO[Unit] = FF.liftF[StatementOp, Unit](LogWarn(() => message))
  def info(message: => String): StatementIO[Unit] = FF.liftF[StatementOp, Unit](LogInfo(() => message))
  def debug(message: => String): StatementIO[Unit] = FF.liftF[StatementOp, Unit](LogDebug(() => message))
  def trace(message: => String): StatementIO[Unit] = FF.liftF[StatementOp, Unit](LogTrace(() => message))

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
  def isWrapperFor(a: Class[_]): StatementIO[Boolean] = FF.liftF(IsWrapperFor(a))
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

  // StatementIO is an Async
  implicit val AsyncStatementIO: Async[StatementIO] =
    new Async[StatementIO] {
      val asyncM = FF.catsFreeMonadForFree[StatementOp]
      def bracketCase[A, B](acquire: StatementIO[A])(use: A => StatementIO[B])(release: (A, ExitCase[Throwable]) => StatementIO[Unit]): StatementIO[B] = module.bracketCase(acquire)(use)(release)
      def pure[A](x: A): StatementIO[A] = asyncM.pure(x)
      def handleErrorWith[A](fa: StatementIO[A])(f: Throwable => StatementIO[A]): StatementIO[A] = module.handleErrorWith(fa, f)
      def raiseError[A](e: Throwable): StatementIO[A] = module.raiseError(e)
      def async[A](k: (Either[Throwable,A] => Unit) => Unit): StatementIO[A] = module.async(k)
      def asyncF[A](k: (Either[Throwable,A] => Unit) => StatementIO[Unit]): StatementIO[A] = module.asyncF(k)
      def flatMap[A, B](fa: StatementIO[A])(f: A => StatementIO[B]): StatementIO[B] = asyncM.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => StatementIO[Either[A, B]]): StatementIO[B] = asyncM.tailRecM(a)(f)
      def suspend[A](thunk: => StatementIO[A]): StatementIO[A] = asyncM.flatten(module.delay(thunk))
    }

  // StatementIO is a ContextShift
  implicit val ContextShiftStatementIO: ContextShift[StatementIO] =
    new ContextShift[StatementIO] {
      def shift: StatementIO[Unit] = module.shift
      def evalOn[A](ec: ExecutionContext)(fa: StatementIO[A]) = module.evalOn(ec)(fa)
    }

  // StatementIO is a MessageLogger
  implicit val MessageLoggerStatementIO: MessageLogger[StatementIO] =
    new MessageLogger[StatementIO] {
      def error(message: => String): StatementIO[Unit] =  module.error(message)
      def warn(message: => String): StatementIO[Unit] = module.warn(message)
      def info(message: => String): StatementIO[Unit] = module.info(message)
      def debug(message: => String): StatementIO[Unit] =  module.debug(message)
      def trace(message: => String): StatementIO[Unit] =  module.trace(message)
    }
}

