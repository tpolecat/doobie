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
import java.sql.Blob
import java.sql.CallableStatement
import java.sql.Clob
import java.sql.Connection
import java.sql.DatabaseMetaData
import java.sql.NClob
import java.sql.PreparedStatement
import java.sql.SQLWarning
import java.sql.SQLXML
import java.sql.Savepoint
import java.sql.ShardingKey
import java.sql.Statement
import java.sql.Struct
import java.sql.{ Array as SqlArray }
import java.util.Properties
import java.util.concurrent.Executor

// This file is Auto-generated using FreeGen2.scala
object connection { module =>

  // Algebra of operations for Connection. Each accepts a visitor as an alternative to pattern-matching.
  sealed trait ConnectionOp[A] {
    def visit[F[_]](v: ConnectionOp.Visitor[F]): F[A]
  }

  // Free monad over ConnectionOp.
  type ConnectionIO[A] = FF[ConnectionOp, A]

  // Module of instances and constructors of ConnectionOp.
  object ConnectionOp {

    // Given a Connection we can embed a ConnectionIO program in any algebra that understands embedding.
    implicit val ConnectionOpEmbeddable: Embeddable[ConnectionOp, Connection] =
      new Embeddable[ConnectionOp, Connection] {
        def embed[A](j: Connection, fa: FF[ConnectionOp, A]): Embedded.Connection[A] = Embedded.Connection(j, fa)
      }

    // Interface for a natural transformation ConnectionOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (ConnectionOp ~> F) {
      final def apply[A](fa: ConnectionOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](f: Connection => A): F[A]
      def embed[A](e: Embedded[A]): F[A]
      def raiseError[A](e: Throwable): F[A]
      def handleErrorWith[A](fa: ConnectionIO[A])(f: Throwable => ConnectionIO[A]): F[A]
      def monotonic: F[FiniteDuration]
      def realTime: F[FiniteDuration]
      def delay[A](thunk: => A): F[A]
      def suspend[A](hint: Sync.Type)(thunk: => A): F[A]
      def forceR[A, B](fa: ConnectionIO[A])(fb: ConnectionIO[B]): F[B]
      def uncancelable[A](body: Poll[ConnectionIO] => ConnectionIO[A]): F[A]
      def poll[A](poll: Any, fa: ConnectionIO[A]): F[A]
      def canceled: F[Unit]
      def onCancel[A](fa: ConnectionIO[A], fin: ConnectionIO[Unit]): F[A]
      def fromFuture[A](fut: ConnectionIO[Future[A]]): F[A]
      def fromFutureCancelable[A](fut: ConnectionIO[(Future[A], ConnectionIO[Unit])]): F[A]
      def cancelable[A](fa: ConnectionIO[A], fin: ConnectionIO[Unit]): F[A]
      def performLogging(event: LogEvent): F[Unit]

      // Connection
      def abort(a: Executor): F[Unit]
      def beginRequest: F[Unit]
      def clearWarnings: F[Unit]
      def close: F[Unit]
      def commit: F[Unit]
      def createArrayOf(a: String, b: Array[AnyRef]): F[SqlArray]
      def createBlob: F[Blob]
      def createClob: F[Clob]
      def createNClob: F[NClob]
      def createSQLXML: F[SQLXML]
      def createStatement: F[Statement]
      def createStatement(a: Int, b: Int): F[Statement]
      def createStatement(a: Int, b: Int, c: Int): F[Statement]
      def createStruct(a: String, b: Array[AnyRef]): F[Struct]
      def endRequest: F[Unit]
      def getAutoCommit: F[Boolean]
      def getCatalog: F[String]
      def getClientInfo: F[Properties]
      def getClientInfo(a: String): F[String]
      def getHoldability: F[Int]
      def getMetaData: F[DatabaseMetaData]
      def getNetworkTimeout: F[Int]
      def getSchema: F[String]
      def getTransactionIsolation: F[Int]
      def getTypeMap: F[java.util.Map[String, Class[?]]]
      def getWarnings: F[SQLWarning]
      def isClosed: F[Boolean]
      def isReadOnly: F[Boolean]
      def isValid(a: Int): F[Boolean]
      def isWrapperFor(a: Class[?]): F[Boolean]
      def nativeSQL(a: String): F[String]
      def prepareCall(a: String): F[CallableStatement]
      def prepareCall(a: String, b: Int, c: Int): F[CallableStatement]
      def prepareCall(a: String, b: Int, c: Int, d: Int): F[CallableStatement]
      def prepareStatement(a: String): F[PreparedStatement]
      def prepareStatement(a: String, b: Array[Int]): F[PreparedStatement]
      def prepareStatement(a: String, b: Array[String]): F[PreparedStatement]
      def prepareStatement(a: String, b: Int): F[PreparedStatement]
      def prepareStatement(a: String, b: Int, c: Int): F[PreparedStatement]
      def prepareStatement(a: String, b: Int, c: Int, d: Int): F[PreparedStatement]
      def releaseSavepoint(a: Savepoint): F[Unit]
      def rollback: F[Unit]
      def rollback(a: Savepoint): F[Unit]
      def setAutoCommit(a: Boolean): F[Unit]
      def setCatalog(a: String): F[Unit]
      def setClientInfo(a: Properties): F[Unit]
      def setClientInfo(a: String, b: String): F[Unit]
      def setHoldability(a: Int): F[Unit]
      def setNetworkTimeout(a: Executor, b: Int): F[Unit]
      def setReadOnly(a: Boolean): F[Unit]
      def setSavepoint: F[Savepoint]
      def setSavepoint(a: String): F[Savepoint]
      def setSchema(a: String): F[Unit]
      def setShardingKey(a: ShardingKey): F[Unit]
      def setShardingKey(a: ShardingKey, b: ShardingKey): F[Unit]
      def setShardingKeyIfValid(a: ShardingKey, b: Int): F[Boolean]
      def setShardingKeyIfValid(a: ShardingKey, b: ShardingKey, c: Int): F[Boolean]
      def setTransactionIsolation(a: Int): F[Unit]
      def setTypeMap(a: java.util.Map[String, Class[?]]): F[Unit]
      def unwrap[T](a: Class[T]): F[T]

    }

    // Common operations for all algebras.
    final case class Raw[A](f: Connection => A) extends ConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(f)
    }
    final case class Embed[A](e: Embedded[A]) extends ConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class RaiseError[A](e: Throwable) extends ConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raiseError(e)
    }
    final case class HandleErrorWith[A](fa: ConnectionIO[A], f: Throwable => ConnectionIO[A]) extends ConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa)(f)
    }
    case object Monotonic extends ConnectionOp[FiniteDuration] {
      def visit[F[_]](v: Visitor[F]) = v.monotonic
    }
    case object Realtime extends ConnectionOp[FiniteDuration] {
      def visit[F[_]](v: Visitor[F]) = v.realTime
    }
    case class Suspend[A](hint: Sync.Type, thunk: () => A) extends ConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.suspend(hint)(thunk())
    }
    case class ForceR[A, B](fa: ConnectionIO[A], fb: ConnectionIO[B]) extends ConnectionOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.forceR(fa)(fb)
    }
    case class Uncancelable[A](body: Poll[ConnectionIO] => ConnectionIO[A]) extends ConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.uncancelable(body)
    }
    case class Poll1[A](poll: Any, fa: ConnectionIO[A]) extends ConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.poll(poll, fa)
    }
    case object Canceled extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.canceled
    }
    case class OnCancel[A](fa: ConnectionIO[A], fin: ConnectionIO[Unit]) extends ConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.onCancel(fa, fin)
    }
    case class FromFuture[A](fut: ConnectionIO[Future[A]]) extends ConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.fromFuture(fut)
    }
    case class FromFutureCancelable[A](fut: ConnectionIO[(Future[A], ConnectionIO[Unit])]) extends ConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.fromFutureCancelable(fut)
    }
    case class Cancelable[A](fa: ConnectionIO[A], fin: ConnectionIO[Unit]) extends ConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.cancelable(fa, fin)
    }
    case class PerformLogging(event: LogEvent) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.performLogging(event)
    }

    // Connection-specific operations.
    final case class Abort(a: Executor) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.abort(a)
    }
    case object BeginRequest extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.beginRequest
    }
    case object ClearWarnings extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.clearWarnings
    }
    case object Close extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.close
    }
    case object Commit extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.commit
    }
    final case class CreateArrayOf(a: String, b: Array[AnyRef]) extends ConnectionOp[SqlArray] {
      def visit[F[_]](v: Visitor[F]) = v.createArrayOf(a, b)
    }
    case object CreateBlob extends ConnectionOp[Blob] {
      def visit[F[_]](v: Visitor[F]) = v.createBlob
    }
    case object CreateClob extends ConnectionOp[Clob] {
      def visit[F[_]](v: Visitor[F]) = v.createClob
    }
    case object CreateNClob extends ConnectionOp[NClob] {
      def visit[F[_]](v: Visitor[F]) = v.createNClob
    }
    case object CreateSQLXML extends ConnectionOp[SQLXML] {
      def visit[F[_]](v: Visitor[F]) = v.createSQLXML
    }
    case object CreateStatement extends ConnectionOp[Statement] {
      def visit[F[_]](v: Visitor[F]) = v.createStatement
    }
    final case class CreateStatement1(a: Int, b: Int) extends ConnectionOp[Statement] {
      def visit[F[_]](v: Visitor[F]) = v.createStatement(a, b)
    }
    final case class CreateStatement2(a: Int, b: Int, c: Int) extends ConnectionOp[Statement] {
      def visit[F[_]](v: Visitor[F]) = v.createStatement(a, b, c)
    }
    final case class CreateStruct(a: String, b: Array[AnyRef]) extends ConnectionOp[Struct] {
      def visit[F[_]](v: Visitor[F]) = v.createStruct(a, b)
    }
    case object EndRequest extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.endRequest
    }
    case object GetAutoCommit extends ConnectionOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.getAutoCommit
    }
    case object GetCatalog extends ConnectionOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getCatalog
    }
    case object GetClientInfo extends ConnectionOp[Properties] {
      def visit[F[_]](v: Visitor[F]) = v.getClientInfo
    }
    final case class GetClientInfo1(a: String) extends ConnectionOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getClientInfo(a)
    }
    case object GetHoldability extends ConnectionOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getHoldability
    }
    case object GetMetaData extends ConnectionOp[DatabaseMetaData] {
      def visit[F[_]](v: Visitor[F]) = v.getMetaData
    }
    case object GetNetworkTimeout extends ConnectionOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getNetworkTimeout
    }
    case object GetSchema extends ConnectionOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getSchema
    }
    case object GetTransactionIsolation extends ConnectionOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getTransactionIsolation
    }
    case object GetTypeMap extends ConnectionOp[java.util.Map[String, Class[?]]] {
      def visit[F[_]](v: Visitor[F]) = v.getTypeMap
    }
    case object GetWarnings extends ConnectionOp[SQLWarning] {
      def visit[F[_]](v: Visitor[F]) = v.getWarnings
    }
    case object IsClosed extends ConnectionOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isClosed
    }
    case object IsReadOnly extends ConnectionOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isReadOnly
    }
    final case class IsValid(a: Int) extends ConnectionOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isValid(a)
    }
    final case class IsWrapperFor(a: Class[?]) extends ConnectionOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isWrapperFor(a)
    }
    final case class NativeSQL(a: String) extends ConnectionOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.nativeSQL(a)
    }
    final case class PrepareCall(a: String) extends ConnectionOp[CallableStatement] {
      def visit[F[_]](v: Visitor[F]) = v.prepareCall(a)
    }
    final case class PrepareCall1(a: String, b: Int, c: Int) extends ConnectionOp[CallableStatement] {
      def visit[F[_]](v: Visitor[F]) = v.prepareCall(a, b, c)
    }
    final case class PrepareCall2(a: String, b: Int, c: Int, d: Int) extends ConnectionOp[CallableStatement] {
      def visit[F[_]](v: Visitor[F]) = v.prepareCall(a, b, c, d)
    }
    final case class PrepareStatement(a: String) extends ConnectionOp[PreparedStatement] {
      def visit[F[_]](v: Visitor[F]) = v.prepareStatement(a)
    }
    final case class PrepareStatement1(a: String, b: Array[Int]) extends ConnectionOp[PreparedStatement] {
      def visit[F[_]](v: Visitor[F]) = v.prepareStatement(a, b)
    }
    final case class PrepareStatement2(a: String, b: Array[String]) extends ConnectionOp[PreparedStatement] {
      def visit[F[_]](v: Visitor[F]) = v.prepareStatement(a, b)
    }
    final case class PrepareStatement3(a: String, b: Int) extends ConnectionOp[PreparedStatement] {
      def visit[F[_]](v: Visitor[F]) = v.prepareStatement(a, b)
    }
    final case class PrepareStatement4(a: String, b: Int, c: Int) extends ConnectionOp[PreparedStatement] {
      def visit[F[_]](v: Visitor[F]) = v.prepareStatement(a, b, c)
    }
    final case class PrepareStatement5(a: String, b: Int, c: Int, d: Int) extends ConnectionOp[PreparedStatement] {
      def visit[F[_]](v: Visitor[F]) = v.prepareStatement(a, b, c, d)
    }
    final case class ReleaseSavepoint(a: Savepoint) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.releaseSavepoint(a)
    }
    case object Rollback extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.rollback
    }
    final case class Rollback1(a: Savepoint) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.rollback(a)
    }
    final case class SetAutoCommit(a: Boolean) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setAutoCommit(a)
    }
    final case class SetCatalog(a: String) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setCatalog(a)
    }
    final case class SetClientInfo(a: Properties) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setClientInfo(a)
    }
    final case class SetClientInfo1(a: String, b: String) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setClientInfo(a, b)
    }
    final case class SetHoldability(a: Int) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setHoldability(a)
    }
    final case class SetNetworkTimeout(a: Executor, b: Int) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setNetworkTimeout(a, b)
    }
    final case class SetReadOnly(a: Boolean) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setReadOnly(a)
    }
    case object SetSavepoint extends ConnectionOp[Savepoint] {
      def visit[F[_]](v: Visitor[F]) = v.setSavepoint
    }
    final case class SetSavepoint1(a: String) extends ConnectionOp[Savepoint] {
      def visit[F[_]](v: Visitor[F]) = v.setSavepoint(a)
    }
    final case class SetSchema(a: String) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setSchema(a)
    }
    final case class SetShardingKey(a: ShardingKey) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setShardingKey(a)
    }
    final case class SetShardingKey1(a: ShardingKey, b: ShardingKey) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setShardingKey(a, b)
    }
    final case class SetShardingKeyIfValid(a: ShardingKey, b: Int) extends ConnectionOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.setShardingKeyIfValid(a, b)
    }
    final case class SetShardingKeyIfValid1(a: ShardingKey, b: ShardingKey, c: Int) extends ConnectionOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.setShardingKeyIfValid(a, b, c)
    }
    final case class SetTransactionIsolation(a: Int) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setTransactionIsolation(a)
    }
    final case class SetTypeMap(a: java.util.Map[String, Class[?]]) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setTypeMap(a)
    }
    final case class Unwrap[T](a: Class[T]) extends ConnectionOp[T] {
      def visit[F[_]](v: Visitor[F]) = v.unwrap(a)
    }

  }
  import ConnectionOp.*

  // Smart constructors for operations common to all algebras.
  val unit: ConnectionIO[Unit] = FF.pure[ConnectionOp, Unit](())
  def pure[A](a: A): ConnectionIO[A] = FF.pure[ConnectionOp, A](a)
  def raw[A](f: Connection => A): ConnectionIO[A] = FF.liftF(Raw(f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[ConnectionOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def raiseError[A](err: Throwable): ConnectionIO[A] = FF.liftF[ConnectionOp, A](RaiseError(err))
  def handleErrorWith[A](fa: ConnectionIO[A])(f: Throwable => ConnectionIO[A]): ConnectionIO[A] = FF.liftF[ConnectionOp, A](HandleErrorWith(fa, f))
  val monotonic = FF.liftF[ConnectionOp, FiniteDuration](Monotonic)
  val realtime = FF.liftF[ConnectionOp, FiniteDuration](Realtime)
  def delay[A](thunk: => A) = FF.liftF[ConnectionOp, A](Suspend(Sync.Type.Delay, () => thunk))
  def suspend[A](hint: Sync.Type)(thunk: => A) = FF.liftF[ConnectionOp, A](Suspend(hint, () => thunk))
  def forceR[A, B](fa: ConnectionIO[A])(fb: ConnectionIO[B]) = FF.liftF[ConnectionOp, B](ForceR(fa, fb))
  def uncancelable[A](body: Poll[ConnectionIO] => ConnectionIO[A]) = FF.liftF[ConnectionOp, A](Uncancelable(body))
  def capturePoll[M[_]](mpoll: Poll[M]) = new Poll[ConnectionIO] {
    def apply[A](fa: ConnectionIO[A]) = FF.liftF[ConnectionOp, A](Poll1(mpoll, fa))
  }
  val canceled = FF.liftF[ConnectionOp, Unit](Canceled)
  def onCancel[A](fa: ConnectionIO[A], fin: ConnectionIO[Unit]) = FF.liftF[ConnectionOp, A](OnCancel(fa, fin))
  def fromFuture[A](fut: ConnectionIO[Future[A]]) = FF.liftF[ConnectionOp, A](FromFuture(fut))
  def fromFutureCancelable[A](fut: ConnectionIO[(Future[A], ConnectionIO[Unit])]) = FF.liftF[ConnectionOp, A](FromFutureCancelable(fut))
  def cancelable[A](fa: ConnectionIO[A], fin: ConnectionIO[Unit]) = FF.liftF[ConnectionOp, A](Cancelable(fa, fin))
  def performLogging(event: LogEvent) = FF.liftF[ConnectionOp, Unit](PerformLogging(event))

  // Smart constructors for Connection-specific operations.
  def abort(a: Executor): ConnectionIO[Unit] = FF.liftF(Abort(a))
  val beginRequest: ConnectionIO[Unit] = FF.liftF(BeginRequest)
  val clearWarnings: ConnectionIO[Unit] = FF.liftF(ClearWarnings)
  val close: ConnectionIO[Unit] = FF.liftF(Close)
  val commit: ConnectionIO[Unit] = FF.liftF(Commit)
  def createArrayOf(a: String, b: Array[AnyRef]): ConnectionIO[SqlArray] = FF.liftF(CreateArrayOf(a, b))
  val createBlob: ConnectionIO[Blob] = FF.liftF(CreateBlob)
  val createClob: ConnectionIO[Clob] = FF.liftF(CreateClob)
  val createNClob: ConnectionIO[NClob] = FF.liftF(CreateNClob)
  val createSQLXML: ConnectionIO[SQLXML] = FF.liftF(CreateSQLXML)
  val createStatement: ConnectionIO[Statement] = FF.liftF(CreateStatement)
  def createStatement(a: Int, b: Int): ConnectionIO[Statement] = FF.liftF(CreateStatement1(a, b))
  def createStatement(a: Int, b: Int, c: Int): ConnectionIO[Statement] = FF.liftF(CreateStatement2(a, b, c))
  def createStruct(a: String, b: Array[AnyRef]): ConnectionIO[Struct] = FF.liftF(CreateStruct(a, b))
  val endRequest: ConnectionIO[Unit] = FF.liftF(EndRequest)
  val getAutoCommit: ConnectionIO[Boolean] = FF.liftF(GetAutoCommit)
  val getCatalog: ConnectionIO[String] = FF.liftF(GetCatalog)
  val getClientInfo: ConnectionIO[Properties] = FF.liftF(GetClientInfo)
  def getClientInfo(a: String): ConnectionIO[String] = FF.liftF(GetClientInfo1(a))
  val getHoldability: ConnectionIO[Int] = FF.liftF(GetHoldability)
  val getMetaData: ConnectionIO[DatabaseMetaData] = FF.liftF(GetMetaData)
  val getNetworkTimeout: ConnectionIO[Int] = FF.liftF(GetNetworkTimeout)
  val getSchema: ConnectionIO[String] = FF.liftF(GetSchema)
  val getTransactionIsolation: ConnectionIO[Int] = FF.liftF(GetTransactionIsolation)
  val getTypeMap: ConnectionIO[java.util.Map[String, Class[?]]] = FF.liftF(GetTypeMap)
  val getWarnings: ConnectionIO[SQLWarning] = FF.liftF(GetWarnings)
  val isClosed: ConnectionIO[Boolean] = FF.liftF(IsClosed)
  val isReadOnly: ConnectionIO[Boolean] = FF.liftF(IsReadOnly)
  def isValid(a: Int): ConnectionIO[Boolean] = FF.liftF(IsValid(a))
  def isWrapperFor(a: Class[?]): ConnectionIO[Boolean] = FF.liftF(IsWrapperFor(a))
  def nativeSQL(a: String): ConnectionIO[String] = FF.liftF(NativeSQL(a))
  def prepareCall(a: String): ConnectionIO[CallableStatement] = FF.liftF(PrepareCall(a))
  def prepareCall(a: String, b: Int, c: Int): ConnectionIO[CallableStatement] = FF.liftF(PrepareCall1(a, b, c))
  def prepareCall(a: String, b: Int, c: Int, d: Int): ConnectionIO[CallableStatement] = FF.liftF(PrepareCall2(a, b, c, d))
  def prepareStatement(a: String): ConnectionIO[PreparedStatement] = FF.liftF(PrepareStatement(a))
  def prepareStatement(a: String, b: Array[Int]): ConnectionIO[PreparedStatement] = FF.liftF(PrepareStatement1(a, b))
  def prepareStatement(a: String, b: Array[String]): ConnectionIO[PreparedStatement] = FF.liftF(PrepareStatement2(a, b))
  def prepareStatement(a: String, b: Int): ConnectionIO[PreparedStatement] = FF.liftF(PrepareStatement3(a, b))
  def prepareStatement(a: String, b: Int, c: Int): ConnectionIO[PreparedStatement] = FF.liftF(PrepareStatement4(a, b, c))
  def prepareStatement(a: String, b: Int, c: Int, d: Int): ConnectionIO[PreparedStatement] = FF.liftF(PrepareStatement5(a, b, c, d))
  def releaseSavepoint(a: Savepoint): ConnectionIO[Unit] = FF.liftF(ReleaseSavepoint(a))
  val rollback: ConnectionIO[Unit] = FF.liftF(Rollback)
  def rollback(a: Savepoint): ConnectionIO[Unit] = FF.liftF(Rollback1(a))
  def setAutoCommit(a: Boolean): ConnectionIO[Unit] = FF.liftF(SetAutoCommit(a))
  def setCatalog(a: String): ConnectionIO[Unit] = FF.liftF(SetCatalog(a))
  def setClientInfo(a: Properties): ConnectionIO[Unit] = FF.liftF(SetClientInfo(a))
  def setClientInfo(a: String, b: String): ConnectionIO[Unit] = FF.liftF(SetClientInfo1(a, b))
  def setHoldability(a: Int): ConnectionIO[Unit] = FF.liftF(SetHoldability(a))
  def setNetworkTimeout(a: Executor, b: Int): ConnectionIO[Unit] = FF.liftF(SetNetworkTimeout(a, b))
  def setReadOnly(a: Boolean): ConnectionIO[Unit] = FF.liftF(SetReadOnly(a))
  val setSavepoint: ConnectionIO[Savepoint] = FF.liftF(SetSavepoint)
  def setSavepoint(a: String): ConnectionIO[Savepoint] = FF.liftF(SetSavepoint1(a))
  def setSchema(a: String): ConnectionIO[Unit] = FF.liftF(SetSchema(a))
  def setShardingKey(a: ShardingKey): ConnectionIO[Unit] = FF.liftF(SetShardingKey(a))
  def setShardingKey(a: ShardingKey, b: ShardingKey): ConnectionIO[Unit] = FF.liftF(SetShardingKey1(a, b))
  def setShardingKeyIfValid(a: ShardingKey, b: Int): ConnectionIO[Boolean] = FF.liftF(SetShardingKeyIfValid(a, b))
  def setShardingKeyIfValid(a: ShardingKey, b: ShardingKey, c: Int): ConnectionIO[Boolean] = FF.liftF(SetShardingKeyIfValid1(a, b, c))
  def setTransactionIsolation(a: Int): ConnectionIO[Unit] = FF.liftF(SetTransactionIsolation(a))
  def setTypeMap(a: java.util.Map[String, Class[?]]): ConnectionIO[Unit] = FF.liftF(SetTypeMap(a))
  def unwrap[T](a: Class[T]): ConnectionIO[T] = FF.liftF(Unwrap(a))

  // Typeclass instances for ConnectionIO
  implicit val WeakAsyncConnectionIO: WeakAsync[ConnectionIO] =
    new WeakAsync[ConnectionIO] {
      val monad = FF.catsFreeMonadForFree[ConnectionOp]
      override val applicative: Applicative[ConnectionIO] = monad
      override val rootCancelScope: CancelScope = CancelScope.Cancelable
      override def pure[A](x: A): ConnectionIO[A] = monad.pure(x)
      override def flatMap[A, B](fa: ConnectionIO[A])(f: A => ConnectionIO[B]): ConnectionIO[B] = monad.flatMap(fa)(f)
      override def tailRecM[A, B](a: A)(f: A => ConnectionIO[Either[A, B]]): ConnectionIO[B] = monad.tailRecM(a)(f)
      override def raiseError[A](e: Throwable): ConnectionIO[A] = module.raiseError(e)
      override def handleErrorWith[A](fa: ConnectionIO[A])(f: Throwable => ConnectionIO[A]): ConnectionIO[A] = module.handleErrorWith(fa)(f)
      override def monotonic: ConnectionIO[FiniteDuration] = module.monotonic
      override def realTime: ConnectionIO[FiniteDuration] = module.realtime
      override def suspend[A](hint: Sync.Type)(thunk: => A): ConnectionIO[A] = module.suspend(hint)(thunk)
      override def forceR[A, B](fa: ConnectionIO[A])(fb: ConnectionIO[B]): ConnectionIO[B] = module.forceR(fa)(fb)
      override def uncancelable[A](body: Poll[ConnectionIO] => ConnectionIO[A]): ConnectionIO[A] = module.uncancelable(body)
      override def canceled: ConnectionIO[Unit] = module.canceled
      override def onCancel[A](fa: ConnectionIO[A], fin: ConnectionIO[Unit]): ConnectionIO[A] = module.onCancel(fa, fin)
      override def fromFuture[A](fut: ConnectionIO[Future[A]]): ConnectionIO[A] = module.fromFuture(fut)
      override def fromFutureCancelable[A](fut: ConnectionIO[(Future[A], ConnectionIO[Unit])]): ConnectionIO[A] = module.fromFutureCancelable(fut)
      override def cancelable[A](fa: ConnectionIO[A], fin: ConnectionIO[Unit]): ConnectionIO[A] = module.cancelable(fa, fin)
    }
    
  implicit def MonoidConnectionIO[A : Monoid]: Monoid[ConnectionIO[A]] = new Monoid[ConnectionIO[A]] {
    override def empty: ConnectionIO[A] = Applicative[ConnectionIO].pure(Monoid[A].empty)
    override def combine(x: ConnectionIO[A], y: ConnectionIO[A]): ConnectionIO[A] =
      Applicative[ConnectionIO].product(x, y).map { case (x, y) => Monoid[A].combine(x, y) }
  }
 
  implicit def SemigroupConnectionIO[A : Semigroup]: Semigroup[ConnectionIO[A]] = new Semigroup[ConnectionIO[A]] {
    override def combine(x: ConnectionIO[A], y: ConnectionIO[A]): ConnectionIO[A] =
      Applicative[ConnectionIO].product(x, y).map { case (x, y) => Semigroup[A].combine(x, y) }
  }
}

