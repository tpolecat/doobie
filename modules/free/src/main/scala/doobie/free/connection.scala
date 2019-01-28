// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.free

import cats.~>
import cats.effect.{ Async, ContextShift, ExitCase }
import cats.free.{ Free => FF } // alias because some algebras have an op called Free
import scala.concurrent.ExecutionContext

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
import java.sql.Statement
import java.sql.Struct
import java.sql.{ Array => SqlArray }
import java.util.Map
import java.util.Properties
import java.util.concurrent.Executor

@com.github.ghik.silencer.silent // deprecations, unused variables, etc.
@SuppressWarnings(Array("org.wartremover.warts.Overloading"))
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
        def embed[A](j: Connection, fa: FF[ConnectionOp, A]) = Embedded.Connection(j, fa)
      }

    // Interface for a natural tansformation ConnectionOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (ConnectionOp ~> F) {
      final def apply[A](fa: ConnectionOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](f: Env[Connection] => A): F[A]
      def embed[A](e: Embedded[A]): F[A]
      def delay[A](a: () => A): F[A]
      def handleErrorWith[A](fa: ConnectionIO[A], f: Throwable => ConnectionIO[A]): F[A]
      def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]
      def asyncF[A](k: (Either[Throwable, A] => Unit) => ConnectionIO[Unit]): F[A]
      def bracketCase[A, B](acquire: ConnectionIO[A])(use: A => ConnectionIO[B])(release: (A, ExitCase[Throwable]) => ConnectionIO[Unit]): F[B]
      def shift: F[Unit]
      def evalOn[A](ec: ExecutionContext)(fa: ConnectionIO[A]): F[A]
      def liftE[G[_]](env: Env[Connection] => G ~> ConnectionIO): F[G ~> ConnectionIO]

      // Connection
      def abort(a: Executor): F[Unit]
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
      def getAutoCommit: F[Boolean]
      def getCatalog: F[String]
      def getClientInfo: F[Properties]
      def getClientInfo(a: String): F[String]
      def getHoldability: F[Int]
      def getMetaData: F[DatabaseMetaData]
      def getNetworkTimeout: F[Int]
      def getSchema: F[String]
      def getTransactionIsolation: F[Int]
      def getTypeMap: F[Map[String, Class[_]]]
      def getWarnings: F[SQLWarning]
      def isClosed: F[Boolean]
      def isReadOnly: F[Boolean]
      def isValid(a: Int): F[Boolean]
      def isWrapperFor(a: Class[_]): F[Boolean]
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
      def setTransactionIsolation(a: Int): F[Unit]
      def setTypeMap(a: Map[String, Class[_]]): F[Unit]
      def unwrap[T](a: Class[T]): F[T]

    }

    // Common operations for all algebras.
    final case class Raw[A](f: Env[Connection] => A) extends ConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(f)
    }
    final case class Embed[A](e: Embedded[A]) extends ConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class Delay[A](a: () => A) extends ConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.delay(a)
    }
    final case class HandleErrorWith[A](fa: ConnectionIO[A], f: Throwable => ConnectionIO[A]) extends ConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa, f)
    }
    final case class Async1[A](k: (Either[Throwable, A] => Unit) => Unit) extends ConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.async(k)
    }
    final case class AsyncF[A](k: (Either[Throwable, A] => Unit) => ConnectionIO[Unit]) extends ConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.asyncF(k)
    }
    final case class BracketCase[A, B](acquire: ConnectionIO[A], use: A => ConnectionIO[B], release: (A, ExitCase[Throwable]) => ConnectionIO[Unit]) extends ConnectionOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.bracketCase(acquire)(use)(release)
    }
    final case object Shift extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.shift
    }
    final case class EvalOn[A](ec: ExecutionContext, fa: ConnectionIO[A]) extends ConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.evalOn(ec)(fa)
    }
    final case class LiftE[G[_]](env: Env[Connection] => G ~> ConnectionIO) extends ConnectionOp[G ~> ConnectionIO] {
      def visit[F[_]](v: Visitor[F]) = v.liftE(env)
    }

    // Connection-specific operations.
    final case class  Abort(a: Executor) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.abort(a)
    }
    final case object ClearWarnings extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.clearWarnings
    }
    final case object Close extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.close
    }
    final case object Commit extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.commit
    }
    final case class  CreateArrayOf(a: String, b: Array[AnyRef]) extends ConnectionOp[SqlArray] {
      def visit[F[_]](v: Visitor[F]) = v.createArrayOf(a, b)
    }
    final case object CreateBlob extends ConnectionOp[Blob] {
      def visit[F[_]](v: Visitor[F]) = v.createBlob
    }
    final case object CreateClob extends ConnectionOp[Clob] {
      def visit[F[_]](v: Visitor[F]) = v.createClob
    }
    final case object CreateNClob extends ConnectionOp[NClob] {
      def visit[F[_]](v: Visitor[F]) = v.createNClob
    }
    final case object CreateSQLXML extends ConnectionOp[SQLXML] {
      def visit[F[_]](v: Visitor[F]) = v.createSQLXML
    }
    final case object CreateStatement extends ConnectionOp[Statement] {
      def visit[F[_]](v: Visitor[F]) = v.createStatement
    }
    final case class  CreateStatement1(a: Int, b: Int) extends ConnectionOp[Statement] {
      def visit[F[_]](v: Visitor[F]) = v.createStatement(a, b)
    }
    final case class  CreateStatement2(a: Int, b: Int, c: Int) extends ConnectionOp[Statement] {
      def visit[F[_]](v: Visitor[F]) = v.createStatement(a, b, c)
    }
    final case class  CreateStruct(a: String, b: Array[AnyRef]) extends ConnectionOp[Struct] {
      def visit[F[_]](v: Visitor[F]) = v.createStruct(a, b)
    }
    final case object GetAutoCommit extends ConnectionOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.getAutoCommit
    }
    final case object GetCatalog extends ConnectionOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getCatalog
    }
    final case object GetClientInfo extends ConnectionOp[Properties] {
      def visit[F[_]](v: Visitor[F]) = v.getClientInfo
    }
    final case class  GetClientInfo1(a: String) extends ConnectionOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getClientInfo(a)
    }
    final case object GetHoldability extends ConnectionOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getHoldability
    }
    final case object GetMetaData extends ConnectionOp[DatabaseMetaData] {
      def visit[F[_]](v: Visitor[F]) = v.getMetaData
    }
    final case object GetNetworkTimeout extends ConnectionOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getNetworkTimeout
    }
    final case object GetSchema extends ConnectionOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getSchema
    }
    final case object GetTransactionIsolation extends ConnectionOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getTransactionIsolation
    }
    final case object GetTypeMap extends ConnectionOp[Map[String, Class[_]]] {
      def visit[F[_]](v: Visitor[F]) = v.getTypeMap
    }
    final case object GetWarnings extends ConnectionOp[SQLWarning] {
      def visit[F[_]](v: Visitor[F]) = v.getWarnings
    }
    final case object IsClosed extends ConnectionOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isClosed
    }
    final case object IsReadOnly extends ConnectionOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isReadOnly
    }
    final case class  IsValid(a: Int) extends ConnectionOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isValid(a)
    }
    final case class  IsWrapperFor(a: Class[_]) extends ConnectionOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isWrapperFor(a)
    }
    final case class  NativeSQL(a: String) extends ConnectionOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.nativeSQL(a)
    }
    final case class  PrepareCall(a: String) extends ConnectionOp[CallableStatement] {
      def visit[F[_]](v: Visitor[F]) = v.prepareCall(a)
    }
    final case class  PrepareCall1(a: String, b: Int, c: Int) extends ConnectionOp[CallableStatement] {
      def visit[F[_]](v: Visitor[F]) = v.prepareCall(a, b, c)
    }
    final case class  PrepareCall2(a: String, b: Int, c: Int, d: Int) extends ConnectionOp[CallableStatement] {
      def visit[F[_]](v: Visitor[F]) = v.prepareCall(a, b, c, d)
    }
    final case class  PrepareStatement(a: String) extends ConnectionOp[PreparedStatement] {
      def visit[F[_]](v: Visitor[F]) = v.prepareStatement(a)
    }
    final case class  PrepareStatement1(a: String, b: Array[Int]) extends ConnectionOp[PreparedStatement] {
      def visit[F[_]](v: Visitor[F]) = v.prepareStatement(a, b)
    }
    final case class  PrepareStatement2(a: String, b: Array[String]) extends ConnectionOp[PreparedStatement] {
      def visit[F[_]](v: Visitor[F]) = v.prepareStatement(a, b)
    }
    final case class  PrepareStatement3(a: String, b: Int) extends ConnectionOp[PreparedStatement] {
      def visit[F[_]](v: Visitor[F]) = v.prepareStatement(a, b)
    }
    final case class  PrepareStatement4(a: String, b: Int, c: Int) extends ConnectionOp[PreparedStatement] {
      def visit[F[_]](v: Visitor[F]) = v.prepareStatement(a, b, c)
    }
    final case class  PrepareStatement5(a: String, b: Int, c: Int, d: Int) extends ConnectionOp[PreparedStatement] {
      def visit[F[_]](v: Visitor[F]) = v.prepareStatement(a, b, c, d)
    }
    final case class  ReleaseSavepoint(a: Savepoint) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.releaseSavepoint(a)
    }
    final case object Rollback extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.rollback
    }
    final case class  Rollback1(a: Savepoint) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.rollback(a)
    }
    final case class  SetAutoCommit(a: Boolean) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setAutoCommit(a)
    }
    final case class  SetCatalog(a: String) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setCatalog(a)
    }
    final case class  SetClientInfo(a: Properties) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setClientInfo(a)
    }
    final case class  SetClientInfo1(a: String, b: String) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setClientInfo(a, b)
    }
    final case class  SetHoldability(a: Int) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setHoldability(a)
    }
    final case class  SetNetworkTimeout(a: Executor, b: Int) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setNetworkTimeout(a, b)
    }
    final case class  SetReadOnly(a: Boolean) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setReadOnly(a)
    }
    final case object SetSavepoint extends ConnectionOp[Savepoint] {
      def visit[F[_]](v: Visitor[F]) = v.setSavepoint
    }
    final case class  SetSavepoint1(a: String) extends ConnectionOp[Savepoint] {
      def visit[F[_]](v: Visitor[F]) = v.setSavepoint(a)
    }
    final case class  SetSchema(a: String) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setSchema(a)
    }
    final case class  SetTransactionIsolation(a: Int) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setTransactionIsolation(a)
    }
    final case class  SetTypeMap(a: Map[String, Class[_]]) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setTypeMap(a)
    }
    final case class  Unwrap[T](a: Class[T]) extends ConnectionOp[T] {
      def visit[F[_]](v: Visitor[F]) = v.unwrap(a)
    }

  }
  import ConnectionOp._

  // Smart constructors for operations common to all algebras.
  val unit: ConnectionIO[Unit] = FF.pure[ConnectionOp, Unit](())
  def pure[A](a: A): ConnectionIO[A] = FF.pure[ConnectionOp, A](a)
  def raw[A](f: Env[Connection] => A): ConnectionIO[A] = FF.liftF(Raw(f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[ConnectionOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def delay[A](a: => A): ConnectionIO[A] = FF.liftF(Delay(() => a))
  def handleErrorWith[A](fa: ConnectionIO[A], f: Throwable => ConnectionIO[A]): ConnectionIO[A] = FF.liftF[ConnectionOp, A](HandleErrorWith(fa, f))
  def raiseError[A](err: Throwable): ConnectionIO[A] = delay(throw err)
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): ConnectionIO[A] = FF.liftF[ConnectionOp, A](Async1(k))
  def asyncF[A](k: (Either[Throwable, A] => Unit) => ConnectionIO[Unit]): ConnectionIO[A] = FF.liftF[ConnectionOp, A](AsyncF(k))
  def bracketCase[A, B](acquire: ConnectionIO[A])(use: A => ConnectionIO[B])(release: (A, ExitCase[Throwable]) => ConnectionIO[Unit]): ConnectionIO[B] = FF.liftF[ConnectionOp, B](BracketCase(acquire, use, release))
  val shift: ConnectionIO[Unit] = FF.liftF[ConnectionOp, Unit](Shift)
  def evalOn[A](ec: ExecutionContext)(fa: ConnectionIO[A]) = FF.liftF[ConnectionOp, A](EvalOn(ec, fa))
  def liftE[F[_]](env: Env[Connection] => F ~> ConnectionIO) = FF.liftF[ConnectionOp, F ~> ConnectionIO](LiftE(env))

  // Smart constructors for Connection-specific operations.
  def abort(a: Executor): ConnectionIO[Unit] = FF.liftF(Abort(a))
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
  val getAutoCommit: ConnectionIO[Boolean] = FF.liftF(GetAutoCommit)
  val getCatalog: ConnectionIO[String] = FF.liftF(GetCatalog)
  val getClientInfo: ConnectionIO[Properties] = FF.liftF(GetClientInfo)
  def getClientInfo(a: String): ConnectionIO[String] = FF.liftF(GetClientInfo1(a))
  val getHoldability: ConnectionIO[Int] = FF.liftF(GetHoldability)
  val getMetaData: ConnectionIO[DatabaseMetaData] = FF.liftF(GetMetaData)
  val getNetworkTimeout: ConnectionIO[Int] = FF.liftF(GetNetworkTimeout)
  val getSchema: ConnectionIO[String] = FF.liftF(GetSchema)
  val getTransactionIsolation: ConnectionIO[Int] = FF.liftF(GetTransactionIsolation)
  val getTypeMap: ConnectionIO[Map[String, Class[_]]] = FF.liftF(GetTypeMap)
  val getWarnings: ConnectionIO[SQLWarning] = FF.liftF(GetWarnings)
  val isClosed: ConnectionIO[Boolean] = FF.liftF(IsClosed)
  val isReadOnly: ConnectionIO[Boolean] = FF.liftF(IsReadOnly)
  def isValid(a: Int): ConnectionIO[Boolean] = FF.liftF(IsValid(a))
  def isWrapperFor(a: Class[_]): ConnectionIO[Boolean] = FF.liftF(IsWrapperFor(a))
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
  def setTransactionIsolation(a: Int): ConnectionIO[Unit] = FF.liftF(SetTransactionIsolation(a))
  def setTypeMap(a: Map[String, Class[_]]): ConnectionIO[Unit] = FF.liftF(SetTypeMap(a))
  def unwrap[T](a: Class[T]): ConnectionIO[T] = FF.liftF(Unwrap(a))

  // ConnectionIO is an Async
  implicit val AsyncConnectionIO: Async[ConnectionIO] =
    new Async[ConnectionIO] {
      val asyncM = FF.catsFreeMonadForFree[ConnectionOp]
      def bracketCase[A, B](acquire: ConnectionIO[A])(use: A => ConnectionIO[B])(release: (A, ExitCase[Throwable]) => ConnectionIO[Unit]): ConnectionIO[B] = module.bracketCase(acquire)(use)(release)
      def pure[A](x: A): ConnectionIO[A] = asyncM.pure(x)
      def handleErrorWith[A](fa: ConnectionIO[A])(f: Throwable => ConnectionIO[A]): ConnectionIO[A] = module.handleErrorWith(fa, f)
      def raiseError[A](e: Throwable): ConnectionIO[A] = module.raiseError(e)
      def async[A](k: (Either[Throwable,A] => Unit) => Unit): ConnectionIO[A] = module.async(k)
      def asyncF[A](k: (Either[Throwable,A] => Unit) => ConnectionIO[Unit]): ConnectionIO[A] = module.asyncF(k)
      def flatMap[A, B](fa: ConnectionIO[A])(f: A => ConnectionIO[B]): ConnectionIO[B] = asyncM.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => ConnectionIO[Either[A, B]]): ConnectionIO[B] = asyncM.tailRecM(a)(f)
      def suspend[A](thunk: => ConnectionIO[A]): ConnectionIO[A] = asyncM.flatten(module.delay(thunk))
    }

  // ConnectionIO is a ContextShift
  implicit val ContextShiftConnectionIO: ContextShift[ConnectionIO] =
    new ContextShift[ConnectionIO] {
      def shift: ConnectionIO[Unit] = module.shift
      def evalOn[A](ec: ExecutionContext)(fa: ConnectionIO[A]) = module.evalOn(ec)(fa)
    }
}

