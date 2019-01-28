// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres.free

import cats.~>
import cats.effect.{ Async, ContextShift, ExitCase }
import cats.free.{ Free => FF } // alias because some algebras have an op called Free
import scala.concurrent.ExecutionContext

import java.lang.Class
import java.lang.String
import java.sql.{ Array => SqlArray }
import org.postgresql.PGConnection
import org.postgresql.PGNotification
import org.postgresql.copy.{ CopyManager => PGCopyManager }
import org.postgresql.fastpath.{ Fastpath => PGFastpath }
import org.postgresql.jdbc.AutoSave
import org.postgresql.jdbc.PreferQueryMode
import org.postgresql.largeobject.LargeObjectManager
import org.postgresql.replication.PGReplicationConnection

@com.github.ghik.silencer.silent // deprecations, unused variables, etc.
@SuppressWarnings(Array("org.wartremover.warts.Overloading"))
object pgconnection { module =>

  // Algebra of operations for PGConnection. Each accepts a visitor as an alternatie to pattern-matching.
  sealed trait PGConnectionOp[A] {
    def visit[F[_]](v: PGConnectionOp.Visitor[F]): F[A]
  }

  // Free monad over PGConnectionOp.
  type PGConnectionIO[A] = FF[PGConnectionOp, A]

  // Module of instances and constructors of PGConnectionOp.
  object PGConnectionOp {

    // Given a PGConnection we can embed a PGConnectionIO program in any algebra that understands embedding.
    implicit val PGConnectionOpEmbeddable: Embeddable[PGConnectionOp, PGConnection] =
      new Embeddable[PGConnectionOp, PGConnection] {
        def embed[A](j: PGConnection, fa: FF[PGConnectionOp, A]) = Embedded.PGConnection(j, fa)
      }

    // Interface for a natural tansformation PGConnectionOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (PGConnectionOp ~> F) {
      final def apply[A](fa: PGConnectionOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](f: Env[PGConnection] => A): F[A]
      def embed[A](e: Embedded[A]): F[A]
      def delay[A](a: () => A): F[A]
      def handleErrorWith[A](fa: PGConnectionIO[A], f: Throwable => PGConnectionIO[A]): F[A]
      def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]
      def asyncF[A](k: (Either[Throwable, A] => Unit) => PGConnectionIO[Unit]): F[A]
      def bracketCase[A, B](acquire: PGConnectionIO[A])(use: A => PGConnectionIO[B])(release: (A, ExitCase[Throwable]) => PGConnectionIO[Unit]): F[B]
      def shift: F[Unit]
      def evalOn[A](ec: ExecutionContext)(fa: PGConnectionIO[A]): F[A]
      def liftE[G[_]](env: Env[PGConnection] => G ~> PGConnectionIO): F[G ~> PGConnectionIO]

      // PGConnection
      def addDataType(a: String, b: Class[_ <: org.postgresql.util.PGobject]): F[Unit]
      def addDataType(a: String, b: String): F[Unit]
      def createArrayOf(a: String, b: AnyRef): F[SqlArray]
      def escapeIdentifier(a: String): F[String]
      def escapeLiteral(a: String): F[String]
      def getAutosave: F[AutoSave]
      def getBackendPID: F[Int]
      def getCopyAPI: F[PGCopyManager]
      def getDefaultFetchSize: F[Int]
      def getFastpathAPI: F[PGFastpath]
      def getLargeObjectAPI: F[LargeObjectManager]
      def getNotifications: F[Array[PGNotification]]
      def getNotifications(a: Int): F[Array[PGNotification]]
      def getPreferQueryMode: F[PreferQueryMode]
      def getPrepareThreshold: F[Int]
      def getReplicationAPI: F[PGReplicationConnection]
      def setAutosave(a: AutoSave): F[Unit]
      def setDefaultFetchSize(a: Int): F[Unit]
      def setPrepareThreshold(a: Int): F[Unit]

    }

    // Common operations for all algebras.
    final case class Raw[A](f: Env[PGConnection] => A) extends PGConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(f)
    }
    final case class Embed[A](e: Embedded[A]) extends PGConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class Delay[A](a: () => A) extends PGConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.delay(a)
    }
    final case class HandleErrorWith[A](fa: PGConnectionIO[A], f: Throwable => PGConnectionIO[A]) extends PGConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa, f)
    }
    final case class Async1[A](k: (Either[Throwable, A] => Unit) => Unit) extends PGConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.async(k)
    }
    final case class AsyncF[A](k: (Either[Throwable, A] => Unit) => PGConnectionIO[Unit]) extends PGConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.asyncF(k)
    }
    final case class BracketCase[A, B](acquire: PGConnectionIO[A], use: A => PGConnectionIO[B], release: (A, ExitCase[Throwable]) => PGConnectionIO[Unit]) extends PGConnectionOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.bracketCase(acquire)(use)(release)
    }
    final case object Shift extends PGConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.shift
    }
    final case class EvalOn[A](ec: ExecutionContext, fa: PGConnectionIO[A]) extends PGConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.evalOn(ec)(fa)
    }
    final case class LiftE[G[_]](env: Env[PGConnection] => G ~> PGConnectionIO) extends PGConnectionOp[G ~> PGConnectionIO] {
      def visit[F[_]](v: Visitor[F]) = v.liftE(env)
    }

    // PGConnection-specific operations.
    final case class  AddDataType(a: String, b: Class[_ <: org.postgresql.util.PGobject]) extends PGConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.addDataType(a, b)
    }
    final case class  AddDataType1(a: String, b: String) extends PGConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.addDataType(a, b)
    }
    final case class  CreateArrayOf(a: String, b: AnyRef) extends PGConnectionOp[SqlArray] {
      def visit[F[_]](v: Visitor[F]) = v.createArrayOf(a, b)
    }
    final case class  EscapeIdentifier(a: String) extends PGConnectionOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.escapeIdentifier(a)
    }
    final case class  EscapeLiteral(a: String) extends PGConnectionOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.escapeLiteral(a)
    }
    final case object GetAutosave extends PGConnectionOp[AutoSave] {
      def visit[F[_]](v: Visitor[F]) = v.getAutosave
    }
    final case object GetBackendPID extends PGConnectionOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getBackendPID
    }
    final case object GetCopyAPI extends PGConnectionOp[PGCopyManager] {
      def visit[F[_]](v: Visitor[F]) = v.getCopyAPI
    }
    final case object GetDefaultFetchSize extends PGConnectionOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getDefaultFetchSize
    }
    final case object GetFastpathAPI extends PGConnectionOp[PGFastpath] {
      def visit[F[_]](v: Visitor[F]) = v.getFastpathAPI
    }
    final case object GetLargeObjectAPI extends PGConnectionOp[LargeObjectManager] {
      def visit[F[_]](v: Visitor[F]) = v.getLargeObjectAPI
    }
    final case object GetNotifications extends PGConnectionOp[Array[PGNotification]] {
      def visit[F[_]](v: Visitor[F]) = v.getNotifications
    }
    final case class  GetNotifications1(a: Int) extends PGConnectionOp[Array[PGNotification]] {
      def visit[F[_]](v: Visitor[F]) = v.getNotifications(a)
    }
    final case object GetPreferQueryMode extends PGConnectionOp[PreferQueryMode] {
      def visit[F[_]](v: Visitor[F]) = v.getPreferQueryMode
    }
    final case object GetPrepareThreshold extends PGConnectionOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getPrepareThreshold
    }
    final case object GetReplicationAPI extends PGConnectionOp[PGReplicationConnection] {
      def visit[F[_]](v: Visitor[F]) = v.getReplicationAPI
    }
    final case class  SetAutosave(a: AutoSave) extends PGConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setAutosave(a)
    }
    final case class  SetDefaultFetchSize(a: Int) extends PGConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setDefaultFetchSize(a)
    }
    final case class  SetPrepareThreshold(a: Int) extends PGConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setPrepareThreshold(a)
    }

  }
  import PGConnectionOp._

  // Smart constructors for operations common to all algebras.
  val unit: PGConnectionIO[Unit] = FF.pure[PGConnectionOp, Unit](())
  def pure[A](a: A): PGConnectionIO[A] = FF.pure[PGConnectionOp, A](a)
  def raw[A](f: Env[PGConnection] => A): PGConnectionIO[A] = FF.liftF(Raw(f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[PGConnectionOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def delay[A](a: => A): PGConnectionIO[A] = FF.liftF(Delay(() => a))
  def handleErrorWith[A](fa: PGConnectionIO[A], f: Throwable => PGConnectionIO[A]): PGConnectionIO[A] = FF.liftF[PGConnectionOp, A](HandleErrorWith(fa, f))
  def raiseError[A](err: Throwable): PGConnectionIO[A] = delay(throw err)
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): PGConnectionIO[A] = FF.liftF[PGConnectionOp, A](Async1(k))
  def asyncF[A](k: (Either[Throwable, A] => Unit) => PGConnectionIO[Unit]): PGConnectionIO[A] = FF.liftF[PGConnectionOp, A](AsyncF(k))
  def bracketCase[A, B](acquire: PGConnectionIO[A])(use: A => PGConnectionIO[B])(release: (A, ExitCase[Throwable]) => PGConnectionIO[Unit]): PGConnectionIO[B] = FF.liftF[PGConnectionOp, B](BracketCase(acquire, use, release))
  val shift: PGConnectionIO[Unit] = FF.liftF[PGConnectionOp, Unit](Shift)
  def evalOn[A](ec: ExecutionContext)(fa: PGConnectionIO[A]) = FF.liftF[PGConnectionOp, A](EvalOn(ec, fa))
  def liftE[F[_]](env: Env[PGConnection] => F ~> PGConnectionIO) = FF.liftF[PGConnectionOp, F ~> PGConnectionIO](LiftE(env))

  // Smart constructors for PGConnection-specific operations.
  def addDataType(a: String, b: Class[_ <: org.postgresql.util.PGobject]): PGConnectionIO[Unit] = FF.liftF(AddDataType(a, b))
  def addDataType(a: String, b: String): PGConnectionIO[Unit] = FF.liftF(AddDataType1(a, b))
  def createArrayOf(a: String, b: AnyRef): PGConnectionIO[SqlArray] = FF.liftF(CreateArrayOf(a, b))
  def escapeIdentifier(a: String): PGConnectionIO[String] = FF.liftF(EscapeIdentifier(a))
  def escapeLiteral(a: String): PGConnectionIO[String] = FF.liftF(EscapeLiteral(a))
  val getAutosave: PGConnectionIO[AutoSave] = FF.liftF(GetAutosave)
  val getBackendPID: PGConnectionIO[Int] = FF.liftF(GetBackendPID)
  val getCopyAPI: PGConnectionIO[PGCopyManager] = FF.liftF(GetCopyAPI)
  val getDefaultFetchSize: PGConnectionIO[Int] = FF.liftF(GetDefaultFetchSize)
  val getFastpathAPI: PGConnectionIO[PGFastpath] = FF.liftF(GetFastpathAPI)
  val getLargeObjectAPI: PGConnectionIO[LargeObjectManager] = FF.liftF(GetLargeObjectAPI)
  val getNotifications: PGConnectionIO[Array[PGNotification]] = FF.liftF(GetNotifications)
  def getNotifications(a: Int): PGConnectionIO[Array[PGNotification]] = FF.liftF(GetNotifications1(a))
  val getPreferQueryMode: PGConnectionIO[PreferQueryMode] = FF.liftF(GetPreferQueryMode)
  val getPrepareThreshold: PGConnectionIO[Int] = FF.liftF(GetPrepareThreshold)
  val getReplicationAPI: PGConnectionIO[PGReplicationConnection] = FF.liftF(GetReplicationAPI)
  def setAutosave(a: AutoSave): PGConnectionIO[Unit] = FF.liftF(SetAutosave(a))
  def setDefaultFetchSize(a: Int): PGConnectionIO[Unit] = FF.liftF(SetDefaultFetchSize(a))
  def setPrepareThreshold(a: Int): PGConnectionIO[Unit] = FF.liftF(SetPrepareThreshold(a))

  // PGConnectionIO is an Async
  implicit val AsyncPGConnectionIO: Async[PGConnectionIO] =
    new Async[PGConnectionIO] {
      val asyncM = FF.catsFreeMonadForFree[PGConnectionOp]
      def bracketCase[A, B](acquire: PGConnectionIO[A])(use: A => PGConnectionIO[B])(release: (A, ExitCase[Throwable]) => PGConnectionIO[Unit]): PGConnectionIO[B] = module.bracketCase(acquire)(use)(release)
      def pure[A](x: A): PGConnectionIO[A] = asyncM.pure(x)
      def handleErrorWith[A](fa: PGConnectionIO[A])(f: Throwable => PGConnectionIO[A]): PGConnectionIO[A] = module.handleErrorWith(fa, f)
      def raiseError[A](e: Throwable): PGConnectionIO[A] = module.raiseError(e)
      def async[A](k: (Either[Throwable,A] => Unit) => Unit): PGConnectionIO[A] = module.async(k)
      def asyncF[A](k: (Either[Throwable,A] => Unit) => PGConnectionIO[Unit]): PGConnectionIO[A] = module.asyncF(k)
      def flatMap[A, B](fa: PGConnectionIO[A])(f: A => PGConnectionIO[B]): PGConnectionIO[B] = asyncM.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => PGConnectionIO[Either[A, B]]): PGConnectionIO[B] = asyncM.tailRecM(a)(f)
      def suspend[A](thunk: => PGConnectionIO[A]): PGConnectionIO[A] = asyncM.flatten(module.delay(thunk))
    }

  // PGConnectionIO is a ContextShift
  implicit val ContextShiftPGConnectionIO: ContextShift[PGConnectionIO] =
    new ContextShift[PGConnectionIO] {
      def shift: PGConnectionIO[Unit] = module.shift
      def evalOn[A](ec: ExecutionContext)(fa: PGConnectionIO[A]) = module.evalOn(ec)(fa)
    }
}

