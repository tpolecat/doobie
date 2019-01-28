// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres.free

import cats.~>
import cats.effect.{ Async, ContextShift, ExitCase }
import cats.free.{ Free => FF } // alias because some algebras have an op called Free
import scala.concurrent.ExecutionContext

import org.postgresql.copy.{ CopyOut => PGCopyOut }

@com.github.ghik.silencer.silent // deprecations, unused variables, etc.
@SuppressWarnings(Array("org.wartremover.warts.Overloading"))
object copyout { module =>

  // Algebra of operations for PGCopyOut. Each accepts a visitor as an alternatie to pattern-matching.
  sealed trait CopyOutOp[A] {
    def visit[F[_]](v: CopyOutOp.Visitor[F]): F[A]
  }

  // Free monad over CopyOutOp.
  type CopyOutIO[A] = FF[CopyOutOp, A]

  // Module of instances and constructors of CopyOutOp.
  object CopyOutOp {

    // Given a PGCopyOut we can embed a CopyOutIO program in any algebra that understands embedding.
    implicit val CopyOutOpEmbeddable: Embeddable[CopyOutOp, PGCopyOut] =
      new Embeddable[CopyOutOp, PGCopyOut] {
        def embed[A](j: PGCopyOut, fa: FF[CopyOutOp, A]) = Embedded.CopyOut(j, fa)
      }

    // Interface for a natural tansformation CopyOutOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (CopyOutOp ~> F) {
      final def apply[A](fa: CopyOutOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](f: Env[PGCopyOut] => A): F[A]
      def embed[A](e: Embedded[A]): F[A]
      def delay[A](a: () => A): F[A]
      def handleErrorWith[A](fa: CopyOutIO[A], f: Throwable => CopyOutIO[A]): F[A]
      def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]
      def asyncF[A](k: (Either[Throwable, A] => Unit) => CopyOutIO[Unit]): F[A]
      def bracketCase[A, B](acquire: CopyOutIO[A])(use: A => CopyOutIO[B])(release: (A, ExitCase[Throwable]) => CopyOutIO[Unit]): F[B]
      def shift: F[Unit]
      def evalOn[A](ec: ExecutionContext)(fa: CopyOutIO[A]): F[A]
      def liftE[G[_]](env: Env[PGCopyOut] => G ~> CopyOutIO): F[G ~> CopyOutIO]

      // PGCopyOut
      def cancelCopy: F[Unit]
      def getFieldCount: F[Int]
      def getFieldFormat(a: Int): F[Int]
      def getFormat: F[Int]
      def getHandledRowCount: F[Long]
      def isActive: F[Boolean]
      def readFromCopy: F[Array[Byte]]
      def readFromCopy(a: Boolean): F[Array[Byte]]

    }

    // Common operations for all algebras.
    final case class Raw[A](f: Env[PGCopyOut] => A) extends CopyOutOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(f)
    }
    final case class Embed[A](e: Embedded[A]) extends CopyOutOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class Delay[A](a: () => A) extends CopyOutOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.delay(a)
    }
    final case class HandleErrorWith[A](fa: CopyOutIO[A], f: Throwable => CopyOutIO[A]) extends CopyOutOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa, f)
    }
    final case class Async1[A](k: (Either[Throwable, A] => Unit) => Unit) extends CopyOutOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.async(k)
    }
    final case class AsyncF[A](k: (Either[Throwable, A] => Unit) => CopyOutIO[Unit]) extends CopyOutOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.asyncF(k)
    }
    final case class BracketCase[A, B](acquire: CopyOutIO[A], use: A => CopyOutIO[B], release: (A, ExitCase[Throwable]) => CopyOutIO[Unit]) extends CopyOutOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.bracketCase(acquire)(use)(release)
    }
    final case object Shift extends CopyOutOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.shift
    }
    final case class EvalOn[A](ec: ExecutionContext, fa: CopyOutIO[A]) extends CopyOutOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.evalOn(ec)(fa)
    }
    final case class LiftE[G[_]](env: Env[PGCopyOut] => G ~> CopyOutIO) extends CopyOutOp[G ~> CopyOutIO] {
      def visit[F[_]](v: Visitor[F]) = v.liftE(env)
    }

    // PGCopyOut-specific operations.
    final case object CancelCopy extends CopyOutOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.cancelCopy
    }
    final case object GetFieldCount extends CopyOutOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getFieldCount
    }
    final case class  GetFieldFormat(a: Int) extends CopyOutOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getFieldFormat(a)
    }
    final case object GetFormat extends CopyOutOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getFormat
    }
    final case object GetHandledRowCount extends CopyOutOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.getHandledRowCount
    }
    final case object IsActive extends CopyOutOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isActive
    }
    final case object ReadFromCopy extends CopyOutOp[Array[Byte]] {
      def visit[F[_]](v: Visitor[F]) = v.readFromCopy
    }
    final case class  ReadFromCopy1(a: Boolean) extends CopyOutOp[Array[Byte]] {
      def visit[F[_]](v: Visitor[F]) = v.readFromCopy(a)
    }

  }
  import CopyOutOp._

  // Smart constructors for operations common to all algebras.
  val unit: CopyOutIO[Unit] = FF.pure[CopyOutOp, Unit](())
  def pure[A](a: A): CopyOutIO[A] = FF.pure[CopyOutOp, A](a)
  def raw[A](f: Env[PGCopyOut] => A): CopyOutIO[A] = FF.liftF(Raw(f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[CopyOutOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def delay[A](a: => A): CopyOutIO[A] = FF.liftF(Delay(() => a))
  def handleErrorWith[A](fa: CopyOutIO[A], f: Throwable => CopyOutIO[A]): CopyOutIO[A] = FF.liftF[CopyOutOp, A](HandleErrorWith(fa, f))
  def raiseError[A](err: Throwable): CopyOutIO[A] = delay(throw err)
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): CopyOutIO[A] = FF.liftF[CopyOutOp, A](Async1(k))
  def asyncF[A](k: (Either[Throwable, A] => Unit) => CopyOutIO[Unit]): CopyOutIO[A] = FF.liftF[CopyOutOp, A](AsyncF(k))
  def bracketCase[A, B](acquire: CopyOutIO[A])(use: A => CopyOutIO[B])(release: (A, ExitCase[Throwable]) => CopyOutIO[Unit]): CopyOutIO[B] = FF.liftF[CopyOutOp, B](BracketCase(acquire, use, release))
  val shift: CopyOutIO[Unit] = FF.liftF[CopyOutOp, Unit](Shift)
  def evalOn[A](ec: ExecutionContext)(fa: CopyOutIO[A]) = FF.liftF[CopyOutOp, A](EvalOn(ec, fa))
  def liftE[F[_]](env: Env[PGCopyOut] => F ~> CopyOutIO) = FF.liftF[CopyOutOp, F ~> CopyOutIO](LiftE(env))

  // Smart constructors for CopyOut-specific operations.
  val cancelCopy: CopyOutIO[Unit] = FF.liftF(CancelCopy)
  val getFieldCount: CopyOutIO[Int] = FF.liftF(GetFieldCount)
  def getFieldFormat(a: Int): CopyOutIO[Int] = FF.liftF(GetFieldFormat(a))
  val getFormat: CopyOutIO[Int] = FF.liftF(GetFormat)
  val getHandledRowCount: CopyOutIO[Long] = FF.liftF(GetHandledRowCount)
  val isActive: CopyOutIO[Boolean] = FF.liftF(IsActive)
  val readFromCopy: CopyOutIO[Array[Byte]] = FF.liftF(ReadFromCopy)
  def readFromCopy(a: Boolean): CopyOutIO[Array[Byte]] = FF.liftF(ReadFromCopy1(a))

  // CopyOutIO is an Async
  implicit val AsyncCopyOutIO: Async[CopyOutIO] =
    new Async[CopyOutIO] {
      val asyncM = FF.catsFreeMonadForFree[CopyOutOp]
      def bracketCase[A, B](acquire: CopyOutIO[A])(use: A => CopyOutIO[B])(release: (A, ExitCase[Throwable]) => CopyOutIO[Unit]): CopyOutIO[B] = module.bracketCase(acquire)(use)(release)
      def pure[A](x: A): CopyOutIO[A] = asyncM.pure(x)
      def handleErrorWith[A](fa: CopyOutIO[A])(f: Throwable => CopyOutIO[A]): CopyOutIO[A] = module.handleErrorWith(fa, f)
      def raiseError[A](e: Throwable): CopyOutIO[A] = module.raiseError(e)
      def async[A](k: (Either[Throwable,A] => Unit) => Unit): CopyOutIO[A] = module.async(k)
      def asyncF[A](k: (Either[Throwable,A] => Unit) => CopyOutIO[Unit]): CopyOutIO[A] = module.asyncF(k)
      def flatMap[A, B](fa: CopyOutIO[A])(f: A => CopyOutIO[B]): CopyOutIO[B] = asyncM.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => CopyOutIO[Either[A, B]]): CopyOutIO[B] = asyncM.tailRecM(a)(f)
      def suspend[A](thunk: => CopyOutIO[A]): CopyOutIO[A] = asyncM.flatten(module.delay(thunk))
    }

  // CopyOutIO is a ContextShift
  implicit val ContextShiftCopyOutIO: ContextShift[CopyOutIO] =
    new ContextShift[CopyOutIO] {
      def shift: CopyOutIO[Unit] = module.shift
      def evalOn[A](ec: ExecutionContext)(fa: CopyOutIO[A]) = module.evalOn(ec)(fa)
    }
}

