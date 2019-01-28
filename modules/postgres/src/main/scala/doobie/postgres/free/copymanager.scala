// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres.free

import cats.~>
import cats.effect.{ Async, ContextShift, ExitCase }
import cats.free.{ Free => FF } // alias because some algebras have an op called Free
import scala.concurrent.ExecutionContext

import java.io.InputStream
import java.io.OutputStream
import java.io.Reader
import java.io.Writer
import java.lang.String
import org.postgresql.copy.{ CopyDual => PGCopyDual }
import org.postgresql.copy.{ CopyIn => PGCopyIn }
import org.postgresql.copy.{ CopyManager => PGCopyManager }
import org.postgresql.copy.{ CopyOut => PGCopyOut }

@com.github.ghik.silencer.silent // deprecations, unused variables, etc.
@SuppressWarnings(Array("org.wartremover.warts.Overloading"))
object copymanager { module =>

  // Algebra of operations for PGCopyManager. Each accepts a visitor as an alternatie to pattern-matching.
  sealed trait CopyManagerOp[A] {
    def visit[F[_]](v: CopyManagerOp.Visitor[F]): F[A]
  }

  // Free monad over CopyManagerOp.
  type CopyManagerIO[A] = FF[CopyManagerOp, A]

  // Module of instances and constructors of CopyManagerOp.
  object CopyManagerOp {

    // Given a PGCopyManager we can embed a CopyManagerIO program in any algebra that understands embedding.
    implicit val CopyManagerOpEmbeddable: Embeddable[CopyManagerOp, PGCopyManager] =
      new Embeddable[CopyManagerOp, PGCopyManager] {
        def embed[A](j: PGCopyManager, fa: FF[CopyManagerOp, A]) = Embedded.CopyManager(j, fa)
      }

    // Interface for a natural tansformation CopyManagerOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (CopyManagerOp ~> F) {
      final def apply[A](fa: CopyManagerOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](f: Env[PGCopyManager] => A): F[A]
      def embed[A](e: Embedded[A]): F[A]
      def delay[A](a: () => A): F[A]
      def handleErrorWith[A](fa: CopyManagerIO[A], f: Throwable => CopyManagerIO[A]): F[A]
      def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]
      def asyncF[A](k: (Either[Throwable, A] => Unit) => CopyManagerIO[Unit]): F[A]
      def bracketCase[A, B](acquire: CopyManagerIO[A])(use: A => CopyManagerIO[B])(release: (A, ExitCase[Throwable]) => CopyManagerIO[Unit]): F[B]
      def shift: F[Unit]
      def evalOn[A](ec: ExecutionContext)(fa: CopyManagerIO[A]): F[A]
      def liftE[G[_]](env: Env[PGCopyManager] => G ~> CopyManagerIO): F[G ~> CopyManagerIO]

      // PGCopyManager
      def copyDual(a: String): F[PGCopyDual]
      def copyIn(a: String): F[PGCopyIn]
      def copyIn(a: String, b: InputStream): F[Long]
      def copyIn(a: String, b: InputStream, c: Int): F[Long]
      def copyIn(a: String, b: Reader): F[Long]
      def copyIn(a: String, b: Reader, c: Int): F[Long]
      def copyOut(a: String): F[PGCopyOut]
      def copyOut(a: String, b: OutputStream): F[Long]
      def copyOut(a: String, b: Writer): F[Long]

    }

    // Common operations for all algebras.
    final case class Raw[A](f: Env[PGCopyManager] => A) extends CopyManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(f)
    }
    final case class Embed[A](e: Embedded[A]) extends CopyManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class Delay[A](a: () => A) extends CopyManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.delay(a)
    }
    final case class HandleErrorWith[A](fa: CopyManagerIO[A], f: Throwable => CopyManagerIO[A]) extends CopyManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa, f)
    }
    final case class Async1[A](k: (Either[Throwable, A] => Unit) => Unit) extends CopyManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.async(k)
    }
    final case class AsyncF[A](k: (Either[Throwable, A] => Unit) => CopyManagerIO[Unit]) extends CopyManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.asyncF(k)
    }
    final case class BracketCase[A, B](acquire: CopyManagerIO[A], use: A => CopyManagerIO[B], release: (A, ExitCase[Throwable]) => CopyManagerIO[Unit]) extends CopyManagerOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.bracketCase(acquire)(use)(release)
    }
    final case object Shift extends CopyManagerOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.shift
    }
    final case class EvalOn[A](ec: ExecutionContext, fa: CopyManagerIO[A]) extends CopyManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.evalOn(ec)(fa)
    }
    final case class LiftE[G[_]](env: Env[PGCopyManager] => G ~> CopyManagerIO) extends CopyManagerOp[G ~> CopyManagerIO] {
      def visit[F[_]](v: Visitor[F]) = v.liftE(env)
    }

    // PGCopyManager-specific operations.
    final case class  CopyDual(a: String) extends CopyManagerOp[PGCopyDual] {
      def visit[F[_]](v: Visitor[F]) = v.copyDual(a)
    }
    final case class  CopyIn(a: String) extends CopyManagerOp[PGCopyIn] {
      def visit[F[_]](v: Visitor[F]) = v.copyIn(a)
    }
    final case class  CopyIn1(a: String, b: InputStream) extends CopyManagerOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.copyIn(a, b)
    }
    final case class  CopyIn2(a: String, b: InputStream, c: Int) extends CopyManagerOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.copyIn(a, b, c)
    }
    final case class  CopyIn3(a: String, b: Reader) extends CopyManagerOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.copyIn(a, b)
    }
    final case class  CopyIn4(a: String, b: Reader, c: Int) extends CopyManagerOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.copyIn(a, b, c)
    }
    final case class  CopyOut(a: String) extends CopyManagerOp[PGCopyOut] {
      def visit[F[_]](v: Visitor[F]) = v.copyOut(a)
    }
    final case class  CopyOut1(a: String, b: OutputStream) extends CopyManagerOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.copyOut(a, b)
    }
    final case class  CopyOut2(a: String, b: Writer) extends CopyManagerOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.copyOut(a, b)
    }

  }
  import CopyManagerOp._

  // Smart constructors for operations common to all algebras.
  val unit: CopyManagerIO[Unit] = FF.pure[CopyManagerOp, Unit](())
  def pure[A](a: A): CopyManagerIO[A] = FF.pure[CopyManagerOp, A](a)
  def raw[A](f: Env[PGCopyManager] => A): CopyManagerIO[A] = FF.liftF(Raw(f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[CopyManagerOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def delay[A](a: => A): CopyManagerIO[A] = FF.liftF(Delay(() => a))
  def handleErrorWith[A](fa: CopyManagerIO[A], f: Throwable => CopyManagerIO[A]): CopyManagerIO[A] = FF.liftF[CopyManagerOp, A](HandleErrorWith(fa, f))
  def raiseError[A](err: Throwable): CopyManagerIO[A] = delay(throw err)
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): CopyManagerIO[A] = FF.liftF[CopyManagerOp, A](Async1(k))
  def asyncF[A](k: (Either[Throwable, A] => Unit) => CopyManagerIO[Unit]): CopyManagerIO[A] = FF.liftF[CopyManagerOp, A](AsyncF(k))
  def bracketCase[A, B](acquire: CopyManagerIO[A])(use: A => CopyManagerIO[B])(release: (A, ExitCase[Throwable]) => CopyManagerIO[Unit]): CopyManagerIO[B] = FF.liftF[CopyManagerOp, B](BracketCase(acquire, use, release))
  val shift: CopyManagerIO[Unit] = FF.liftF[CopyManagerOp, Unit](Shift)
  def evalOn[A](ec: ExecutionContext)(fa: CopyManagerIO[A]) = FF.liftF[CopyManagerOp, A](EvalOn(ec, fa))
  def liftE[F[_]](env: Env[PGCopyManager] => F ~> CopyManagerIO) = FF.liftF[CopyManagerOp, F ~> CopyManagerIO](LiftE(env))

  // Smart constructors for CopyManager-specific operations.
  def copyDual(a: String): CopyManagerIO[PGCopyDual] = FF.liftF(CopyDual(a))
  def copyIn(a: String): CopyManagerIO[PGCopyIn] = FF.liftF(CopyIn(a))
  def copyIn(a: String, b: InputStream): CopyManagerIO[Long] = FF.liftF(CopyIn1(a, b))
  def copyIn(a: String, b: InputStream, c: Int): CopyManagerIO[Long] = FF.liftF(CopyIn2(a, b, c))
  def copyIn(a: String, b: Reader): CopyManagerIO[Long] = FF.liftF(CopyIn3(a, b))
  def copyIn(a: String, b: Reader, c: Int): CopyManagerIO[Long] = FF.liftF(CopyIn4(a, b, c))
  def copyOut(a: String): CopyManagerIO[PGCopyOut] = FF.liftF(CopyOut(a))
  def copyOut(a: String, b: OutputStream): CopyManagerIO[Long] = FF.liftF(CopyOut1(a, b))
  def copyOut(a: String, b: Writer): CopyManagerIO[Long] = FF.liftF(CopyOut2(a, b))

  // CopyManagerIO is an Async
  implicit val AsyncCopyManagerIO: Async[CopyManagerIO] =
    new Async[CopyManagerIO] {
      val asyncM = FF.catsFreeMonadForFree[CopyManagerOp]
      def bracketCase[A, B](acquire: CopyManagerIO[A])(use: A => CopyManagerIO[B])(release: (A, ExitCase[Throwable]) => CopyManagerIO[Unit]): CopyManagerIO[B] = module.bracketCase(acquire)(use)(release)
      def pure[A](x: A): CopyManagerIO[A] = asyncM.pure(x)
      def handleErrorWith[A](fa: CopyManagerIO[A])(f: Throwable => CopyManagerIO[A]): CopyManagerIO[A] = module.handleErrorWith(fa, f)
      def raiseError[A](e: Throwable): CopyManagerIO[A] = module.raiseError(e)
      def async[A](k: (Either[Throwable,A] => Unit) => Unit): CopyManagerIO[A] = module.async(k)
      def asyncF[A](k: (Either[Throwable,A] => Unit) => CopyManagerIO[Unit]): CopyManagerIO[A] = module.asyncF(k)
      def flatMap[A, B](fa: CopyManagerIO[A])(f: A => CopyManagerIO[B]): CopyManagerIO[B] = asyncM.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => CopyManagerIO[Either[A, B]]): CopyManagerIO[B] = asyncM.tailRecM(a)(f)
      def suspend[A](thunk: => CopyManagerIO[A]): CopyManagerIO[A] = asyncM.flatten(module.delay(thunk))
    }

  // CopyManagerIO is a ContextShift
  implicit val ContextShiftCopyManagerIO: ContextShift[CopyManagerIO] =
    new ContextShift[CopyManagerIO] {
      def shift: CopyManagerIO[Unit] = module.shift
      def evalOn[A](ec: ExecutionContext)(fa: CopyManagerIO[A]) = module.evalOn(ec)(fa)
    }
}

