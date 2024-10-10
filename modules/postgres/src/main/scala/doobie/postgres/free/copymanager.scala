// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

// format: off

package doobie.postgres.free

import cats.{~>, Applicative, Semigroup, Monoid}
import cats.effect.kernel.{ CancelScope, Poll, Sync }
import cats.free.{ Free as FF } // alias because some algebras have an op called Free
import doobie.util.log.LogEvent
import doobie.WeakAsync
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

import java.io.InputStream
import java.io.OutputStream
import java.io.Reader
import java.io.Writer
import java.lang.String
import org.postgresql.copy.{ CopyDual as PGCopyDual }
import org.postgresql.copy.{ CopyIn as PGCopyIn }
import org.postgresql.copy.{ CopyManager as PGCopyManager }
import org.postgresql.copy.{ CopyOut as PGCopyOut }
import org.postgresql.util.ByteStreamWriter

// This file is Auto-generated using FreeGen2.scala
object copymanager { module =>

  // Algebra of operations for PGCopyManager. Each accepts a visitor as an alternative to pattern-matching.
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
        def embed[A](j: PGCopyManager, fa: FF[CopyManagerOp, A]): Embedded.CopyManager[A] = Embedded.CopyManager(j, fa)
      }

    // Interface for a natural transformation CopyManagerOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (CopyManagerOp ~> F) {
      final def apply[A](fa: CopyManagerOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](f: PGCopyManager => A): F[A]
      def embed[A](e: Embedded[A]): F[A]
      def raiseError[A](e: Throwable): F[A]
      def handleErrorWith[A](fa: CopyManagerIO[A])(f: Throwable => CopyManagerIO[A]): F[A]
      def monotonic: F[FiniteDuration]
      def realTime: F[FiniteDuration]
      def delay[A](thunk: => A): F[A]
      def suspend[A](hint: Sync.Type)(thunk: => A): F[A]
      def forceR[A, B](fa: CopyManagerIO[A])(fb: CopyManagerIO[B]): F[B]
      def uncancelable[A](body: Poll[CopyManagerIO] => CopyManagerIO[A]): F[A]
      def poll[A](poll: Any, fa: CopyManagerIO[A]): F[A]
      def canceled: F[Unit]
      def onCancel[A](fa: CopyManagerIO[A], fin: CopyManagerIO[Unit]): F[A]
      def fromFuture[A](fut: CopyManagerIO[Future[A]]): F[A]
      def fromFutureCancelable[A](fut: CopyManagerIO[(Future[A], CopyManagerIO[Unit])]): F[A]
      def cancelable[A](fa: CopyManagerIO[A], fin: CopyManagerIO[Unit]): F[A]
      def performLogging(event: LogEvent): F[Unit]

      // PGCopyManager
      def copyDual(a: String): F[PGCopyDual]
      def copyIn(a: String): F[PGCopyIn]
      def copyIn(a: String, b: ByteStreamWriter): F[Long]
      def copyIn(a: String, b: InputStream): F[Long]
      def copyIn(a: String, b: InputStream, c: Int): F[Long]
      def copyIn(a: String, b: Reader): F[Long]
      def copyIn(a: String, b: Reader, c: Int): F[Long]
      def copyOut(a: String): F[PGCopyOut]
      def copyOut(a: String, b: OutputStream): F[Long]
      def copyOut(a: String, b: Writer): F[Long]

    }

    // Common operations for all algebras.
    final case class Raw[A](f: PGCopyManager => A) extends CopyManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(f)
    }
    final case class Embed[A](e: Embedded[A]) extends CopyManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class RaiseError[A](e: Throwable) extends CopyManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raiseError(e)
    }
    final case class HandleErrorWith[A](fa: CopyManagerIO[A], f: Throwable => CopyManagerIO[A]) extends CopyManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa)(f)
    }
    case object Monotonic extends CopyManagerOp[FiniteDuration] {
      def visit[F[_]](v: Visitor[F]) = v.monotonic
    }
    case object Realtime extends CopyManagerOp[FiniteDuration] {
      def visit[F[_]](v: Visitor[F]) = v.realTime
    }
    case class Suspend[A](hint: Sync.Type, thunk: () => A) extends CopyManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.suspend(hint)(thunk())
    }
    case class ForceR[A, B](fa: CopyManagerIO[A], fb: CopyManagerIO[B]) extends CopyManagerOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.forceR(fa)(fb)
    }
    case class Uncancelable[A](body: Poll[CopyManagerIO] => CopyManagerIO[A]) extends CopyManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.uncancelable(body)
    }
    case class Poll1[A](poll: Any, fa: CopyManagerIO[A]) extends CopyManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.poll(poll, fa)
    }
    case object Canceled extends CopyManagerOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.canceled
    }
    case class OnCancel[A](fa: CopyManagerIO[A], fin: CopyManagerIO[Unit]) extends CopyManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.onCancel(fa, fin)
    }
    case class FromFuture[A](fut: CopyManagerIO[Future[A]]) extends CopyManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.fromFuture(fut)
    }
    case class FromFutureCancelable[A](fut: CopyManagerIO[(Future[A], CopyManagerIO[Unit])]) extends CopyManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.fromFutureCancelable(fut)
    }
    case class Cancelable[A](fa: CopyManagerIO[A], fin: CopyManagerIO[Unit]) extends CopyManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.cancelable(fa, fin)
    }
    case class PerformLogging(event: LogEvent) extends CopyManagerOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.performLogging(event)
    }

    // PGCopyManager-specific operations.
    final case class CopyDual(a: String) extends CopyManagerOp[PGCopyDual] {
      def visit[F[_]](v: Visitor[F]) = v.copyDual(a)
    }
    final case class CopyIn(a: String) extends CopyManagerOp[PGCopyIn] {
      def visit[F[_]](v: Visitor[F]) = v.copyIn(a)
    }
    final case class CopyIn1(a: String, b: ByteStreamWriter) extends CopyManagerOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.copyIn(a, b)
    }
    final case class CopyIn2(a: String, b: InputStream) extends CopyManagerOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.copyIn(a, b)
    }
    final case class CopyIn3(a: String, b: InputStream, c: Int) extends CopyManagerOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.copyIn(a, b, c)
    }
    final case class CopyIn4(a: String, b: Reader) extends CopyManagerOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.copyIn(a, b)
    }
    final case class CopyIn5(a: String, b: Reader, c: Int) extends CopyManagerOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.copyIn(a, b, c)
    }
    final case class CopyOut(a: String) extends CopyManagerOp[PGCopyOut] {
      def visit[F[_]](v: Visitor[F]) = v.copyOut(a)
    }
    final case class CopyOut1(a: String, b: OutputStream) extends CopyManagerOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.copyOut(a, b)
    }
    final case class CopyOut2(a: String, b: Writer) extends CopyManagerOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.copyOut(a, b)
    }

  }
  import CopyManagerOp.*

  // Smart constructors for operations common to all algebras.
  val unit: CopyManagerIO[Unit] = FF.pure[CopyManagerOp, Unit](())
  def pure[A](a: A): CopyManagerIO[A] = FF.pure[CopyManagerOp, A](a)
  def raw[A](f: PGCopyManager => A): CopyManagerIO[A] = FF.liftF(Raw(f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[CopyManagerOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def raiseError[A](err: Throwable): CopyManagerIO[A] = FF.liftF[CopyManagerOp, A](RaiseError(err))
  def handleErrorWith[A](fa: CopyManagerIO[A])(f: Throwable => CopyManagerIO[A]): CopyManagerIO[A] = FF.liftF[CopyManagerOp, A](HandleErrorWith(fa, f))
  val monotonic = FF.liftF[CopyManagerOp, FiniteDuration](Monotonic)
  val realtime = FF.liftF[CopyManagerOp, FiniteDuration](Realtime)
  def delay[A](thunk: => A) = FF.liftF[CopyManagerOp, A](Suspend(Sync.Type.Delay, () => thunk))
  def suspend[A](hint: Sync.Type)(thunk: => A) = FF.liftF[CopyManagerOp, A](Suspend(hint, () => thunk))
  def forceR[A, B](fa: CopyManagerIO[A])(fb: CopyManagerIO[B]) = FF.liftF[CopyManagerOp, B](ForceR(fa, fb))
  def uncancelable[A](body: Poll[CopyManagerIO] => CopyManagerIO[A]) = FF.liftF[CopyManagerOp, A](Uncancelable(body))
  def capturePoll[M[_]](mpoll: Poll[M]) = new Poll[CopyManagerIO] {
    def apply[A](fa: CopyManagerIO[A]) = FF.liftF[CopyManagerOp, A](Poll1(mpoll, fa))
  }
  val canceled = FF.liftF[CopyManagerOp, Unit](Canceled)
  def onCancel[A](fa: CopyManagerIO[A], fin: CopyManagerIO[Unit]) = FF.liftF[CopyManagerOp, A](OnCancel(fa, fin))
  def fromFuture[A](fut: CopyManagerIO[Future[A]]) = FF.liftF[CopyManagerOp, A](FromFuture(fut))
  def fromFutureCancelable[A](fut: CopyManagerIO[(Future[A], CopyManagerIO[Unit])]) = FF.liftF[CopyManagerOp, A](FromFutureCancelable(fut))
  def cancelable[A](fa: CopyManagerIO[A], fin: CopyManagerIO[Unit]) = FF.liftF[CopyManagerOp, A](Cancelable(fa, fin))
  def performLogging(event: LogEvent) = FF.liftF[CopyManagerOp, Unit](PerformLogging(event))

  // Smart constructors for CopyManager-specific operations.
  def copyDual(a: String): CopyManagerIO[PGCopyDual] = FF.liftF(CopyDual(a))
  def copyIn(a: String): CopyManagerIO[PGCopyIn] = FF.liftF(CopyIn(a))
  def copyIn(a: String, b: ByteStreamWriter): CopyManagerIO[Long] = FF.liftF(CopyIn1(a, b))
  def copyIn(a: String, b: InputStream): CopyManagerIO[Long] = FF.liftF(CopyIn2(a, b))
  def copyIn(a: String, b: InputStream, c: Int): CopyManagerIO[Long] = FF.liftF(CopyIn3(a, b, c))
  def copyIn(a: String, b: Reader): CopyManagerIO[Long] = FF.liftF(CopyIn4(a, b))
  def copyIn(a: String, b: Reader, c: Int): CopyManagerIO[Long] = FF.liftF(CopyIn5(a, b, c))
  def copyOut(a: String): CopyManagerIO[PGCopyOut] = FF.liftF(CopyOut(a))
  def copyOut(a: String, b: OutputStream): CopyManagerIO[Long] = FF.liftF(CopyOut1(a, b))
  def copyOut(a: String, b: Writer): CopyManagerIO[Long] = FF.liftF(CopyOut2(a, b))

  // Typeclass instances for CopyManagerIO
  implicit val WeakAsyncCopyManagerIO: WeakAsync[CopyManagerIO] =
    new WeakAsync[CopyManagerIO] {
      val monad = FF.catsFreeMonadForFree[CopyManagerOp]
      override val applicative: Applicative[CopyManagerIO] = monad
      override val rootCancelScope: CancelScope = CancelScope.Cancelable
      override def pure[A](x: A): CopyManagerIO[A] = monad.pure(x)
      override def flatMap[A, B](fa: CopyManagerIO[A])(f: A => CopyManagerIO[B]): CopyManagerIO[B] = monad.flatMap(fa)(f)
      override def tailRecM[A, B](a: A)(f: A => CopyManagerIO[Either[A, B]]): CopyManagerIO[B] = monad.tailRecM(a)(f)
      override def raiseError[A](e: Throwable): CopyManagerIO[A] = module.raiseError(e)
      override def handleErrorWith[A](fa: CopyManagerIO[A])(f: Throwable => CopyManagerIO[A]): CopyManagerIO[A] = module.handleErrorWith(fa)(f)
      override def monotonic: CopyManagerIO[FiniteDuration] = module.monotonic
      override def realTime: CopyManagerIO[FiniteDuration] = module.realtime
      override def suspend[A](hint: Sync.Type)(thunk: => A): CopyManagerIO[A] = module.suspend(hint)(thunk)
      override def forceR[A, B](fa: CopyManagerIO[A])(fb: CopyManagerIO[B]): CopyManagerIO[B] = module.forceR(fa)(fb)
      override def uncancelable[A](body: Poll[CopyManagerIO] => CopyManagerIO[A]): CopyManagerIO[A] = module.uncancelable(body)
      override def canceled: CopyManagerIO[Unit] = module.canceled
      override def onCancel[A](fa: CopyManagerIO[A], fin: CopyManagerIO[Unit]): CopyManagerIO[A] = module.onCancel(fa, fin)
      override def fromFuture[A](fut: CopyManagerIO[Future[A]]): CopyManagerIO[A] = module.fromFuture(fut)
      override def fromFutureCancelable[A](fut: CopyManagerIO[(Future[A], CopyManagerIO[Unit])]): CopyManagerIO[A] = module.fromFutureCancelable(fut)
      override def cancelable[A](fa: CopyManagerIO[A], fin: CopyManagerIO[Unit]): CopyManagerIO[A] = module.cancelable(fa, fin)
    }
    
  implicit def MonoidCopyManagerIO[A : Monoid]: Monoid[CopyManagerIO[A]] = new Monoid[CopyManagerIO[A]] {
    override def empty: CopyManagerIO[A] = Applicative[CopyManagerIO].pure(Monoid[A].empty)
    override def combine(x: CopyManagerIO[A], y: CopyManagerIO[A]): CopyManagerIO[A] =
      Applicative[CopyManagerIO].product(x, y).map { case (x, y) => Monoid[A].combine(x, y) }
  }
 
  implicit def SemigroupCopyManagerIO[A : Semigroup]: Semigroup[CopyManagerIO[A]] = new Semigroup[CopyManagerIO[A]] {
    override def combine(x: CopyManagerIO[A], y: CopyManagerIO[A]): CopyManagerIO[A] =
      Applicative[CopyManagerIO].product(x, y).map { case (x, y) => Semigroup[A].combine(x, y) }
  }
}

