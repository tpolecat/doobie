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

import org.postgresql.copy.{ CopyIn as PGCopyIn }
import org.postgresql.util.ByteStreamWriter

// This file is Auto-generated using FreeGen2.scala
object copyin { module =>

  // Algebra of operations for PGCopyIn. Each accepts a visitor as an alternative to pattern-matching.
  sealed trait CopyInOp[A] {
    def visit[F[_]](v: CopyInOp.Visitor[F]): F[A]
  }

  // Free monad over CopyInOp.
  type CopyInIO[A] = FF[CopyInOp, A]

  // Module of instances and constructors of CopyInOp.
  object CopyInOp {

    // Given a PGCopyIn we can embed a CopyInIO program in any algebra that understands embedding.
    implicit val CopyInOpEmbeddable: Embeddable[CopyInOp, PGCopyIn] =
      new Embeddable[CopyInOp, PGCopyIn] {
        def embed[A](j: PGCopyIn, fa: FF[CopyInOp, A]): Embedded.CopyIn[A] = Embedded.CopyIn(j, fa)
      }

    // Interface for a natural transformation CopyInOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (CopyInOp ~> F) {
      final def apply[A](fa: CopyInOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](f: PGCopyIn => A): F[A]
      def embed[A](e: Embedded[A]): F[A]
      def raiseError[A](e: Throwable): F[A]
      def handleErrorWith[A](fa: CopyInIO[A])(f: Throwable => CopyInIO[A]): F[A]
      def monotonic: F[FiniteDuration]
      def realTime: F[FiniteDuration]
      def delay[A](thunk: => A): F[A]
      def suspend[A](hint: Sync.Type)(thunk: => A): F[A]
      def forceR[A, B](fa: CopyInIO[A])(fb: CopyInIO[B]): F[B]
      def uncancelable[A](body: Poll[CopyInIO] => CopyInIO[A]): F[A]
      def poll[A](poll: Any, fa: CopyInIO[A]): F[A]
      def canceled: F[Unit]
      def onCancel[A](fa: CopyInIO[A], fin: CopyInIO[Unit]): F[A]
      def fromFuture[A](fut: CopyInIO[Future[A]]): F[A]
      def fromFutureCancelable[A](fut: CopyInIO[(Future[A], CopyInIO[Unit])]): F[A]
      def cancelable[A](fa: CopyInIO[A], fin: CopyInIO[Unit]): F[A]
      def performLogging(event: LogEvent): F[Unit]

      // PGCopyIn
      def cancelCopy: F[Unit]
      def endCopy: F[Long]
      def flushCopy: F[Unit]
      def getFieldCount: F[Int]
      def getFieldFormat(a: Int): F[Int]
      def getFormat: F[Int]
      def getHandledRowCount: F[Long]
      def isActive: F[Boolean]
      def writeToCopy(a: Array[Byte], b: Int, c: Int): F[Unit]
      def writeToCopy(a: ByteStreamWriter): F[Unit]

    }

    // Common operations for all algebras.
    final case class Raw[A](f: PGCopyIn => A) extends CopyInOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(f)
    }
    final case class Embed[A](e: Embedded[A]) extends CopyInOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class RaiseError[A](e: Throwable) extends CopyInOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raiseError(e)
    }
    final case class HandleErrorWith[A](fa: CopyInIO[A], f: Throwable => CopyInIO[A]) extends CopyInOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa)(f)
    }
    case object Monotonic extends CopyInOp[FiniteDuration] {
      def visit[F[_]](v: Visitor[F]) = v.monotonic
    }
    case object Realtime extends CopyInOp[FiniteDuration] {
      def visit[F[_]](v: Visitor[F]) = v.realTime
    }
    case class Suspend[A](hint: Sync.Type, thunk: () => A) extends CopyInOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.suspend(hint)(thunk())
    }
    case class ForceR[A, B](fa: CopyInIO[A], fb: CopyInIO[B]) extends CopyInOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.forceR(fa)(fb)
    }
    case class Uncancelable[A](body: Poll[CopyInIO] => CopyInIO[A]) extends CopyInOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.uncancelable(body)
    }
    case class Poll1[A](poll: Any, fa: CopyInIO[A]) extends CopyInOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.poll(poll, fa)
    }
    case object Canceled extends CopyInOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.canceled
    }
    case class OnCancel[A](fa: CopyInIO[A], fin: CopyInIO[Unit]) extends CopyInOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.onCancel(fa, fin)
    }
    case class FromFuture[A](fut: CopyInIO[Future[A]]) extends CopyInOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.fromFuture(fut)
    }
    case class FromFutureCancelable[A](fut: CopyInIO[(Future[A], CopyInIO[Unit])]) extends CopyInOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.fromFutureCancelable(fut)
    }
    case class Cancelable[A](fa: CopyInIO[A], fin: CopyInIO[Unit]) extends CopyInOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.cancelable(fa, fin)
    }
    case class PerformLogging(event: LogEvent) extends CopyInOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.performLogging(event)
    }

    // PGCopyIn-specific operations.
    case object CancelCopy extends CopyInOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.cancelCopy
    }
    case object EndCopy extends CopyInOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.endCopy
    }
    case object FlushCopy extends CopyInOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.flushCopy
    }
    case object GetFieldCount extends CopyInOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getFieldCount
    }
    final case class GetFieldFormat(a: Int) extends CopyInOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getFieldFormat(a)
    }
    case object GetFormat extends CopyInOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getFormat
    }
    case object GetHandledRowCount extends CopyInOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.getHandledRowCount
    }
    case object IsActive extends CopyInOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isActive
    }
    final case class WriteToCopy(a: Array[Byte], b: Int, c: Int) extends CopyInOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeToCopy(a, b, c)
    }
    final case class WriteToCopy1(a: ByteStreamWriter) extends CopyInOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeToCopy(a)
    }

  }
  import CopyInOp.*

  // Smart constructors for operations common to all algebras.
  val unit: CopyInIO[Unit] = FF.pure[CopyInOp, Unit](())
  def pure[A](a: A): CopyInIO[A] = FF.pure[CopyInOp, A](a)
  def raw[A](f: PGCopyIn => A): CopyInIO[A] = FF.liftF(Raw(f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[CopyInOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def raiseError[A](err: Throwable): CopyInIO[A] = FF.liftF[CopyInOp, A](RaiseError(err))
  def handleErrorWith[A](fa: CopyInIO[A])(f: Throwable => CopyInIO[A]): CopyInIO[A] = FF.liftF[CopyInOp, A](HandleErrorWith(fa, f))
  val monotonic = FF.liftF[CopyInOp, FiniteDuration](Monotonic)
  val realtime = FF.liftF[CopyInOp, FiniteDuration](Realtime)
  def delay[A](thunk: => A) = FF.liftF[CopyInOp, A](Suspend(Sync.Type.Delay, () => thunk))
  def suspend[A](hint: Sync.Type)(thunk: => A) = FF.liftF[CopyInOp, A](Suspend(hint, () => thunk))
  def forceR[A, B](fa: CopyInIO[A])(fb: CopyInIO[B]) = FF.liftF[CopyInOp, B](ForceR(fa, fb))
  def uncancelable[A](body: Poll[CopyInIO] => CopyInIO[A]) = FF.liftF[CopyInOp, A](Uncancelable(body))
  def capturePoll[M[_]](mpoll: Poll[M]) = new Poll[CopyInIO] {
    def apply[A](fa: CopyInIO[A]) = FF.liftF[CopyInOp, A](Poll1(mpoll, fa))
  }
  val canceled = FF.liftF[CopyInOp, Unit](Canceled)
  def onCancel[A](fa: CopyInIO[A], fin: CopyInIO[Unit]) = FF.liftF[CopyInOp, A](OnCancel(fa, fin))
  def fromFuture[A](fut: CopyInIO[Future[A]]) = FF.liftF[CopyInOp, A](FromFuture(fut))
  def fromFutureCancelable[A](fut: CopyInIO[(Future[A], CopyInIO[Unit])]) = FF.liftF[CopyInOp, A](FromFutureCancelable(fut))
  def cancelable[A](fa: CopyInIO[A], fin: CopyInIO[Unit]) = FF.liftF[CopyInOp, A](Cancelable(fa, fin))
  def performLogging(event: LogEvent) = FF.liftF[CopyInOp, Unit](PerformLogging(event))

  // Smart constructors for CopyIn-specific operations.
  val cancelCopy: CopyInIO[Unit] = FF.liftF(CancelCopy)
  val endCopy: CopyInIO[Long] = FF.liftF(EndCopy)
  val flushCopy: CopyInIO[Unit] = FF.liftF(FlushCopy)
  val getFieldCount: CopyInIO[Int] = FF.liftF(GetFieldCount)
  def getFieldFormat(a: Int): CopyInIO[Int] = FF.liftF(GetFieldFormat(a))
  val getFormat: CopyInIO[Int] = FF.liftF(GetFormat)
  val getHandledRowCount: CopyInIO[Long] = FF.liftF(GetHandledRowCount)
  val isActive: CopyInIO[Boolean] = FF.liftF(IsActive)
  def writeToCopy(a: Array[Byte], b: Int, c: Int): CopyInIO[Unit] = FF.liftF(WriteToCopy(a, b, c))
  def writeToCopy(a: ByteStreamWriter): CopyInIO[Unit] = FF.liftF(WriteToCopy1(a))

  // Typeclass instances for CopyInIO
  implicit val WeakAsyncCopyInIO: WeakAsync[CopyInIO] =
    new WeakAsync[CopyInIO] {
      val monad = FF.catsFreeMonadForFree[CopyInOp]
      override val applicative: Applicative[CopyInIO] = monad
      override val rootCancelScope: CancelScope = CancelScope.Cancelable
      override def pure[A](x: A): CopyInIO[A] = monad.pure(x)
      override def flatMap[A, B](fa: CopyInIO[A])(f: A => CopyInIO[B]): CopyInIO[B] = monad.flatMap(fa)(f)
      override def tailRecM[A, B](a: A)(f: A => CopyInIO[Either[A, B]]): CopyInIO[B] = monad.tailRecM(a)(f)
      override def raiseError[A](e: Throwable): CopyInIO[A] = module.raiseError(e)
      override def handleErrorWith[A](fa: CopyInIO[A])(f: Throwable => CopyInIO[A]): CopyInIO[A] = module.handleErrorWith(fa)(f)
      override def monotonic: CopyInIO[FiniteDuration] = module.monotonic
      override def realTime: CopyInIO[FiniteDuration] = module.realtime
      override def suspend[A](hint: Sync.Type)(thunk: => A): CopyInIO[A] = module.suspend(hint)(thunk)
      override def forceR[A, B](fa: CopyInIO[A])(fb: CopyInIO[B]): CopyInIO[B] = module.forceR(fa)(fb)
      override def uncancelable[A](body: Poll[CopyInIO] => CopyInIO[A]): CopyInIO[A] = module.uncancelable(body)
      override def canceled: CopyInIO[Unit] = module.canceled
      override def onCancel[A](fa: CopyInIO[A], fin: CopyInIO[Unit]): CopyInIO[A] = module.onCancel(fa, fin)
      override def fromFuture[A](fut: CopyInIO[Future[A]]): CopyInIO[A] = module.fromFuture(fut)
      override def fromFutureCancelable[A](fut: CopyInIO[(Future[A], CopyInIO[Unit])]): CopyInIO[A] = module.fromFutureCancelable(fut)
      override def cancelable[A](fa: CopyInIO[A], fin: CopyInIO[Unit]): CopyInIO[A] = module.cancelable(fa, fin)
    }
    
  implicit def MonoidCopyInIO[A : Monoid]: Monoid[CopyInIO[A]] = new Monoid[CopyInIO[A]] {
    override def empty: CopyInIO[A] = Applicative[CopyInIO].pure(Monoid[A].empty)
    override def combine(x: CopyInIO[A], y: CopyInIO[A]): CopyInIO[A] =
      Applicative[CopyInIO].product(x, y).map { case (x, y) => Monoid[A].combine(x, y) }
  }
 
  implicit def SemigroupCopyInIO[A : Semigroup]: Semigroup[CopyInIO[A]] = new Semigroup[CopyInIO[A]] {
    override def combine(x: CopyInIO[A], y: CopyInIO[A]): CopyInIO[A] =
      Applicative[CopyInIO].product(x, y).map { case (x, y) => Semigroup[A].combine(x, y) }
  }
}

