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

import org.postgresql.largeobject.LargeObject
import org.postgresql.largeobject.LargeObjectManager

// This file is Auto-generated using FreeGen2.scala
object largeobjectmanager { module =>

  // Algebra of operations for LargeObjectManager. Each accepts a visitor as an alternative to pattern-matching.
  sealed trait LargeObjectManagerOp[A] {
    def visit[F[_]](v: LargeObjectManagerOp.Visitor[F]): F[A]
  }

  // Free monad over LargeObjectManagerOp.
  type LargeObjectManagerIO[A] = FF[LargeObjectManagerOp, A]

  // Module of instances and constructors of LargeObjectManagerOp.
  object LargeObjectManagerOp {

    // Given a LargeObjectManager we can embed a LargeObjectManagerIO program in any algebra that understands embedding.
    implicit val LargeObjectManagerOpEmbeddable: Embeddable[LargeObjectManagerOp, LargeObjectManager] =
      new Embeddable[LargeObjectManagerOp, LargeObjectManager] {
        def embed[A](j: LargeObjectManager, fa: FF[LargeObjectManagerOp, A]): Embedded.LargeObjectManager[A] = Embedded.LargeObjectManager(j, fa)
      }

    // Interface for a natural transformation LargeObjectManagerOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (LargeObjectManagerOp ~> F) {
      final def apply[A](fa: LargeObjectManagerOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](f: LargeObjectManager => A): F[A]
      def embed[A](e: Embedded[A]): F[A]
      def raiseError[A](e: Throwable): F[A]
      def handleErrorWith[A](fa: LargeObjectManagerIO[A])(f: Throwable => LargeObjectManagerIO[A]): F[A]
      def monotonic: F[FiniteDuration]
      def realTime: F[FiniteDuration]
      def delay[A](thunk: => A): F[A]
      def suspend[A](hint: Sync.Type)(thunk: => A): F[A]
      def forceR[A, B](fa: LargeObjectManagerIO[A])(fb: LargeObjectManagerIO[B]): F[B]
      def uncancelable[A](body: Poll[LargeObjectManagerIO] => LargeObjectManagerIO[A]): F[A]
      def poll[A](poll: Any, fa: LargeObjectManagerIO[A]): F[A]
      def canceled: F[Unit]
      def onCancel[A](fa: LargeObjectManagerIO[A], fin: LargeObjectManagerIO[Unit]): F[A]
      def fromFuture[A](fut: LargeObjectManagerIO[Future[A]]): F[A]
      def fromFutureCancelable[A](fut: LargeObjectManagerIO[(Future[A], LargeObjectManagerIO[Unit])]): F[A]
      def cancelable[A](fa: LargeObjectManagerIO[A], fin: LargeObjectManagerIO[Unit]): F[A]
      def performLogging(event: LogEvent): F[Unit]

      // LargeObjectManager
      def createLO: F[Long]
      def createLO(a: Int): F[Long]
      def delete(a: Long): F[Unit]
      def open(a: Int, b: Boolean): F[LargeObject]
      def open(a: Int, b: Int, c: Boolean): F[LargeObject]
      def open(a: Long): F[LargeObject]
      def open(a: Long, b: Boolean): F[LargeObject]
      def open(a: Long, b: Int): F[LargeObject]
      def open(a: Long, b: Int, c: Boolean): F[LargeObject]
      def unlink(a: Long): F[Unit]

    }

    // Common operations for all algebras.
    final case class Raw[A](f: LargeObjectManager => A) extends LargeObjectManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(f)
    }
    final case class Embed[A](e: Embedded[A]) extends LargeObjectManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class RaiseError[A](e: Throwable) extends LargeObjectManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raiseError(e)
    }
    final case class HandleErrorWith[A](fa: LargeObjectManagerIO[A], f: Throwable => LargeObjectManagerIO[A]) extends LargeObjectManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa)(f)
    }
    case object Monotonic extends LargeObjectManagerOp[FiniteDuration] {
      def visit[F[_]](v: Visitor[F]) = v.monotonic
    }
    case object Realtime extends LargeObjectManagerOp[FiniteDuration] {
      def visit[F[_]](v: Visitor[F]) = v.realTime
    }
    case class Suspend[A](hint: Sync.Type, thunk: () => A) extends LargeObjectManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.suspend(hint)(thunk())
    }
    case class ForceR[A, B](fa: LargeObjectManagerIO[A], fb: LargeObjectManagerIO[B]) extends LargeObjectManagerOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.forceR(fa)(fb)
    }
    case class Uncancelable[A](body: Poll[LargeObjectManagerIO] => LargeObjectManagerIO[A]) extends LargeObjectManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.uncancelable(body)
    }
    case class Poll1[A](poll: Any, fa: LargeObjectManagerIO[A]) extends LargeObjectManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.poll(poll, fa)
    }
    case object Canceled extends LargeObjectManagerOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.canceled
    }
    case class OnCancel[A](fa: LargeObjectManagerIO[A], fin: LargeObjectManagerIO[Unit]) extends LargeObjectManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.onCancel(fa, fin)
    }
    case class FromFuture[A](fut: LargeObjectManagerIO[Future[A]]) extends LargeObjectManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.fromFuture(fut)
    }
    case class FromFutureCancelable[A](fut: LargeObjectManagerIO[(Future[A], LargeObjectManagerIO[Unit])]) extends LargeObjectManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.fromFutureCancelable(fut)
    }
    case class Cancelable[A](fa: LargeObjectManagerIO[A], fin: LargeObjectManagerIO[Unit]) extends LargeObjectManagerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.cancelable(fa, fin)
    }
    case class PerformLogging(event: LogEvent) extends LargeObjectManagerOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.performLogging(event)
    }

    // LargeObjectManager-specific operations.
    case object CreateLO extends LargeObjectManagerOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.createLO
    }
    final case class CreateLO1(a: Int) extends LargeObjectManagerOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.createLO(a)
    }
    final case class Delete(a: Long) extends LargeObjectManagerOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.delete(a)
    }
    final case class Open(a: Int, b: Boolean) extends LargeObjectManagerOp[LargeObject] {
      def visit[F[_]](v: Visitor[F]) = v.open(a, b)
    }
    final case class Open1(a: Int, b: Int, c: Boolean) extends LargeObjectManagerOp[LargeObject] {
      def visit[F[_]](v: Visitor[F]) = v.open(a, b, c)
    }
    final case class Open2(a: Long) extends LargeObjectManagerOp[LargeObject] {
      def visit[F[_]](v: Visitor[F]) = v.open(a)
    }
    final case class Open3(a: Long, b: Boolean) extends LargeObjectManagerOp[LargeObject] {
      def visit[F[_]](v: Visitor[F]) = v.open(a, b)
    }
    final case class Open4(a: Long, b: Int) extends LargeObjectManagerOp[LargeObject] {
      def visit[F[_]](v: Visitor[F]) = v.open(a, b)
    }
    final case class Open5(a: Long, b: Int, c: Boolean) extends LargeObjectManagerOp[LargeObject] {
      def visit[F[_]](v: Visitor[F]) = v.open(a, b, c)
    }
    final case class Unlink(a: Long) extends LargeObjectManagerOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.unlink(a)
    }

  }
  import LargeObjectManagerOp.*

  // Smart constructors for operations common to all algebras.
  val unit: LargeObjectManagerIO[Unit] = FF.pure[LargeObjectManagerOp, Unit](())
  def pure[A](a: A): LargeObjectManagerIO[A] = FF.pure[LargeObjectManagerOp, A](a)
  def raw[A](f: LargeObjectManager => A): LargeObjectManagerIO[A] = FF.liftF(Raw(f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[LargeObjectManagerOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def raiseError[A](err: Throwable): LargeObjectManagerIO[A] = FF.liftF[LargeObjectManagerOp, A](RaiseError(err))
  def handleErrorWith[A](fa: LargeObjectManagerIO[A])(f: Throwable => LargeObjectManagerIO[A]): LargeObjectManagerIO[A] = FF.liftF[LargeObjectManagerOp, A](HandleErrorWith(fa, f))
  val monotonic = FF.liftF[LargeObjectManagerOp, FiniteDuration](Monotonic)
  val realtime = FF.liftF[LargeObjectManagerOp, FiniteDuration](Realtime)
  def delay[A](thunk: => A) = FF.liftF[LargeObjectManagerOp, A](Suspend(Sync.Type.Delay, () => thunk))
  def suspend[A](hint: Sync.Type)(thunk: => A) = FF.liftF[LargeObjectManagerOp, A](Suspend(hint, () => thunk))
  def forceR[A, B](fa: LargeObjectManagerIO[A])(fb: LargeObjectManagerIO[B]) = FF.liftF[LargeObjectManagerOp, B](ForceR(fa, fb))
  def uncancelable[A](body: Poll[LargeObjectManagerIO] => LargeObjectManagerIO[A]) = FF.liftF[LargeObjectManagerOp, A](Uncancelable(body))
  def capturePoll[M[_]](mpoll: Poll[M]) = new Poll[LargeObjectManagerIO] {
    def apply[A](fa: LargeObjectManagerIO[A]) = FF.liftF[LargeObjectManagerOp, A](Poll1(mpoll, fa))
  }
  val canceled = FF.liftF[LargeObjectManagerOp, Unit](Canceled)
  def onCancel[A](fa: LargeObjectManagerIO[A], fin: LargeObjectManagerIO[Unit]) = FF.liftF[LargeObjectManagerOp, A](OnCancel(fa, fin))
  def fromFuture[A](fut: LargeObjectManagerIO[Future[A]]) = FF.liftF[LargeObjectManagerOp, A](FromFuture(fut))
  def fromFutureCancelable[A](fut: LargeObjectManagerIO[(Future[A], LargeObjectManagerIO[Unit])]) = FF.liftF[LargeObjectManagerOp, A](FromFutureCancelable(fut))
  def cancelable[A](fa: LargeObjectManagerIO[A], fin: LargeObjectManagerIO[Unit]) = FF.liftF[LargeObjectManagerOp, A](Cancelable(fa, fin))
  def performLogging(event: LogEvent) = FF.liftF[LargeObjectManagerOp, Unit](PerformLogging(event))

  // Smart constructors for LargeObjectManager-specific operations.
  val createLO: LargeObjectManagerIO[Long] = FF.liftF(CreateLO)
  def createLO(a: Int): LargeObjectManagerIO[Long] = FF.liftF(CreateLO1(a))
  def delete(a: Long): LargeObjectManagerIO[Unit] = FF.liftF(Delete(a))
  def open(a: Int, b: Boolean): LargeObjectManagerIO[LargeObject] = FF.liftF(Open(a, b))
  def open(a: Int, b: Int, c: Boolean): LargeObjectManagerIO[LargeObject] = FF.liftF(Open1(a, b, c))
  def open(a: Long): LargeObjectManagerIO[LargeObject] = FF.liftF(Open2(a))
  def open(a: Long, b: Boolean): LargeObjectManagerIO[LargeObject] = FF.liftF(Open3(a, b))
  def open(a: Long, b: Int): LargeObjectManagerIO[LargeObject] = FF.liftF(Open4(a, b))
  def open(a: Long, b: Int, c: Boolean): LargeObjectManagerIO[LargeObject] = FF.liftF(Open5(a, b, c))
  def unlink(a: Long): LargeObjectManagerIO[Unit] = FF.liftF(Unlink(a))

  // Typeclass instances for LargeObjectManagerIO
  implicit val WeakAsyncLargeObjectManagerIO: WeakAsync[LargeObjectManagerIO] =
    new WeakAsync[LargeObjectManagerIO] {
      val monad = FF.catsFreeMonadForFree[LargeObjectManagerOp]
      override val applicative: Applicative[LargeObjectManagerIO] = monad
      override val rootCancelScope: CancelScope = CancelScope.Cancelable
      override def pure[A](x: A): LargeObjectManagerIO[A] = monad.pure(x)
      override def flatMap[A, B](fa: LargeObjectManagerIO[A])(f: A => LargeObjectManagerIO[B]): LargeObjectManagerIO[B] = monad.flatMap(fa)(f)
      override def tailRecM[A, B](a: A)(f: A => LargeObjectManagerIO[Either[A, B]]): LargeObjectManagerIO[B] = monad.tailRecM(a)(f)
      override def raiseError[A](e: Throwable): LargeObjectManagerIO[A] = module.raiseError(e)
      override def handleErrorWith[A](fa: LargeObjectManagerIO[A])(f: Throwable => LargeObjectManagerIO[A]): LargeObjectManagerIO[A] = module.handleErrorWith(fa)(f)
      override def monotonic: LargeObjectManagerIO[FiniteDuration] = module.monotonic
      override def realTime: LargeObjectManagerIO[FiniteDuration] = module.realtime
      override def suspend[A](hint: Sync.Type)(thunk: => A): LargeObjectManagerIO[A] = module.suspend(hint)(thunk)
      override def forceR[A, B](fa: LargeObjectManagerIO[A])(fb: LargeObjectManagerIO[B]): LargeObjectManagerIO[B] = module.forceR(fa)(fb)
      override def uncancelable[A](body: Poll[LargeObjectManagerIO] => LargeObjectManagerIO[A]): LargeObjectManagerIO[A] = module.uncancelable(body)
      override def canceled: LargeObjectManagerIO[Unit] = module.canceled
      override def onCancel[A](fa: LargeObjectManagerIO[A], fin: LargeObjectManagerIO[Unit]): LargeObjectManagerIO[A] = module.onCancel(fa, fin)
      override def fromFuture[A](fut: LargeObjectManagerIO[Future[A]]): LargeObjectManagerIO[A] = module.fromFuture(fut)
      override def fromFutureCancelable[A](fut: LargeObjectManagerIO[(Future[A], LargeObjectManagerIO[Unit])]): LargeObjectManagerIO[A] = module.fromFutureCancelable(fut)
      override def cancelable[A](fa: LargeObjectManagerIO[A], fin: LargeObjectManagerIO[Unit]): LargeObjectManagerIO[A] = module.cancelable(fa, fin)
    }
    
  implicit def MonoidLargeObjectManagerIO[A : Monoid]: Monoid[LargeObjectManagerIO[A]] = new Monoid[LargeObjectManagerIO[A]] {
    override def empty: LargeObjectManagerIO[A] = Applicative[LargeObjectManagerIO].pure(Monoid[A].empty)
    override def combine(x: LargeObjectManagerIO[A], y: LargeObjectManagerIO[A]): LargeObjectManagerIO[A] =
      Applicative[LargeObjectManagerIO].product(x, y).map { case (x, y) => Monoid[A].combine(x, y) }
  }
 
  implicit def SemigroupLargeObjectManagerIO[A : Semigroup]: Semigroup[LargeObjectManagerIO[A]] = new Semigroup[LargeObjectManagerIO[A]] {
    override def combine(x: LargeObjectManagerIO[A], y: LargeObjectManagerIO[A]): LargeObjectManagerIO[A] =
      Applicative[LargeObjectManagerIO].product(x, y).map { case (x, y) => Semigroup[A].combine(x, y) }
  }
}

