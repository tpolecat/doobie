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

import java.lang.String
import java.sql.SQLData
import java.sql.SQLInput
import java.sql.SQLOutput

// This file is Auto-generated using FreeGen2.scala
object sqldata { module =>

  // Algebra of operations for SQLData. Each accepts a visitor as an alternative to pattern-matching.
  sealed trait SQLDataOp[A] {
    def visit[F[_]](v: SQLDataOp.Visitor[F]): F[A]
  }

  // Free monad over SQLDataOp.
  type SQLDataIO[A] = FF[SQLDataOp, A]

  // Module of instances and constructors of SQLDataOp.
  object SQLDataOp {

    // Given a SQLData we can embed a SQLDataIO program in any algebra that understands embedding.
    implicit val SQLDataOpEmbeddable: Embeddable[SQLDataOp, SQLData] =
      new Embeddable[SQLDataOp, SQLData] {
        def embed[A](j: SQLData, fa: FF[SQLDataOp, A]): Embedded.SQLData[A] = Embedded.SQLData(j, fa)
      }

    // Interface for a natural transformation SQLDataOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (SQLDataOp ~> F) {
      final def apply[A](fa: SQLDataOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](f: SQLData => A): F[A]
      def embed[A](e: Embedded[A]): F[A]
      def raiseError[A](e: Throwable): F[A]
      def handleErrorWith[A](fa: SQLDataIO[A])(f: Throwable => SQLDataIO[A]): F[A]
      def monotonic: F[FiniteDuration]
      def realTime: F[FiniteDuration]
      def delay[A](thunk: => A): F[A]
      def suspend[A](hint: Sync.Type)(thunk: => A): F[A]
      def forceR[A, B](fa: SQLDataIO[A])(fb: SQLDataIO[B]): F[B]
      def uncancelable[A](body: Poll[SQLDataIO] => SQLDataIO[A]): F[A]
      def poll[A](poll: Any, fa: SQLDataIO[A]): F[A]
      def canceled: F[Unit]
      def onCancel[A](fa: SQLDataIO[A], fin: SQLDataIO[Unit]): F[A]
      def fromFuture[A](fut: SQLDataIO[Future[A]]): F[A]
      def fromFutureCancelable[A](fut: SQLDataIO[(Future[A], SQLDataIO[Unit])]): F[A]
      def cancelable[A](fa: SQLDataIO[A], fin: SQLDataIO[Unit]): F[A]
      def performLogging(event: LogEvent): F[Unit]

      // SQLData
      def getSQLTypeName: F[String]
      def readSQL(a: SQLInput, b: String): F[Unit]
      def writeSQL(a: SQLOutput): F[Unit]

    }

    // Common operations for all algebras.
    final case class Raw[A](f: SQLData => A) extends SQLDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(f)
    }
    final case class Embed[A](e: Embedded[A]) extends SQLDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class RaiseError[A](e: Throwable) extends SQLDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raiseError(e)
    }
    final case class HandleErrorWith[A](fa: SQLDataIO[A], f: Throwable => SQLDataIO[A]) extends SQLDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa)(f)
    }
    case object Monotonic extends SQLDataOp[FiniteDuration] {
      def visit[F[_]](v: Visitor[F]) = v.monotonic
    }
    case object Realtime extends SQLDataOp[FiniteDuration] {
      def visit[F[_]](v: Visitor[F]) = v.realTime
    }
    case class Suspend[A](hint: Sync.Type, thunk: () => A) extends SQLDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.suspend(hint)(thunk())
    }
    case class ForceR[A, B](fa: SQLDataIO[A], fb: SQLDataIO[B]) extends SQLDataOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.forceR(fa)(fb)
    }
    case class Uncancelable[A](body: Poll[SQLDataIO] => SQLDataIO[A]) extends SQLDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.uncancelable(body)
    }
    case class Poll1[A](poll: Any, fa: SQLDataIO[A]) extends SQLDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.poll(poll, fa)
    }
    case object Canceled extends SQLDataOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.canceled
    }
    case class OnCancel[A](fa: SQLDataIO[A], fin: SQLDataIO[Unit]) extends SQLDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.onCancel(fa, fin)
    }
    case class FromFuture[A](fut: SQLDataIO[Future[A]]) extends SQLDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.fromFuture(fut)
    }
    case class FromFutureCancelable[A](fut: SQLDataIO[(Future[A], SQLDataIO[Unit])]) extends SQLDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.fromFutureCancelable(fut)
    }
    case class Cancelable[A](fa: SQLDataIO[A], fin: SQLDataIO[Unit]) extends SQLDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.cancelable(fa, fin)
    }
    case class PerformLogging(event: LogEvent) extends SQLDataOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.performLogging(event)
    }

    // SQLData-specific operations.
    case object GetSQLTypeName extends SQLDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getSQLTypeName
    }
    final case class ReadSQL(a: SQLInput, b: String) extends SQLDataOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.readSQL(a, b)
    }
    final case class WriteSQL(a: SQLOutput) extends SQLDataOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeSQL(a)
    }

  }
  import SQLDataOp.*

  // Smart constructors for operations common to all algebras.
  val unit: SQLDataIO[Unit] = FF.pure[SQLDataOp, Unit](())
  def pure[A](a: A): SQLDataIO[A] = FF.pure[SQLDataOp, A](a)
  def raw[A](f: SQLData => A): SQLDataIO[A] = FF.liftF(Raw(f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[SQLDataOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def raiseError[A](err: Throwable): SQLDataIO[A] = FF.liftF[SQLDataOp, A](RaiseError(err))
  def handleErrorWith[A](fa: SQLDataIO[A])(f: Throwable => SQLDataIO[A]): SQLDataIO[A] = FF.liftF[SQLDataOp, A](HandleErrorWith(fa, f))
  val monotonic = FF.liftF[SQLDataOp, FiniteDuration](Monotonic)
  val realtime = FF.liftF[SQLDataOp, FiniteDuration](Realtime)
  def delay[A](thunk: => A) = FF.liftF[SQLDataOp, A](Suspend(Sync.Type.Delay, () => thunk))
  def suspend[A](hint: Sync.Type)(thunk: => A) = FF.liftF[SQLDataOp, A](Suspend(hint, () => thunk))
  def forceR[A, B](fa: SQLDataIO[A])(fb: SQLDataIO[B]) = FF.liftF[SQLDataOp, B](ForceR(fa, fb))
  def uncancelable[A](body: Poll[SQLDataIO] => SQLDataIO[A]) = FF.liftF[SQLDataOp, A](Uncancelable(body))
  def capturePoll[M[_]](mpoll: Poll[M]) = new Poll[SQLDataIO] {
    def apply[A](fa: SQLDataIO[A]) = FF.liftF[SQLDataOp, A](Poll1(mpoll, fa))
  }
  val canceled = FF.liftF[SQLDataOp, Unit](Canceled)
  def onCancel[A](fa: SQLDataIO[A], fin: SQLDataIO[Unit]) = FF.liftF[SQLDataOp, A](OnCancel(fa, fin))
  def fromFuture[A](fut: SQLDataIO[Future[A]]) = FF.liftF[SQLDataOp, A](FromFuture(fut))
  def fromFutureCancelable[A](fut: SQLDataIO[(Future[A], SQLDataIO[Unit])]) = FF.liftF[SQLDataOp, A](FromFutureCancelable(fut))
  def cancelable[A](fa: SQLDataIO[A], fin: SQLDataIO[Unit]) = FF.liftF[SQLDataOp, A](Cancelable(fa, fin))
  def performLogging(event: LogEvent) = FF.liftF[SQLDataOp, Unit](PerformLogging(event))

  // Smart constructors for SQLData-specific operations.
  val getSQLTypeName: SQLDataIO[String] = FF.liftF(GetSQLTypeName)
  def readSQL(a: SQLInput, b: String): SQLDataIO[Unit] = FF.liftF(ReadSQL(a, b))
  def writeSQL(a: SQLOutput): SQLDataIO[Unit] = FF.liftF(WriteSQL(a))

  // Typeclass instances for SQLDataIO
  implicit val WeakAsyncSQLDataIO: WeakAsync[SQLDataIO] =
    new WeakAsync[SQLDataIO] {
      val monad = FF.catsFreeMonadForFree[SQLDataOp]
      override val applicative: Applicative[SQLDataIO] = monad
      override val rootCancelScope: CancelScope = CancelScope.Cancelable
      override def pure[A](x: A): SQLDataIO[A] = monad.pure(x)
      override def flatMap[A, B](fa: SQLDataIO[A])(f: A => SQLDataIO[B]): SQLDataIO[B] = monad.flatMap(fa)(f)
      override def tailRecM[A, B](a: A)(f: A => SQLDataIO[Either[A, B]]): SQLDataIO[B] = monad.tailRecM(a)(f)
      override def raiseError[A](e: Throwable): SQLDataIO[A] = module.raiseError(e)
      override def handleErrorWith[A](fa: SQLDataIO[A])(f: Throwable => SQLDataIO[A]): SQLDataIO[A] = module.handleErrorWith(fa)(f)
      override def monotonic: SQLDataIO[FiniteDuration] = module.monotonic
      override def realTime: SQLDataIO[FiniteDuration] = module.realtime
      override def suspend[A](hint: Sync.Type)(thunk: => A): SQLDataIO[A] = module.suspend(hint)(thunk)
      override def forceR[A, B](fa: SQLDataIO[A])(fb: SQLDataIO[B]): SQLDataIO[B] = module.forceR(fa)(fb)
      override def uncancelable[A](body: Poll[SQLDataIO] => SQLDataIO[A]): SQLDataIO[A] = module.uncancelable(body)
      override def canceled: SQLDataIO[Unit] = module.canceled
      override def onCancel[A](fa: SQLDataIO[A], fin: SQLDataIO[Unit]): SQLDataIO[A] = module.onCancel(fa, fin)
      override def fromFuture[A](fut: SQLDataIO[Future[A]]): SQLDataIO[A] = module.fromFuture(fut)
      override def fromFutureCancelable[A](fut: SQLDataIO[(Future[A], SQLDataIO[Unit])]): SQLDataIO[A] = module.fromFutureCancelable(fut)
      override def cancelable[A](fa: SQLDataIO[A], fin: SQLDataIO[Unit]): SQLDataIO[A] = module.cancelable(fa, fin)
    }
    
  implicit def MonoidSQLDataIO[A : Monoid]: Monoid[SQLDataIO[A]] = new Monoid[SQLDataIO[A]] {
    override def empty: SQLDataIO[A] = Applicative[SQLDataIO].pure(Monoid[A].empty)
    override def combine(x: SQLDataIO[A], y: SQLDataIO[A]): SQLDataIO[A] =
      Applicative[SQLDataIO].product(x, y).map { case (x, y) => Monoid[A].combine(x, y) }
  }
 
  implicit def SemigroupSQLDataIO[A : Semigroup]: Semigroup[SQLDataIO[A]] = new Semigroup[SQLDataIO[A]] {
    override def combine(x: SQLDataIO[A], y: SQLDataIO[A]): SQLDataIO[A] =
      Applicative[SQLDataIO].product(x, y).map { case (x, y) => Semigroup[A].combine(x, y) }
  }
}

