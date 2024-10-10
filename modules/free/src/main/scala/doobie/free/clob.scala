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

import java.io.InputStream
import java.io.OutputStream
import java.io.Reader
import java.io.Writer
import java.lang.String
import java.sql.Clob

// This file is Auto-generated using FreeGen2.scala
object clob { module =>

  // Algebra of operations for Clob. Each accepts a visitor as an alternative to pattern-matching.
  sealed trait ClobOp[A] {
    def visit[F[_]](v: ClobOp.Visitor[F]): F[A]
  }

  // Free monad over ClobOp.
  type ClobIO[A] = FF[ClobOp, A]

  // Module of instances and constructors of ClobOp.
  object ClobOp {

    // Given a Clob we can embed a ClobIO program in any algebra that understands embedding.
    implicit val ClobOpEmbeddable: Embeddable[ClobOp, Clob] =
      new Embeddable[ClobOp, Clob] {
        def embed[A](j: Clob, fa: FF[ClobOp, A]): Embedded.Clob[A] = Embedded.Clob(j, fa)
      }

    // Interface for a natural transformation ClobOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (ClobOp ~> F) {
      final def apply[A](fa: ClobOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](f: Clob => A): F[A]
      def embed[A](e: Embedded[A]): F[A]
      def raiseError[A](e: Throwable): F[A]
      def handleErrorWith[A](fa: ClobIO[A])(f: Throwable => ClobIO[A]): F[A]
      def monotonic: F[FiniteDuration]
      def realTime: F[FiniteDuration]
      def delay[A](thunk: => A): F[A]
      def suspend[A](hint: Sync.Type)(thunk: => A): F[A]
      def forceR[A, B](fa: ClobIO[A])(fb: ClobIO[B]): F[B]
      def uncancelable[A](body: Poll[ClobIO] => ClobIO[A]): F[A]
      def poll[A](poll: Any, fa: ClobIO[A]): F[A]
      def canceled: F[Unit]
      def onCancel[A](fa: ClobIO[A], fin: ClobIO[Unit]): F[A]
      def fromFuture[A](fut: ClobIO[Future[A]]): F[A]
      def fromFutureCancelable[A](fut: ClobIO[(Future[A], ClobIO[Unit])]): F[A]
      def cancelable[A](fa: ClobIO[A], fin: ClobIO[Unit]): F[A]
      def performLogging(event: LogEvent): F[Unit]

      // Clob
      def free: F[Unit]
      def getAsciiStream: F[InputStream]
      def getCharacterStream: F[Reader]
      def getCharacterStream(a: Long, b: Long): F[Reader]
      def getSubString(a: Long, b: Int): F[String]
      def length: F[Long]
      def position(a: Clob, b: Long): F[Long]
      def position(a: String, b: Long): F[Long]
      def setAsciiStream(a: Long): F[OutputStream]
      def setCharacterStream(a: Long): F[Writer]
      def setString(a: Long, b: String): F[Int]
      def setString(a: Long, b: String, c: Int, d: Int): F[Int]
      def truncate(a: Long): F[Unit]

    }

    // Common operations for all algebras.
    final case class Raw[A](f: Clob => A) extends ClobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(f)
    }
    final case class Embed[A](e: Embedded[A]) extends ClobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class RaiseError[A](e: Throwable) extends ClobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raiseError(e)
    }
    final case class HandleErrorWith[A](fa: ClobIO[A], f: Throwable => ClobIO[A]) extends ClobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa)(f)
    }
    case object Monotonic extends ClobOp[FiniteDuration] {
      def visit[F[_]](v: Visitor[F]) = v.monotonic
    }
    case object Realtime extends ClobOp[FiniteDuration] {
      def visit[F[_]](v: Visitor[F]) = v.realTime
    }
    case class Suspend[A](hint: Sync.Type, thunk: () => A) extends ClobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.suspend(hint)(thunk())
    }
    case class ForceR[A, B](fa: ClobIO[A], fb: ClobIO[B]) extends ClobOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.forceR(fa)(fb)
    }
    case class Uncancelable[A](body: Poll[ClobIO] => ClobIO[A]) extends ClobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.uncancelable(body)
    }
    case class Poll1[A](poll: Any, fa: ClobIO[A]) extends ClobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.poll(poll, fa)
    }
    case object Canceled extends ClobOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.canceled
    }
    case class OnCancel[A](fa: ClobIO[A], fin: ClobIO[Unit]) extends ClobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.onCancel(fa, fin)
    }
    case class FromFuture[A](fut: ClobIO[Future[A]]) extends ClobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.fromFuture(fut)
    }
    case class FromFutureCancelable[A](fut: ClobIO[(Future[A], ClobIO[Unit])]) extends ClobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.fromFutureCancelable(fut)
    }
    case class Cancelable[A](fa: ClobIO[A], fin: ClobIO[Unit]) extends ClobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.cancelable(fa, fin)
    }
    case class PerformLogging(event: LogEvent) extends ClobOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.performLogging(event)
    }

    // Clob-specific operations.
    case object Free extends ClobOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.free
    }
    case object GetAsciiStream extends ClobOp[InputStream] {
      def visit[F[_]](v: Visitor[F]) = v.getAsciiStream
    }
    case object GetCharacterStream extends ClobOp[Reader] {
      def visit[F[_]](v: Visitor[F]) = v.getCharacterStream
    }
    final case class GetCharacterStream1(a: Long, b: Long) extends ClobOp[Reader] {
      def visit[F[_]](v: Visitor[F]) = v.getCharacterStream(a, b)
    }
    final case class GetSubString(a: Long, b: Int) extends ClobOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getSubString(a, b)
    }
    case object Length extends ClobOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.length
    }
    final case class Position(a: Clob, b: Long) extends ClobOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.position(a, b)
    }
    final case class Position1(a: String, b: Long) extends ClobOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.position(a, b)
    }
    final case class SetAsciiStream(a: Long) extends ClobOp[OutputStream] {
      def visit[F[_]](v: Visitor[F]) = v.setAsciiStream(a)
    }
    final case class SetCharacterStream(a: Long) extends ClobOp[Writer] {
      def visit[F[_]](v: Visitor[F]) = v.setCharacterStream(a)
    }
    final case class SetString(a: Long, b: String) extends ClobOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.setString(a, b)
    }
    final case class SetString1(a: Long, b: String, c: Int, d: Int) extends ClobOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.setString(a, b, c, d)
    }
    final case class Truncate(a: Long) extends ClobOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.truncate(a)
    }

  }
  import ClobOp.*

  // Smart constructors for operations common to all algebras.
  val unit: ClobIO[Unit] = FF.pure[ClobOp, Unit](())
  def pure[A](a: A): ClobIO[A] = FF.pure[ClobOp, A](a)
  def raw[A](f: Clob => A): ClobIO[A] = FF.liftF(Raw(f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[ClobOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def raiseError[A](err: Throwable): ClobIO[A] = FF.liftF[ClobOp, A](RaiseError(err))
  def handleErrorWith[A](fa: ClobIO[A])(f: Throwable => ClobIO[A]): ClobIO[A] = FF.liftF[ClobOp, A](HandleErrorWith(fa, f))
  val monotonic = FF.liftF[ClobOp, FiniteDuration](Monotonic)
  val realtime = FF.liftF[ClobOp, FiniteDuration](Realtime)
  def delay[A](thunk: => A) = FF.liftF[ClobOp, A](Suspend(Sync.Type.Delay, () => thunk))
  def suspend[A](hint: Sync.Type)(thunk: => A) = FF.liftF[ClobOp, A](Suspend(hint, () => thunk))
  def forceR[A, B](fa: ClobIO[A])(fb: ClobIO[B]) = FF.liftF[ClobOp, B](ForceR(fa, fb))
  def uncancelable[A](body: Poll[ClobIO] => ClobIO[A]) = FF.liftF[ClobOp, A](Uncancelable(body))
  def capturePoll[M[_]](mpoll: Poll[M]) = new Poll[ClobIO] {
    def apply[A](fa: ClobIO[A]) = FF.liftF[ClobOp, A](Poll1(mpoll, fa))
  }
  val canceled = FF.liftF[ClobOp, Unit](Canceled)
  def onCancel[A](fa: ClobIO[A], fin: ClobIO[Unit]) = FF.liftF[ClobOp, A](OnCancel(fa, fin))
  def fromFuture[A](fut: ClobIO[Future[A]]) = FF.liftF[ClobOp, A](FromFuture(fut))
  def fromFutureCancelable[A](fut: ClobIO[(Future[A], ClobIO[Unit])]) = FF.liftF[ClobOp, A](FromFutureCancelable(fut))
  def cancelable[A](fa: ClobIO[A], fin: ClobIO[Unit]) = FF.liftF[ClobOp, A](Cancelable(fa, fin))
  def performLogging(event: LogEvent) = FF.liftF[ClobOp, Unit](PerformLogging(event))

  // Smart constructors for Clob-specific operations.
  val free: ClobIO[Unit] = FF.liftF(Free)
  val getAsciiStream: ClobIO[InputStream] = FF.liftF(GetAsciiStream)
  val getCharacterStream: ClobIO[Reader] = FF.liftF(GetCharacterStream)
  def getCharacterStream(a: Long, b: Long): ClobIO[Reader] = FF.liftF(GetCharacterStream1(a, b))
  def getSubString(a: Long, b: Int): ClobIO[String] = FF.liftF(GetSubString(a, b))
  val length: ClobIO[Long] = FF.liftF(Length)
  def position(a: Clob, b: Long): ClobIO[Long] = FF.liftF(Position(a, b))
  def position(a: String, b: Long): ClobIO[Long] = FF.liftF(Position1(a, b))
  def setAsciiStream(a: Long): ClobIO[OutputStream] = FF.liftF(SetAsciiStream(a))
  def setCharacterStream(a: Long): ClobIO[Writer] = FF.liftF(SetCharacterStream(a))
  def setString(a: Long, b: String): ClobIO[Int] = FF.liftF(SetString(a, b))
  def setString(a: Long, b: String, c: Int, d: Int): ClobIO[Int] = FF.liftF(SetString1(a, b, c, d))
  def truncate(a: Long): ClobIO[Unit] = FF.liftF(Truncate(a))

  // Typeclass instances for ClobIO
  implicit val WeakAsyncClobIO: WeakAsync[ClobIO] =
    new WeakAsync[ClobIO] {
      val monad = FF.catsFreeMonadForFree[ClobOp]
      override val applicative: Applicative[ClobIO] = monad
      override val rootCancelScope: CancelScope = CancelScope.Cancelable
      override def pure[A](x: A): ClobIO[A] = monad.pure(x)
      override def flatMap[A, B](fa: ClobIO[A])(f: A => ClobIO[B]): ClobIO[B] = monad.flatMap(fa)(f)
      override def tailRecM[A, B](a: A)(f: A => ClobIO[Either[A, B]]): ClobIO[B] = monad.tailRecM(a)(f)
      override def raiseError[A](e: Throwable): ClobIO[A] = module.raiseError(e)
      override def handleErrorWith[A](fa: ClobIO[A])(f: Throwable => ClobIO[A]): ClobIO[A] = module.handleErrorWith(fa)(f)
      override def monotonic: ClobIO[FiniteDuration] = module.monotonic
      override def realTime: ClobIO[FiniteDuration] = module.realtime
      override def suspend[A](hint: Sync.Type)(thunk: => A): ClobIO[A] = module.suspend(hint)(thunk)
      override def forceR[A, B](fa: ClobIO[A])(fb: ClobIO[B]): ClobIO[B] = module.forceR(fa)(fb)
      override def uncancelable[A](body: Poll[ClobIO] => ClobIO[A]): ClobIO[A] = module.uncancelable(body)
      override def canceled: ClobIO[Unit] = module.canceled
      override def onCancel[A](fa: ClobIO[A], fin: ClobIO[Unit]): ClobIO[A] = module.onCancel(fa, fin)
      override def fromFuture[A](fut: ClobIO[Future[A]]): ClobIO[A] = module.fromFuture(fut)
      override def fromFutureCancelable[A](fut: ClobIO[(Future[A], ClobIO[Unit])]): ClobIO[A] = module.fromFutureCancelable(fut)
      override def cancelable[A](fa: ClobIO[A], fin: ClobIO[Unit]): ClobIO[A] = module.cancelable(fa, fin)
    }
    
  implicit def MonoidClobIO[A : Monoid]: Monoid[ClobIO[A]] = new Monoid[ClobIO[A]] {
    override def empty: ClobIO[A] = Applicative[ClobIO].pure(Monoid[A].empty)
    override def combine(x: ClobIO[A], y: ClobIO[A]): ClobIO[A] =
      Applicative[ClobIO].product(x, y).map { case (x, y) => Monoid[A].combine(x, y) }
  }
 
  implicit def SemigroupClobIO[A : Semigroup]: Semigroup[ClobIO[A]] = new Semigroup[ClobIO[A]] {
    override def combine(x: ClobIO[A], y: ClobIO[A]): ClobIO[A] =
      Applicative[ClobIO].product(x, y).map { case (x, y) => Semigroup[A].combine(x, y) }
  }
}

