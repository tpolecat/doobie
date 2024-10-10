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
import java.sql.Blob

// This file is Auto-generated using FreeGen2.scala
object blob { module =>

  // Algebra of operations for Blob. Each accepts a visitor as an alternative to pattern-matching.
  sealed trait BlobOp[A] {
    def visit[F[_]](v: BlobOp.Visitor[F]): F[A]
  }

  // Free monad over BlobOp.
  type BlobIO[A] = FF[BlobOp, A]

  // Module of instances and constructors of BlobOp.
  object BlobOp {

    // Given a Blob we can embed a BlobIO program in any algebra that understands embedding.
    implicit val BlobOpEmbeddable: Embeddable[BlobOp, Blob] =
      new Embeddable[BlobOp, Blob] {
        def embed[A](j: Blob, fa: FF[BlobOp, A]): Embedded.Blob[A] = Embedded.Blob(j, fa)
      }

    // Interface for a natural transformation BlobOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (BlobOp ~> F) {
      final def apply[A](fa: BlobOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](f: Blob => A): F[A]
      def embed[A](e: Embedded[A]): F[A]
      def raiseError[A](e: Throwable): F[A]
      def handleErrorWith[A](fa: BlobIO[A])(f: Throwable => BlobIO[A]): F[A]
      def monotonic: F[FiniteDuration]
      def realTime: F[FiniteDuration]
      def delay[A](thunk: => A): F[A]
      def suspend[A](hint: Sync.Type)(thunk: => A): F[A]
      def forceR[A, B](fa: BlobIO[A])(fb: BlobIO[B]): F[B]
      def uncancelable[A](body: Poll[BlobIO] => BlobIO[A]): F[A]
      def poll[A](poll: Any, fa: BlobIO[A]): F[A]
      def canceled: F[Unit]
      def onCancel[A](fa: BlobIO[A], fin: BlobIO[Unit]): F[A]
      def fromFuture[A](fut: BlobIO[Future[A]]): F[A]
      def fromFutureCancelable[A](fut: BlobIO[(Future[A], BlobIO[Unit])]): F[A]
      def cancelable[A](fa: BlobIO[A], fin: BlobIO[Unit]): F[A]
      def performLogging(event: LogEvent): F[Unit]

      // Blob
      def free: F[Unit]
      def getBinaryStream: F[InputStream]
      def getBinaryStream(a: Long, b: Long): F[InputStream]
      def getBytes(a: Long, b: Int): F[Array[Byte]]
      def length: F[Long]
      def position(a: Array[Byte], b: Long): F[Long]
      def position(a: Blob, b: Long): F[Long]
      def setBinaryStream(a: Long): F[OutputStream]
      def setBytes(a: Long, b: Array[Byte]): F[Int]
      def setBytes(a: Long, b: Array[Byte], c: Int, d: Int): F[Int]
      def truncate(a: Long): F[Unit]

    }

    // Common operations for all algebras.
    final case class Raw[A](f: Blob => A) extends BlobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(f)
    }
    final case class Embed[A](e: Embedded[A]) extends BlobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class RaiseError[A](e: Throwable) extends BlobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raiseError(e)
    }
    final case class HandleErrorWith[A](fa: BlobIO[A], f: Throwable => BlobIO[A]) extends BlobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa)(f)
    }
    case object Monotonic extends BlobOp[FiniteDuration] {
      def visit[F[_]](v: Visitor[F]) = v.monotonic
    }
    case object Realtime extends BlobOp[FiniteDuration] {
      def visit[F[_]](v: Visitor[F]) = v.realTime
    }
    case class Suspend[A](hint: Sync.Type, thunk: () => A) extends BlobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.suspend(hint)(thunk())
    }
    case class ForceR[A, B](fa: BlobIO[A], fb: BlobIO[B]) extends BlobOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.forceR(fa)(fb)
    }
    case class Uncancelable[A](body: Poll[BlobIO] => BlobIO[A]) extends BlobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.uncancelable(body)
    }
    case class Poll1[A](poll: Any, fa: BlobIO[A]) extends BlobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.poll(poll, fa)
    }
    case object Canceled extends BlobOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.canceled
    }
    case class OnCancel[A](fa: BlobIO[A], fin: BlobIO[Unit]) extends BlobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.onCancel(fa, fin)
    }
    case class FromFuture[A](fut: BlobIO[Future[A]]) extends BlobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.fromFuture(fut)
    }
    case class FromFutureCancelable[A](fut: BlobIO[(Future[A], BlobIO[Unit])]) extends BlobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.fromFutureCancelable(fut)
    }
    case class Cancelable[A](fa: BlobIO[A], fin: BlobIO[Unit]) extends BlobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.cancelable(fa, fin)
    }
    case class PerformLogging(event: LogEvent) extends BlobOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.performLogging(event)
    }

    // Blob-specific operations.
    case object Free extends BlobOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.free
    }
    case object GetBinaryStream extends BlobOp[InputStream] {
      def visit[F[_]](v: Visitor[F]) = v.getBinaryStream
    }
    final case class GetBinaryStream1(a: Long, b: Long) extends BlobOp[InputStream] {
      def visit[F[_]](v: Visitor[F]) = v.getBinaryStream(a, b)
    }
    final case class GetBytes(a: Long, b: Int) extends BlobOp[Array[Byte]] {
      def visit[F[_]](v: Visitor[F]) = v.getBytes(a, b)
    }
    case object Length extends BlobOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.length
    }
    final case class Position(a: Array[Byte], b: Long) extends BlobOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.position(a, b)
    }
    final case class Position1(a: Blob, b: Long) extends BlobOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.position(a, b)
    }
    final case class SetBinaryStream(a: Long) extends BlobOp[OutputStream] {
      def visit[F[_]](v: Visitor[F]) = v.setBinaryStream(a)
    }
    final case class SetBytes(a: Long, b: Array[Byte]) extends BlobOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.setBytes(a, b)
    }
    final case class SetBytes1(a: Long, b: Array[Byte], c: Int, d: Int) extends BlobOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.setBytes(a, b, c, d)
    }
    final case class Truncate(a: Long) extends BlobOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.truncate(a)
    }

  }
  import BlobOp.*

  // Smart constructors for operations common to all algebras.
  val unit: BlobIO[Unit] = FF.pure[BlobOp, Unit](())
  def pure[A](a: A): BlobIO[A] = FF.pure[BlobOp, A](a)
  def raw[A](f: Blob => A): BlobIO[A] = FF.liftF(Raw(f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[BlobOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def raiseError[A](err: Throwable): BlobIO[A] = FF.liftF[BlobOp, A](RaiseError(err))
  def handleErrorWith[A](fa: BlobIO[A])(f: Throwable => BlobIO[A]): BlobIO[A] = FF.liftF[BlobOp, A](HandleErrorWith(fa, f))
  val monotonic = FF.liftF[BlobOp, FiniteDuration](Monotonic)
  val realtime = FF.liftF[BlobOp, FiniteDuration](Realtime)
  def delay[A](thunk: => A) = FF.liftF[BlobOp, A](Suspend(Sync.Type.Delay, () => thunk))
  def suspend[A](hint: Sync.Type)(thunk: => A) = FF.liftF[BlobOp, A](Suspend(hint, () => thunk))
  def forceR[A, B](fa: BlobIO[A])(fb: BlobIO[B]) = FF.liftF[BlobOp, B](ForceR(fa, fb))
  def uncancelable[A](body: Poll[BlobIO] => BlobIO[A]) = FF.liftF[BlobOp, A](Uncancelable(body))
  def capturePoll[M[_]](mpoll: Poll[M]) = new Poll[BlobIO] {
    def apply[A](fa: BlobIO[A]) = FF.liftF[BlobOp, A](Poll1(mpoll, fa))
  }
  val canceled = FF.liftF[BlobOp, Unit](Canceled)
  def onCancel[A](fa: BlobIO[A], fin: BlobIO[Unit]) = FF.liftF[BlobOp, A](OnCancel(fa, fin))
  def fromFuture[A](fut: BlobIO[Future[A]]) = FF.liftF[BlobOp, A](FromFuture(fut))
  def fromFutureCancelable[A](fut: BlobIO[(Future[A], BlobIO[Unit])]) = FF.liftF[BlobOp, A](FromFutureCancelable(fut))
  def cancelable[A](fa: BlobIO[A], fin: BlobIO[Unit]) = FF.liftF[BlobOp, A](Cancelable(fa, fin))
  def performLogging(event: LogEvent) = FF.liftF[BlobOp, Unit](PerformLogging(event))

  // Smart constructors for Blob-specific operations.
  val free: BlobIO[Unit] = FF.liftF(Free)
  val getBinaryStream: BlobIO[InputStream] = FF.liftF(GetBinaryStream)
  def getBinaryStream(a: Long, b: Long): BlobIO[InputStream] = FF.liftF(GetBinaryStream1(a, b))
  def getBytes(a: Long, b: Int): BlobIO[Array[Byte]] = FF.liftF(GetBytes(a, b))
  val length: BlobIO[Long] = FF.liftF(Length)
  def position(a: Array[Byte], b: Long): BlobIO[Long] = FF.liftF(Position(a, b))
  def position(a: Blob, b: Long): BlobIO[Long] = FF.liftF(Position1(a, b))
  def setBinaryStream(a: Long): BlobIO[OutputStream] = FF.liftF(SetBinaryStream(a))
  def setBytes(a: Long, b: Array[Byte]): BlobIO[Int] = FF.liftF(SetBytes(a, b))
  def setBytes(a: Long, b: Array[Byte], c: Int, d: Int): BlobIO[Int] = FF.liftF(SetBytes1(a, b, c, d))
  def truncate(a: Long): BlobIO[Unit] = FF.liftF(Truncate(a))

  // Typeclass instances for BlobIO
  implicit val WeakAsyncBlobIO: WeakAsync[BlobIO] =
    new WeakAsync[BlobIO] {
      val monad = FF.catsFreeMonadForFree[BlobOp]
      override val applicative: Applicative[BlobIO] = monad
      override val rootCancelScope: CancelScope = CancelScope.Cancelable
      override def pure[A](x: A): BlobIO[A] = monad.pure(x)
      override def flatMap[A, B](fa: BlobIO[A])(f: A => BlobIO[B]): BlobIO[B] = monad.flatMap(fa)(f)
      override def tailRecM[A, B](a: A)(f: A => BlobIO[Either[A, B]]): BlobIO[B] = monad.tailRecM(a)(f)
      override def raiseError[A](e: Throwable): BlobIO[A] = module.raiseError(e)
      override def handleErrorWith[A](fa: BlobIO[A])(f: Throwable => BlobIO[A]): BlobIO[A] = module.handleErrorWith(fa)(f)
      override def monotonic: BlobIO[FiniteDuration] = module.monotonic
      override def realTime: BlobIO[FiniteDuration] = module.realtime
      override def suspend[A](hint: Sync.Type)(thunk: => A): BlobIO[A] = module.suspend(hint)(thunk)
      override def forceR[A, B](fa: BlobIO[A])(fb: BlobIO[B]): BlobIO[B] = module.forceR(fa)(fb)
      override def uncancelable[A](body: Poll[BlobIO] => BlobIO[A]): BlobIO[A] = module.uncancelable(body)
      override def canceled: BlobIO[Unit] = module.canceled
      override def onCancel[A](fa: BlobIO[A], fin: BlobIO[Unit]): BlobIO[A] = module.onCancel(fa, fin)
      override def fromFuture[A](fut: BlobIO[Future[A]]): BlobIO[A] = module.fromFuture(fut)
      override def fromFutureCancelable[A](fut: BlobIO[(Future[A], BlobIO[Unit])]): BlobIO[A] = module.fromFutureCancelable(fut)
      override def cancelable[A](fa: BlobIO[A], fin: BlobIO[Unit]): BlobIO[A] = module.cancelable(fa, fin)
    }
    
  implicit def MonoidBlobIO[A : Monoid]: Monoid[BlobIO[A]] = new Monoid[BlobIO[A]] {
    override def empty: BlobIO[A] = Applicative[BlobIO].pure(Monoid[A].empty)
    override def combine(x: BlobIO[A], y: BlobIO[A]): BlobIO[A] =
      Applicative[BlobIO].product(x, y).map { case (x, y) => Monoid[A].combine(x, y) }
  }
 
  implicit def SemigroupBlobIO[A : Semigroup]: Semigroup[BlobIO[A]] = new Semigroup[BlobIO[A]] {
    override def combine(x: BlobIO[A], y: BlobIO[A]): BlobIO[A] =
      Applicative[BlobIO].product(x, y).map { case (x, y) => Semigroup[A].combine(x, y) }
  }
}

