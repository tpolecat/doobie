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
import org.postgresql.largeobject.LargeObject
import org.postgresql.util.ByteStreamWriter

// This file is Auto-generated using FreeGen2.scala
object largeobject { module =>

  // Algebra of operations for LargeObject. Each accepts a visitor as an alternative to pattern-matching.
  sealed trait LargeObjectOp[A] {
    def visit[F[_]](v: LargeObjectOp.Visitor[F]): F[A]
  }

  // Free monad over LargeObjectOp.
  type LargeObjectIO[A] = FF[LargeObjectOp, A]

  // Module of instances and constructors of LargeObjectOp.
  object LargeObjectOp {

    // Given a LargeObject we can embed a LargeObjectIO program in any algebra that understands embedding.
    implicit val LargeObjectOpEmbeddable: Embeddable[LargeObjectOp, LargeObject] =
      new Embeddable[LargeObjectOp, LargeObject] {
        def embed[A](j: LargeObject, fa: FF[LargeObjectOp, A]): Embedded.LargeObject[A] = Embedded.LargeObject(j, fa)
      }

    // Interface for a natural transformation LargeObjectOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (LargeObjectOp ~> F) {
      final def apply[A](fa: LargeObjectOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](f: LargeObject => A): F[A]
      def embed[A](e: Embedded[A]): F[A]
      def raiseError[A](e: Throwable): F[A]
      def handleErrorWith[A](fa: LargeObjectIO[A])(f: Throwable => LargeObjectIO[A]): F[A]
      def monotonic: F[FiniteDuration]
      def realTime: F[FiniteDuration]
      def delay[A](thunk: => A): F[A]
      def suspend[A](hint: Sync.Type)(thunk: => A): F[A]
      def forceR[A, B](fa: LargeObjectIO[A])(fb: LargeObjectIO[B]): F[B]
      def uncancelable[A](body: Poll[LargeObjectIO] => LargeObjectIO[A]): F[A]
      def poll[A](poll: Any, fa: LargeObjectIO[A]): F[A]
      def canceled: F[Unit]
      def onCancel[A](fa: LargeObjectIO[A], fin: LargeObjectIO[Unit]): F[A]
      def fromFuture[A](fut: LargeObjectIO[Future[A]]): F[A]
      def fromFutureCancelable[A](fut: LargeObjectIO[(Future[A], LargeObjectIO[Unit])]): F[A]
      def cancelable[A](fa: LargeObjectIO[A], fin: LargeObjectIO[Unit]): F[A]
      def performLogging(event: LogEvent): F[Unit]

      // LargeObject
      def close: F[Unit]
      def copy: F[LargeObject]
      def getInputStream: F[InputStream]
      def getInputStream(a: Int, b: Long): F[InputStream]
      def getInputStream(a: Long): F[InputStream]
      def getLongOID: F[Long]
      def getOutputStream: F[OutputStream]
      def read(a: Array[Byte], b: Int, c: Int): F[Int]
      def read(a: Int): F[Array[Byte]]
      def seek(a: Int): F[Unit]
      def seek(a: Int, b: Int): F[Unit]
      def seek64(a: Long, b: Int): F[Unit]
      def size: F[Int]
      def size64: F[Long]
      def tell: F[Int]
      def tell64: F[Long]
      def truncate(a: Int): F[Unit]
      def truncate64(a: Long): F[Unit]
      def write(a: Array[Byte]): F[Unit]
      def write(a: Array[Byte], b: Int, c: Int): F[Unit]
      def write(a: ByteStreamWriter): F[Unit]

    }

    // Common operations for all algebras.
    final case class Raw[A](f: LargeObject => A) extends LargeObjectOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(f)
    }
    final case class Embed[A](e: Embedded[A]) extends LargeObjectOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class RaiseError[A](e: Throwable) extends LargeObjectOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raiseError(e)
    }
    final case class HandleErrorWith[A](fa: LargeObjectIO[A], f: Throwable => LargeObjectIO[A]) extends LargeObjectOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa)(f)
    }
    case object Monotonic extends LargeObjectOp[FiniteDuration] {
      def visit[F[_]](v: Visitor[F]) = v.monotonic
    }
    case object Realtime extends LargeObjectOp[FiniteDuration] {
      def visit[F[_]](v: Visitor[F]) = v.realTime
    }
    case class Suspend[A](hint: Sync.Type, thunk: () => A) extends LargeObjectOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.suspend(hint)(thunk())
    }
    case class ForceR[A, B](fa: LargeObjectIO[A], fb: LargeObjectIO[B]) extends LargeObjectOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.forceR(fa)(fb)
    }
    case class Uncancelable[A](body: Poll[LargeObjectIO] => LargeObjectIO[A]) extends LargeObjectOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.uncancelable(body)
    }
    case class Poll1[A](poll: Any, fa: LargeObjectIO[A]) extends LargeObjectOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.poll(poll, fa)
    }
    case object Canceled extends LargeObjectOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.canceled
    }
    case class OnCancel[A](fa: LargeObjectIO[A], fin: LargeObjectIO[Unit]) extends LargeObjectOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.onCancel(fa, fin)
    }
    case class FromFuture[A](fut: LargeObjectIO[Future[A]]) extends LargeObjectOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.fromFuture(fut)
    }
    case class FromFutureCancelable[A](fut: LargeObjectIO[(Future[A], LargeObjectIO[Unit])]) extends LargeObjectOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.fromFutureCancelable(fut)
    }
    case class Cancelable[A](fa: LargeObjectIO[A], fin: LargeObjectIO[Unit]) extends LargeObjectOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.cancelable(fa, fin)
    }
    case class PerformLogging(event: LogEvent) extends LargeObjectOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.performLogging(event)
    }

    // LargeObject-specific operations.
    case object Close extends LargeObjectOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.close
    }
    case object Copy extends LargeObjectOp[LargeObject] {
      def visit[F[_]](v: Visitor[F]) = v.copy
    }
    case object GetInputStream extends LargeObjectOp[InputStream] {
      def visit[F[_]](v: Visitor[F]) = v.getInputStream
    }
    final case class GetInputStream1(a: Int, b: Long) extends LargeObjectOp[InputStream] {
      def visit[F[_]](v: Visitor[F]) = v.getInputStream(a, b)
    }
    final case class GetInputStream2(a: Long) extends LargeObjectOp[InputStream] {
      def visit[F[_]](v: Visitor[F]) = v.getInputStream(a)
    }
    case object GetLongOID extends LargeObjectOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.getLongOID
    }
    case object GetOutputStream extends LargeObjectOp[OutputStream] {
      def visit[F[_]](v: Visitor[F]) = v.getOutputStream
    }
    final case class Read(a: Array[Byte], b: Int, c: Int) extends LargeObjectOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.read(a, b, c)
    }
    final case class Read1(a: Int) extends LargeObjectOp[Array[Byte]] {
      def visit[F[_]](v: Visitor[F]) = v.read(a)
    }
    final case class Seek(a: Int) extends LargeObjectOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.seek(a)
    }
    final case class Seek1(a: Int, b: Int) extends LargeObjectOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.seek(a, b)
    }
    final case class Seek64(a: Long, b: Int) extends LargeObjectOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.seek64(a, b)
    }
    case object Size extends LargeObjectOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.size
    }
    case object Size64 extends LargeObjectOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.size64
    }
    case object Tell extends LargeObjectOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.tell
    }
    case object Tell64 extends LargeObjectOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.tell64
    }
    final case class Truncate(a: Int) extends LargeObjectOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.truncate(a)
    }
    final case class Truncate64(a: Long) extends LargeObjectOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.truncate64(a)
    }
    final case class Write(a: Array[Byte]) extends LargeObjectOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.write(a)
    }
    final case class Write1(a: Array[Byte], b: Int, c: Int) extends LargeObjectOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.write(a, b, c)
    }
    final case class Write2(a: ByteStreamWriter) extends LargeObjectOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.write(a)
    }

  }
  import LargeObjectOp.*

  // Smart constructors for operations common to all algebras.
  val unit: LargeObjectIO[Unit] = FF.pure[LargeObjectOp, Unit](())
  def pure[A](a: A): LargeObjectIO[A] = FF.pure[LargeObjectOp, A](a)
  def raw[A](f: LargeObject => A): LargeObjectIO[A] = FF.liftF(Raw(f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[LargeObjectOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def raiseError[A](err: Throwable): LargeObjectIO[A] = FF.liftF[LargeObjectOp, A](RaiseError(err))
  def handleErrorWith[A](fa: LargeObjectIO[A])(f: Throwable => LargeObjectIO[A]): LargeObjectIO[A] = FF.liftF[LargeObjectOp, A](HandleErrorWith(fa, f))
  val monotonic = FF.liftF[LargeObjectOp, FiniteDuration](Monotonic)
  val realtime = FF.liftF[LargeObjectOp, FiniteDuration](Realtime)
  def delay[A](thunk: => A) = FF.liftF[LargeObjectOp, A](Suspend(Sync.Type.Delay, () => thunk))
  def suspend[A](hint: Sync.Type)(thunk: => A) = FF.liftF[LargeObjectOp, A](Suspend(hint, () => thunk))
  def forceR[A, B](fa: LargeObjectIO[A])(fb: LargeObjectIO[B]) = FF.liftF[LargeObjectOp, B](ForceR(fa, fb))
  def uncancelable[A](body: Poll[LargeObjectIO] => LargeObjectIO[A]) = FF.liftF[LargeObjectOp, A](Uncancelable(body))
  def capturePoll[M[_]](mpoll: Poll[M]) = new Poll[LargeObjectIO] {
    def apply[A](fa: LargeObjectIO[A]) = FF.liftF[LargeObjectOp, A](Poll1(mpoll, fa))
  }
  val canceled = FF.liftF[LargeObjectOp, Unit](Canceled)
  def onCancel[A](fa: LargeObjectIO[A], fin: LargeObjectIO[Unit]) = FF.liftF[LargeObjectOp, A](OnCancel(fa, fin))
  def fromFuture[A](fut: LargeObjectIO[Future[A]]) = FF.liftF[LargeObjectOp, A](FromFuture(fut))
  def fromFutureCancelable[A](fut: LargeObjectIO[(Future[A], LargeObjectIO[Unit])]) = FF.liftF[LargeObjectOp, A](FromFutureCancelable(fut))
  def cancelable[A](fa: LargeObjectIO[A], fin: LargeObjectIO[Unit]) = FF.liftF[LargeObjectOp, A](Cancelable(fa, fin))
  def performLogging(event: LogEvent) = FF.liftF[LargeObjectOp, Unit](PerformLogging(event))

  // Smart constructors for LargeObject-specific operations.
  val close: LargeObjectIO[Unit] = FF.liftF(Close)
  val copy: LargeObjectIO[LargeObject] = FF.liftF(Copy)
  val getInputStream: LargeObjectIO[InputStream] = FF.liftF(GetInputStream)
  def getInputStream(a: Int, b: Long): LargeObjectIO[InputStream] = FF.liftF(GetInputStream1(a, b))
  def getInputStream(a: Long): LargeObjectIO[InputStream] = FF.liftF(GetInputStream2(a))
  val getLongOID: LargeObjectIO[Long] = FF.liftF(GetLongOID)
  val getOutputStream: LargeObjectIO[OutputStream] = FF.liftF(GetOutputStream)
  def read(a: Array[Byte], b: Int, c: Int): LargeObjectIO[Int] = FF.liftF(Read(a, b, c))
  def read(a: Int): LargeObjectIO[Array[Byte]] = FF.liftF(Read1(a))
  def seek(a: Int): LargeObjectIO[Unit] = FF.liftF(Seek(a))
  def seek(a: Int, b: Int): LargeObjectIO[Unit] = FF.liftF(Seek1(a, b))
  def seek64(a: Long, b: Int): LargeObjectIO[Unit] = FF.liftF(Seek64(a, b))
  val size: LargeObjectIO[Int] = FF.liftF(Size)
  val size64: LargeObjectIO[Long] = FF.liftF(Size64)
  val tell: LargeObjectIO[Int] = FF.liftF(Tell)
  val tell64: LargeObjectIO[Long] = FF.liftF(Tell64)
  def truncate(a: Int): LargeObjectIO[Unit] = FF.liftF(Truncate(a))
  def truncate64(a: Long): LargeObjectIO[Unit] = FF.liftF(Truncate64(a))
  def write(a: Array[Byte]): LargeObjectIO[Unit] = FF.liftF(Write(a))
  def write(a: Array[Byte], b: Int, c: Int): LargeObjectIO[Unit] = FF.liftF(Write1(a, b, c))
  def write(a: ByteStreamWriter): LargeObjectIO[Unit] = FF.liftF(Write2(a))

  // Typeclass instances for LargeObjectIO
  implicit val WeakAsyncLargeObjectIO: WeakAsync[LargeObjectIO] =
    new WeakAsync[LargeObjectIO] {
      val monad = FF.catsFreeMonadForFree[LargeObjectOp]
      override val applicative: Applicative[LargeObjectIO] = monad
      override val rootCancelScope: CancelScope = CancelScope.Cancelable
      override def pure[A](x: A): LargeObjectIO[A] = monad.pure(x)
      override def flatMap[A, B](fa: LargeObjectIO[A])(f: A => LargeObjectIO[B]): LargeObjectIO[B] = monad.flatMap(fa)(f)
      override def tailRecM[A, B](a: A)(f: A => LargeObjectIO[Either[A, B]]): LargeObjectIO[B] = monad.tailRecM(a)(f)
      override def raiseError[A](e: Throwable): LargeObjectIO[A] = module.raiseError(e)
      override def handleErrorWith[A](fa: LargeObjectIO[A])(f: Throwable => LargeObjectIO[A]): LargeObjectIO[A] = module.handleErrorWith(fa)(f)
      override def monotonic: LargeObjectIO[FiniteDuration] = module.monotonic
      override def realTime: LargeObjectIO[FiniteDuration] = module.realtime
      override def suspend[A](hint: Sync.Type)(thunk: => A): LargeObjectIO[A] = module.suspend(hint)(thunk)
      override def forceR[A, B](fa: LargeObjectIO[A])(fb: LargeObjectIO[B]): LargeObjectIO[B] = module.forceR(fa)(fb)
      override def uncancelable[A](body: Poll[LargeObjectIO] => LargeObjectIO[A]): LargeObjectIO[A] = module.uncancelable(body)
      override def canceled: LargeObjectIO[Unit] = module.canceled
      override def onCancel[A](fa: LargeObjectIO[A], fin: LargeObjectIO[Unit]): LargeObjectIO[A] = module.onCancel(fa, fin)
      override def fromFuture[A](fut: LargeObjectIO[Future[A]]): LargeObjectIO[A] = module.fromFuture(fut)
      override def fromFutureCancelable[A](fut: LargeObjectIO[(Future[A], LargeObjectIO[Unit])]): LargeObjectIO[A] = module.fromFutureCancelable(fut)
      override def cancelable[A](fa: LargeObjectIO[A], fin: LargeObjectIO[Unit]): LargeObjectIO[A] = module.cancelable(fa, fin)
    }
    
  implicit def MonoidLargeObjectIO[A : Monoid]: Monoid[LargeObjectIO[A]] = new Monoid[LargeObjectIO[A]] {
    override def empty: LargeObjectIO[A] = Applicative[LargeObjectIO].pure(Monoid[A].empty)
    override def combine(x: LargeObjectIO[A], y: LargeObjectIO[A]): LargeObjectIO[A] =
      Applicative[LargeObjectIO].product(x, y).map { case (x, y) => Monoid[A].combine(x, y) }
  }
 
  implicit def SemigroupLargeObjectIO[A : Semigroup]: Semigroup[LargeObjectIO[A]] = new Semigroup[LargeObjectIO[A]] {
    override def combine(x: LargeObjectIO[A], y: LargeObjectIO[A]): LargeObjectIO[A] =
      Applicative[LargeObjectIO].product(x, y).map { case (x, y) => Semigroup[A].combine(x, y) }
  }
}

