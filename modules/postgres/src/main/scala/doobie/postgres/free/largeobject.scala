// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres.free

import cats.~>
import cats.effect.{ Async, ContextShift, ExitCase }
import cats.free.{ Free => FF } // alias because some algebras have an op called Free
import scala.concurrent.ExecutionContext
import com.github.ghik.silencer.silent
import io.chrisdavenport.log4cats.MessageLogger

import java.io.InputStream
import java.io.OutputStream
import org.postgresql.largeobject.LargeObject

@silent("deprecated")
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
        def embed[A](j: LargeObject, fa: FF[LargeObjectOp, A]) = Embedded.LargeObject(j, fa)
      }

    // Interface for a natural transformation LargeObjectOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (LargeObjectOp ~> F) {
      final def apply[A](fa: LargeObjectOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](message: => String, f: LargeObject => A): F[A]
      def embed[A](e: Embedded[A]): F[A]

      // Async
      def delay[A](a: () => A): F[A]
      def handleErrorWith[A](fa: LargeObjectIO[A], f: Throwable => LargeObjectIO[A]): F[A]
      def raiseError[A](e: Throwable): F[A]
      def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]
      def asyncF[A](k: (Either[Throwable, A] => Unit) => LargeObjectIO[Unit]): F[A]
      def bracketCase[A, B](acquire: LargeObjectIO[A])(use: A => LargeObjectIO[B])(release: (A, ExitCase[Throwable]) => LargeObjectIO[Unit]): F[B]
      def shift: F[Unit]
      def evalOn[A](ec: ExecutionContext)(fa: LargeObjectIO[A]): F[A]

      // Logger
      def error(message: => String): F[Unit]
      def warn(message: => String): F[Unit]
      def info(message: => String): F[Unit]
      def debug(message: => String): F[Unit]
      def trace(message: => String): F[Unit]

      // LargeObject
      def close: F[Unit]
      def copy: F[LargeObject]
      def getInputStream: F[InputStream]
      def getInputStream(a: Long): F[InputStream]
      def getLongOID: F[Long]
      def getOID: F[Int]
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

    }

    // Common operations for all algebras.
    final case class Raw[A](message: () => String, f: LargeObject => A) extends LargeObjectOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(message(), f)
    }
    final case class Embed[A](e: Embedded[A]) extends LargeObjectOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class Delay[A](a: () => A) extends LargeObjectOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.delay(a)
    }
    final case class HandleErrorWith[A](fa: LargeObjectIO[A], f: Throwable => LargeObjectIO[A]) extends LargeObjectOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa, f)
    }
    final case class RaiseError[A](e: Throwable) extends LargeObjectOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raiseError(e)
    }
    final case class Async1[A](k: (Either[Throwable, A] => Unit) => Unit) extends LargeObjectOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.async(k)
    }
    final case class AsyncF[A](k: (Either[Throwable, A] => Unit) => LargeObjectIO[Unit]) extends LargeObjectOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.asyncF(k)
    }
    final case class BracketCase[A, B](acquire: LargeObjectIO[A], use: A => LargeObjectIO[B], release: (A, ExitCase[Throwable]) => LargeObjectIO[Unit]) extends LargeObjectOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.bracketCase(acquire)(use)(release)
    }
    final case object Shift extends LargeObjectOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.shift
    }
    final case class EvalOn[A](ec: ExecutionContext, fa: LargeObjectIO[A]) extends LargeObjectOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.evalOn(ec)(fa)
    }
    final case class LogError(message: () => String) extends LargeObjectOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.error(message()): F[Unit]
    }
    final case class LogWarn(message: () => String) extends LargeObjectOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.warn(message()): F[Unit]
    }
    final case class LogInfo(message: () => String) extends LargeObjectOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.info(message()): F[Unit]
    }
    final case class LogDebug(message: () => String) extends LargeObjectOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.debug(message()): F[Unit]
    }
    final case class LogTrace(message: () => String) extends LargeObjectOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.trace(message()): F[Unit]
    }

    // LargeObject-specific operations.
    final case object Close extends LargeObjectOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.close
    }
    final case object Copy extends LargeObjectOp[LargeObject] {
      def visit[F[_]](v: Visitor[F]) = v.copy
    }
    final case object GetInputStream extends LargeObjectOp[InputStream] {
      def visit[F[_]](v: Visitor[F]) = v.getInputStream
    }
    final case class  GetInputStream1(a: Long) extends LargeObjectOp[InputStream] {
      def visit[F[_]](v: Visitor[F]) = v.getInputStream(a)
    }
    final case object GetLongOID extends LargeObjectOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.getLongOID
    }
    final case object GetOID extends LargeObjectOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getOID
    }
    final case object GetOutputStream extends LargeObjectOp[OutputStream] {
      def visit[F[_]](v: Visitor[F]) = v.getOutputStream
    }
    final case class  Read(a: Array[Byte], b: Int, c: Int) extends LargeObjectOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.read(a, b, c)
    }
    final case class  Read1(a: Int) extends LargeObjectOp[Array[Byte]] {
      def visit[F[_]](v: Visitor[F]) = v.read(a)
    }
    final case class  Seek(a: Int) extends LargeObjectOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.seek(a)
    }
    final case class  Seek1(a: Int, b: Int) extends LargeObjectOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.seek(a, b)
    }
    final case class  Seek64(a: Long, b: Int) extends LargeObjectOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.seek64(a, b)
    }
    final case object Size extends LargeObjectOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.size
    }
    final case object Size64 extends LargeObjectOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.size64
    }
    final case object Tell extends LargeObjectOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.tell
    }
    final case object Tell64 extends LargeObjectOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.tell64
    }
    final case class  Truncate(a: Int) extends LargeObjectOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.truncate(a)
    }
    final case class  Truncate64(a: Long) extends LargeObjectOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.truncate64(a)
    }
    final case class  Write(a: Array[Byte]) extends LargeObjectOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.write(a)
    }
    final case class  Write1(a: Array[Byte], b: Int, c: Int) extends LargeObjectOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.write(a, b, c)
    }

  }
  import LargeObjectOp._

  // Smart constructors for operations common to all algebras.
  val unit: LargeObjectIO[Unit] = FF.pure[LargeObjectOp, Unit](())
  def pure[A](a: A): LargeObjectIO[A] = FF.pure[LargeObjectOp, A](a)
  def raw[A](message: => String)(f: LargeObject => A): LargeObjectIO[A] = FF.liftF(Raw(() => message, f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[LargeObjectOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def delay[A](a: => A): LargeObjectIO[A] = FF.liftF(Delay(() => a))
  def handleErrorWith[A](fa: LargeObjectIO[A], f: Throwable => LargeObjectIO[A]): LargeObjectIO[A] = FF.liftF[LargeObjectOp, A](HandleErrorWith(fa, f))
  def raiseError[A](err: Throwable): LargeObjectIO[A] = FF.liftF[LargeObjectOp, A](RaiseError(err))
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): LargeObjectIO[A] = FF.liftF[LargeObjectOp, A](Async1(k))
  def asyncF[A](k: (Either[Throwable, A] => Unit) => LargeObjectIO[Unit]): LargeObjectIO[A] = FF.liftF[LargeObjectOp, A](AsyncF(k))
  def bracketCase[A, B](acquire: LargeObjectIO[A])(use: A => LargeObjectIO[B])(release: (A, ExitCase[Throwable]) => LargeObjectIO[Unit]): LargeObjectIO[B] = FF.liftF[LargeObjectOp, B](BracketCase(acquire, use, release))
  val shift: LargeObjectIO[Unit] = FF.liftF[LargeObjectOp, Unit](Shift)
  def evalOn[A](ec: ExecutionContext)(fa: LargeObjectIO[A]) = FF.liftF[LargeObjectOp, A](EvalOn(ec, fa))

  // Logger
  def error(message: => String): LargeObjectIO[Unit] = FF.liftF[LargeObjectOp, Unit](LogError(() => message))
  def warn(message: => String): LargeObjectIO[Unit] = FF.liftF[LargeObjectOp, Unit](LogWarn(() => message))
  def info(message: => String): LargeObjectIO[Unit] = FF.liftF[LargeObjectOp, Unit](LogInfo(() => message))
  def debug(message: => String): LargeObjectIO[Unit] = FF.liftF[LargeObjectOp, Unit](LogDebug(() => message))
  def trace(message: => String): LargeObjectIO[Unit] = FF.liftF[LargeObjectOp, Unit](LogTrace(() => message))

  // Smart constructors for LargeObject-specific operations.
  val close: LargeObjectIO[Unit] = FF.liftF(Close)
  val copy: LargeObjectIO[LargeObject] = FF.liftF(Copy)
  val getInputStream: LargeObjectIO[InputStream] = FF.liftF(GetInputStream)
  def getInputStream(a: Long): LargeObjectIO[InputStream] = FF.liftF(GetInputStream1(a))
  val getLongOID: LargeObjectIO[Long] = FF.liftF(GetLongOID)
  val getOID: LargeObjectIO[Int] = FF.liftF(GetOID)
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

  // LargeObjectIO is an Async
  implicit val AsyncLargeObjectIO: Async[LargeObjectIO] =
    new Async[LargeObjectIO] {
      val asyncM = FF.catsFreeMonadForFree[LargeObjectOp]
      def bracketCase[A, B](acquire: LargeObjectIO[A])(use: A => LargeObjectIO[B])(release: (A, ExitCase[Throwable]) => LargeObjectIO[Unit]): LargeObjectIO[B] = module.bracketCase(acquire)(use)(release)
      def pure[A](x: A): LargeObjectIO[A] = asyncM.pure(x)
      def handleErrorWith[A](fa: LargeObjectIO[A])(f: Throwable => LargeObjectIO[A]): LargeObjectIO[A] = module.handleErrorWith(fa, f)
      def raiseError[A](e: Throwable): LargeObjectIO[A] = module.raiseError(e)
      def async[A](k: (Either[Throwable,A] => Unit) => Unit): LargeObjectIO[A] = module.async(k)
      def asyncF[A](k: (Either[Throwable,A] => Unit) => LargeObjectIO[Unit]): LargeObjectIO[A] = module.asyncF(k)
      def flatMap[A, B](fa: LargeObjectIO[A])(f: A => LargeObjectIO[B]): LargeObjectIO[B] = asyncM.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => LargeObjectIO[Either[A, B]]): LargeObjectIO[B] = asyncM.tailRecM(a)(f)
      def suspend[A](thunk: => LargeObjectIO[A]): LargeObjectIO[A] = asyncM.flatten(module.delay(thunk))
    }

  // LargeObjectIO is a ContextShift
  implicit val ContextShiftLargeObjectIO: ContextShift[LargeObjectIO] =
    new ContextShift[LargeObjectIO] {
      def shift: LargeObjectIO[Unit] = module.shift
      def evalOn[A](ec: ExecutionContext)(fa: LargeObjectIO[A]) = module.evalOn(ec)(fa)
    }

  // LargeObjectIO is a MessageLogger
  implicit val MessageLoggerLargeObjectIO: MessageLogger[LargeObjectIO] =
    new MessageLogger[LargeObjectIO] {
      def error(message: => String): LargeObjectIO[Unit] =  module.error(message)
      def warn(message: => String): LargeObjectIO[Unit] = module.warn(message)
      def info(message: => String): LargeObjectIO[Unit] = module.info(message)
      def debug(message: => String): LargeObjectIO[Unit] =  module.debug(message)
      def trace(message: => String): LargeObjectIO[Unit] =  module.trace(message)
    }
}

