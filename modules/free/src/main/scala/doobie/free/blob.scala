// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.free

import cats.~>
import cats.effect.{ Async, ContextShift, ExitCase }
import cats.free.{ Free => FF } // alias because some algebras have an op called Free
import scala.concurrent.ExecutionContext
import com.github.ghik.silencer.silent
import io.chrisdavenport.log4cats.MessageLogger

import java.io.InputStream
import java.io.OutputStream
import java.sql.Blob

@silent("deprecated")
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
        def embed[A](j: Blob, fa: FF[BlobOp, A]) = Embedded.Blob(j, fa)
      }

    // Interface for a natural transformation BlobOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (BlobOp ~> F) {
      final def apply[A](fa: BlobOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](message: => String, f: Blob => A): F[A]
      def embed[A](e: Embedded[A]): F[A]

      // Async
      def delay[A](a: () => A): F[A]
      def handleErrorWith[A](fa: BlobIO[A], f: Throwable => BlobIO[A]): F[A]
      def raiseError[A](e: Throwable): F[A]
      def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]
      def asyncF[A](k: (Either[Throwable, A] => Unit) => BlobIO[Unit]): F[A]
      def bracketCase[A, B](acquire: BlobIO[A])(use: A => BlobIO[B])(release: (A, ExitCase[Throwable]) => BlobIO[Unit]): F[B]
      def shift: F[Unit]
      def evalOn[A](ec: ExecutionContext)(fa: BlobIO[A]): F[A]

      // Logger
      def error(message: => String): F[Unit]
      def warn(message: => String): F[Unit]
      def info(message: => String): F[Unit]
      def debug(message: => String): F[Unit]
      def trace(message: => String): F[Unit]

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
    final case class Raw[A](message: () => String, f: Blob => A) extends BlobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(message(), f)
    }
    final case class Embed[A](e: Embedded[A]) extends BlobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class Delay[A](a: () => A) extends BlobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.delay(a)
    }
    final case class HandleErrorWith[A](fa: BlobIO[A], f: Throwable => BlobIO[A]) extends BlobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa, f)
    }
    final case class RaiseError[A](e: Throwable) extends BlobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raiseError(e)
    }
    final case class Async1[A](k: (Either[Throwable, A] => Unit) => Unit) extends BlobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.async(k)
    }
    final case class AsyncF[A](k: (Either[Throwable, A] => Unit) => BlobIO[Unit]) extends BlobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.asyncF(k)
    }
    final case class BracketCase[A, B](acquire: BlobIO[A], use: A => BlobIO[B], release: (A, ExitCase[Throwable]) => BlobIO[Unit]) extends BlobOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.bracketCase(acquire)(use)(release)
    }
    final case object Shift extends BlobOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.shift
    }
    final case class EvalOn[A](ec: ExecutionContext, fa: BlobIO[A]) extends BlobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.evalOn(ec)(fa)
    }
    final case class LogError(message: () => String) extends BlobOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.error(message()): F[Unit]
    }
    final case class LogWarn(message: () => String) extends BlobOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.warn(message()): F[Unit]
    }
    final case class LogInfo(message: () => String) extends BlobOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.info(message()): F[Unit]
    }
    final case class LogDebug(message: () => String) extends BlobOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.debug(message()): F[Unit]
    }
    final case class LogTrace(message: () => String) extends BlobOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.trace(message()): F[Unit]
    }

    // Blob-specific operations.
    final case object Free extends BlobOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.free
    }
    final case object GetBinaryStream extends BlobOp[InputStream] {
      def visit[F[_]](v: Visitor[F]) = v.getBinaryStream
    }
    final case class  GetBinaryStream1(a: Long, b: Long) extends BlobOp[InputStream] {
      def visit[F[_]](v: Visitor[F]) = v.getBinaryStream(a, b)
    }
    final case class  GetBytes(a: Long, b: Int) extends BlobOp[Array[Byte]] {
      def visit[F[_]](v: Visitor[F]) = v.getBytes(a, b)
    }
    final case object Length extends BlobOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.length
    }
    final case class  Position(a: Array[Byte], b: Long) extends BlobOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.position(a, b)
    }
    final case class  Position1(a: Blob, b: Long) extends BlobOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.position(a, b)
    }
    final case class  SetBinaryStream(a: Long) extends BlobOp[OutputStream] {
      def visit[F[_]](v: Visitor[F]) = v.setBinaryStream(a)
    }
    final case class  SetBytes(a: Long, b: Array[Byte]) extends BlobOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.setBytes(a, b)
    }
    final case class  SetBytes1(a: Long, b: Array[Byte], c: Int, d: Int) extends BlobOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.setBytes(a, b, c, d)
    }
    final case class  Truncate(a: Long) extends BlobOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.truncate(a)
    }

  }
  import BlobOp._

  // Smart constructors for operations common to all algebras.
  val unit: BlobIO[Unit] = FF.pure[BlobOp, Unit](())
  def pure[A](a: A): BlobIO[A] = FF.pure[BlobOp, A](a)
  def raw[A](message: => String)(f: Blob => A): BlobIO[A] = FF.liftF(Raw(() => message, f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[BlobOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def delay[A](a: => A): BlobIO[A] = FF.liftF(Delay(() => a))
  def handleErrorWith[A](fa: BlobIO[A], f: Throwable => BlobIO[A]): BlobIO[A] = FF.liftF[BlobOp, A](HandleErrorWith(fa, f))
  def raiseError[A](err: Throwable): BlobIO[A] = FF.liftF[BlobOp, A](RaiseError(err))
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): BlobIO[A] = FF.liftF[BlobOp, A](Async1(k))
  def asyncF[A](k: (Either[Throwable, A] => Unit) => BlobIO[Unit]): BlobIO[A] = FF.liftF[BlobOp, A](AsyncF(k))
  def bracketCase[A, B](acquire: BlobIO[A])(use: A => BlobIO[B])(release: (A, ExitCase[Throwable]) => BlobIO[Unit]): BlobIO[B] = FF.liftF[BlobOp, B](BracketCase(acquire, use, release))
  val shift: BlobIO[Unit] = FF.liftF[BlobOp, Unit](Shift)
  def evalOn[A](ec: ExecutionContext)(fa: BlobIO[A]) = FF.liftF[BlobOp, A](EvalOn(ec, fa))

  // Logger
  def error(message: => String): BlobIO[Unit] = FF.liftF[BlobOp, Unit](LogError(() => message))
  def warn(message: => String): BlobIO[Unit] = FF.liftF[BlobOp, Unit](LogWarn(() => message))
  def info(message: => String): BlobIO[Unit] = FF.liftF[BlobOp, Unit](LogInfo(() => message))
  def debug(message: => String): BlobIO[Unit] = FF.liftF[BlobOp, Unit](LogDebug(() => message))
  def trace(message: => String): BlobIO[Unit] = FF.liftF[BlobOp, Unit](LogTrace(() => message))

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

  // BlobIO is an Async
  implicit val AsyncBlobIO: Async[BlobIO] =
    new Async[BlobIO] {
      val asyncM = FF.catsFreeMonadForFree[BlobOp]
      def bracketCase[A, B](acquire: BlobIO[A])(use: A => BlobIO[B])(release: (A, ExitCase[Throwable]) => BlobIO[Unit]): BlobIO[B] = module.bracketCase(acquire)(use)(release)
      def pure[A](x: A): BlobIO[A] = asyncM.pure(x)
      def handleErrorWith[A](fa: BlobIO[A])(f: Throwable => BlobIO[A]): BlobIO[A] = module.handleErrorWith(fa, f)
      def raiseError[A](e: Throwable): BlobIO[A] = module.raiseError(e)
      def async[A](k: (Either[Throwable,A] => Unit) => Unit): BlobIO[A] = module.async(k)
      def asyncF[A](k: (Either[Throwable,A] => Unit) => BlobIO[Unit]): BlobIO[A] = module.asyncF(k)
      def flatMap[A, B](fa: BlobIO[A])(f: A => BlobIO[B]): BlobIO[B] = asyncM.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => BlobIO[Either[A, B]]): BlobIO[B] = asyncM.tailRecM(a)(f)
      def suspend[A](thunk: => BlobIO[A]): BlobIO[A] = asyncM.flatten(module.delay(thunk))
    }

  // BlobIO is a ContextShift
  implicit val ContextShiftBlobIO: ContextShift[BlobIO] =
    new ContextShift[BlobIO] {
      def shift: BlobIO[Unit] = module.shift
      def evalOn[A](ec: ExecutionContext)(fa: BlobIO[A]) = module.evalOn(ec)(fa)
    }

  // BlobIO is a MessageLogger
  implicit val MessageLoggerBlobIO: MessageLogger[BlobIO] =
    new MessageLogger[BlobIO] {
      def error(message: => String): BlobIO[Unit] =  module.error(message)
      def warn(message: => String): BlobIO[Unit] = module.warn(message)
      def info(message: => String): BlobIO[Unit] = module.info(message)
      def debug(message: => String): BlobIO[Unit] =  module.debug(message)
      def trace(message: => String): BlobIO[Unit] =  module.trace(message)
    }
}

