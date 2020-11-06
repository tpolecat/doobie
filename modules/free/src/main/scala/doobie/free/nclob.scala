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
import java.io.Reader
import java.io.Writer
import java.lang.String
import java.sql.Clob
import java.sql.NClob

@silent("deprecated")
object nclob { module =>

  // Algebra of operations for NClob. Each accepts a visitor as an alternative to pattern-matching.
  sealed trait NClobOp[A] {
    def visit[F[_]](v: NClobOp.Visitor[F]): F[A]
  }

  // Free monad over NClobOp.
  type NClobIO[A] = FF[NClobOp, A]

  // Module of instances and constructors of NClobOp.
  object NClobOp {

    // Given a NClob we can embed a NClobIO program in any algebra that understands embedding.
    implicit val NClobOpEmbeddable: Embeddable[NClobOp, NClob] =
      new Embeddable[NClobOp, NClob] {
        def embed[A](j: NClob, fa: FF[NClobOp, A]) = Embedded.NClob(j, fa)
      }

    // Interface for a natural transformation NClobOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (NClobOp ~> F) {
      final def apply[A](fa: NClobOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](message: => String, f: NClob => A): F[A]
      def embed[A](e: Embedded[A]): F[A]

      // Async
      def delay[A](a: () => A): F[A]
      def handleErrorWith[A](fa: NClobIO[A], f: Throwable => NClobIO[A]): F[A]
      def raiseError[A](e: Throwable): F[A]
      def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]
      def asyncF[A](k: (Either[Throwable, A] => Unit) => NClobIO[Unit]): F[A]
      def bracketCase[A, B](acquire: NClobIO[A])(use: A => NClobIO[B])(release: (A, ExitCase[Throwable]) => NClobIO[Unit]): F[B]
      def shift: F[Unit]
      def evalOn[A](ec: ExecutionContext)(fa: NClobIO[A]): F[A]

      // Logger
      def error(message: => String): F[Unit]
      def warn(message: => String): F[Unit]
      def info(message: => String): F[Unit]
      def debug(message: => String): F[Unit]
      def trace(message: => String): F[Unit]

      // NClob
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
    final case class Raw[A](message: () => String, f: NClob => A) extends NClobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(message(), f)
    }
    final case class Embed[A](e: Embedded[A]) extends NClobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class Delay[A](a: () => A) extends NClobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.delay(a)
    }
    final case class HandleErrorWith[A](fa: NClobIO[A], f: Throwable => NClobIO[A]) extends NClobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa, f)
    }
    final case class RaiseError[A](e: Throwable) extends NClobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raiseError(e)
    }
    final case class Async1[A](k: (Either[Throwable, A] => Unit) => Unit) extends NClobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.async(k)
    }
    final case class AsyncF[A](k: (Either[Throwable, A] => Unit) => NClobIO[Unit]) extends NClobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.asyncF(k)
    }
    final case class BracketCase[A, B](acquire: NClobIO[A], use: A => NClobIO[B], release: (A, ExitCase[Throwable]) => NClobIO[Unit]) extends NClobOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.bracketCase(acquire)(use)(release)
    }
    final case object Shift extends NClobOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.shift
    }
    final case class EvalOn[A](ec: ExecutionContext, fa: NClobIO[A]) extends NClobOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.evalOn(ec)(fa)
    }
    final case class LogError(message: () => String) extends NClobOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.error(message()): F[Unit]
    }
    final case class LogWarn(message: () => String) extends NClobOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.warn(message()): F[Unit]
    }
    final case class LogInfo(message: () => String) extends NClobOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.info(message()): F[Unit]
    }
    final case class LogDebug(message: () => String) extends NClobOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.debug(message()): F[Unit]
    }
    final case class LogTrace(message: () => String) extends NClobOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.trace(message()): F[Unit]
    }

    // NClob-specific operations.
    final case object Free extends NClobOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.free
    }
    final case object GetAsciiStream extends NClobOp[InputStream] {
      def visit[F[_]](v: Visitor[F]) = v.getAsciiStream
    }
    final case object GetCharacterStream extends NClobOp[Reader] {
      def visit[F[_]](v: Visitor[F]) = v.getCharacterStream
    }
    final case class  GetCharacterStream1(a: Long, b: Long) extends NClobOp[Reader] {
      def visit[F[_]](v: Visitor[F]) = v.getCharacterStream(a, b)
    }
    final case class  GetSubString(a: Long, b: Int) extends NClobOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getSubString(a, b)
    }
    final case object Length extends NClobOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.length
    }
    final case class  Position(a: Clob, b: Long) extends NClobOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.position(a, b)
    }
    final case class  Position1(a: String, b: Long) extends NClobOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.position(a, b)
    }
    final case class  SetAsciiStream(a: Long) extends NClobOp[OutputStream] {
      def visit[F[_]](v: Visitor[F]) = v.setAsciiStream(a)
    }
    final case class  SetCharacterStream(a: Long) extends NClobOp[Writer] {
      def visit[F[_]](v: Visitor[F]) = v.setCharacterStream(a)
    }
    final case class  SetString(a: Long, b: String) extends NClobOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.setString(a, b)
    }
    final case class  SetString1(a: Long, b: String, c: Int, d: Int) extends NClobOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.setString(a, b, c, d)
    }
    final case class  Truncate(a: Long) extends NClobOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.truncate(a)
    }

  }
  import NClobOp._

  // Smart constructors for operations common to all algebras.
  val unit: NClobIO[Unit] = FF.pure[NClobOp, Unit](())
  def pure[A](a: A): NClobIO[A] = FF.pure[NClobOp, A](a)
  def raw[A](message: => String)(f: NClob => A): NClobIO[A] = FF.liftF(Raw(() => message, f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[NClobOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def delay[A](a: => A): NClobIO[A] = FF.liftF(Delay(() => a))
  def handleErrorWith[A](fa: NClobIO[A], f: Throwable => NClobIO[A]): NClobIO[A] = FF.liftF[NClobOp, A](HandleErrorWith(fa, f))
  def raiseError[A](err: Throwable): NClobIO[A] = FF.liftF[NClobOp, A](RaiseError(err))
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): NClobIO[A] = FF.liftF[NClobOp, A](Async1(k))
  def asyncF[A](k: (Either[Throwable, A] => Unit) => NClobIO[Unit]): NClobIO[A] = FF.liftF[NClobOp, A](AsyncF(k))
  def bracketCase[A, B](acquire: NClobIO[A])(use: A => NClobIO[B])(release: (A, ExitCase[Throwable]) => NClobIO[Unit]): NClobIO[B] = FF.liftF[NClobOp, B](BracketCase(acquire, use, release))
  val shift: NClobIO[Unit] = FF.liftF[NClobOp, Unit](Shift)
  def evalOn[A](ec: ExecutionContext)(fa: NClobIO[A]) = FF.liftF[NClobOp, A](EvalOn(ec, fa))

  // Logger
  def error(message: => String): NClobIO[Unit] = FF.liftF[NClobOp, Unit](LogError(() => message))
  def warn(message: => String): NClobIO[Unit] = FF.liftF[NClobOp, Unit](LogWarn(() => message))
  def info(message: => String): NClobIO[Unit] = FF.liftF[NClobOp, Unit](LogInfo(() => message))
  def debug(message: => String): NClobIO[Unit] = FF.liftF[NClobOp, Unit](LogDebug(() => message))
  def trace(message: => String): NClobIO[Unit] = FF.liftF[NClobOp, Unit](LogTrace(() => message))

  // Smart constructors for NClob-specific operations.
  val free: NClobIO[Unit] = FF.liftF(Free)
  val getAsciiStream: NClobIO[InputStream] = FF.liftF(GetAsciiStream)
  val getCharacterStream: NClobIO[Reader] = FF.liftF(GetCharacterStream)
  def getCharacterStream(a: Long, b: Long): NClobIO[Reader] = FF.liftF(GetCharacterStream1(a, b))
  def getSubString(a: Long, b: Int): NClobIO[String] = FF.liftF(GetSubString(a, b))
  val length: NClobIO[Long] = FF.liftF(Length)
  def position(a: Clob, b: Long): NClobIO[Long] = FF.liftF(Position(a, b))
  def position(a: String, b: Long): NClobIO[Long] = FF.liftF(Position1(a, b))
  def setAsciiStream(a: Long): NClobIO[OutputStream] = FF.liftF(SetAsciiStream(a))
  def setCharacterStream(a: Long): NClobIO[Writer] = FF.liftF(SetCharacterStream(a))
  def setString(a: Long, b: String): NClobIO[Int] = FF.liftF(SetString(a, b))
  def setString(a: Long, b: String, c: Int, d: Int): NClobIO[Int] = FF.liftF(SetString1(a, b, c, d))
  def truncate(a: Long): NClobIO[Unit] = FF.liftF(Truncate(a))

  // NClobIO is an Async
  implicit val AsyncNClobIO: Async[NClobIO] =
    new Async[NClobIO] {
      val asyncM = FF.catsFreeMonadForFree[NClobOp]
      def bracketCase[A, B](acquire: NClobIO[A])(use: A => NClobIO[B])(release: (A, ExitCase[Throwable]) => NClobIO[Unit]): NClobIO[B] = module.bracketCase(acquire)(use)(release)
      def pure[A](x: A): NClobIO[A] = asyncM.pure(x)
      def handleErrorWith[A](fa: NClobIO[A])(f: Throwable => NClobIO[A]): NClobIO[A] = module.handleErrorWith(fa, f)
      def raiseError[A](e: Throwable): NClobIO[A] = module.raiseError(e)
      def async[A](k: (Either[Throwable,A] => Unit) => Unit): NClobIO[A] = module.async(k)
      def asyncF[A](k: (Either[Throwable,A] => Unit) => NClobIO[Unit]): NClobIO[A] = module.asyncF(k)
      def flatMap[A, B](fa: NClobIO[A])(f: A => NClobIO[B]): NClobIO[B] = asyncM.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => NClobIO[Either[A, B]]): NClobIO[B] = asyncM.tailRecM(a)(f)
      def suspend[A](thunk: => NClobIO[A]): NClobIO[A] = asyncM.flatten(module.delay(thunk))
    }

  // NClobIO is a ContextShift
  implicit val ContextShiftNClobIO: ContextShift[NClobIO] =
    new ContextShift[NClobIO] {
      def shift: NClobIO[Unit] = module.shift
      def evalOn[A](ec: ExecutionContext)(fa: NClobIO[A]) = module.evalOn(ec)(fa)
    }

  // NClobIO is a MessageLogger
  implicit val MessageLoggerNClobIO: MessageLogger[NClobIO] =
    new MessageLogger[NClobIO] {
      def error(message: => String): NClobIO[Unit] =  module.error(message)
      def warn(message: => String): NClobIO[Unit] = module.warn(message)
      def info(message: => String): NClobIO[Unit] = module.info(message)
      def debug(message: => String): NClobIO[Unit] =  module.debug(message)
      def trace(message: => String): NClobIO[Unit] =  module.trace(message)
    }
}

