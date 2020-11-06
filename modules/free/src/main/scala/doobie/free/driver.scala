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

import java.lang.String
import java.sql.Connection
import java.sql.Driver
import java.sql.DriverPropertyInfo
import java.util.Properties
import java.util.logging.Logger

@silent("deprecated")
object driver { module =>

  // Algebra of operations for Driver. Each accepts a visitor as an alternative to pattern-matching.
  sealed trait DriverOp[A] {
    def visit[F[_]](v: DriverOp.Visitor[F]): F[A]
  }

  // Free monad over DriverOp.
  type DriverIO[A] = FF[DriverOp, A]

  // Module of instances and constructors of DriverOp.
  object DriverOp {

    // Given a Driver we can embed a DriverIO program in any algebra that understands embedding.
    implicit val DriverOpEmbeddable: Embeddable[DriverOp, Driver] =
      new Embeddable[DriverOp, Driver] {
        def embed[A](j: Driver, fa: FF[DriverOp, A]) = Embedded.Driver(j, fa)
      }

    // Interface for a natural transformation DriverOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (DriverOp ~> F) {
      final def apply[A](fa: DriverOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](message: => String, f: Driver => A): F[A]
      def embed[A](e: Embedded[A]): F[A]

      // Async
      def delay[A](a: () => A): F[A]
      def handleErrorWith[A](fa: DriverIO[A], f: Throwable => DriverIO[A]): F[A]
      def raiseError[A](e: Throwable): F[A]
      def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]
      def asyncF[A](k: (Either[Throwable, A] => Unit) => DriverIO[Unit]): F[A]
      def bracketCase[A, B](acquire: DriverIO[A])(use: A => DriverIO[B])(release: (A, ExitCase[Throwable]) => DriverIO[Unit]): F[B]
      def shift: F[Unit]
      def evalOn[A](ec: ExecutionContext)(fa: DriverIO[A]): F[A]

      // Logger
      def error(message: => String): F[Unit]
      def warn(message: => String): F[Unit]
      def info(message: => String): F[Unit]
      def debug(message: => String): F[Unit]
      def trace(message: => String): F[Unit]

      // Driver
      def acceptsURL(a: String): F[Boolean]
      def connect(a: String, b: Properties): F[Connection]
      def getMajorVersion: F[Int]
      def getMinorVersion: F[Int]
      def getParentLogger: F[Logger]
      def getPropertyInfo(a: String, b: Properties): F[Array[DriverPropertyInfo]]
      def jdbcCompliant: F[Boolean]

    }

    // Common operations for all algebras.
    final case class Raw[A](message: () => String, f: Driver => A) extends DriverOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(message(), f)
    }
    final case class Embed[A](e: Embedded[A]) extends DriverOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class Delay[A](a: () => A) extends DriverOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.delay(a)
    }
    final case class HandleErrorWith[A](fa: DriverIO[A], f: Throwable => DriverIO[A]) extends DriverOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa, f)
    }
    final case class RaiseError[A](e: Throwable) extends DriverOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raiseError(e)
    }
    final case class Async1[A](k: (Either[Throwable, A] => Unit) => Unit) extends DriverOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.async(k)
    }
    final case class AsyncF[A](k: (Either[Throwable, A] => Unit) => DriverIO[Unit]) extends DriverOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.asyncF(k)
    }
    final case class BracketCase[A, B](acquire: DriverIO[A], use: A => DriverIO[B], release: (A, ExitCase[Throwable]) => DriverIO[Unit]) extends DriverOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.bracketCase(acquire)(use)(release)
    }
    final case object Shift extends DriverOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.shift
    }
    final case class EvalOn[A](ec: ExecutionContext, fa: DriverIO[A]) extends DriverOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.evalOn(ec)(fa)
    }
    final case class LogError(message: () => String) extends DriverOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.error(message()): F[Unit]
    }
    final case class LogWarn(message: () => String) extends DriverOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.warn(message()): F[Unit]
    }
    final case class LogInfo(message: () => String) extends DriverOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.info(message()): F[Unit]
    }
    final case class LogDebug(message: () => String) extends DriverOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.debug(message()): F[Unit]
    }
    final case class LogTrace(message: () => String) extends DriverOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.trace(message()): F[Unit]
    }

    // Driver-specific operations.
    final case class  AcceptsURL(a: String) extends DriverOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.acceptsURL(a)
    }
    final case class  Connect(a: String, b: Properties) extends DriverOp[Connection] {
      def visit[F[_]](v: Visitor[F]) = v.connect(a, b)
    }
    final case object GetMajorVersion extends DriverOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMajorVersion
    }
    final case object GetMinorVersion extends DriverOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMinorVersion
    }
    final case object GetParentLogger extends DriverOp[Logger] {
      def visit[F[_]](v: Visitor[F]) = v.getParentLogger
    }
    final case class  GetPropertyInfo(a: String, b: Properties) extends DriverOp[Array[DriverPropertyInfo]] {
      def visit[F[_]](v: Visitor[F]) = v.getPropertyInfo(a, b)
    }
    final case object JdbcCompliant extends DriverOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.jdbcCompliant
    }

  }
  import DriverOp._

  // Smart constructors for operations common to all algebras.
  val unit: DriverIO[Unit] = FF.pure[DriverOp, Unit](())
  def pure[A](a: A): DriverIO[A] = FF.pure[DriverOp, A](a)
  def raw[A](message: => String)(f: Driver => A): DriverIO[A] = FF.liftF(Raw(() => message, f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[DriverOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def delay[A](a: => A): DriverIO[A] = FF.liftF(Delay(() => a))
  def handleErrorWith[A](fa: DriverIO[A], f: Throwable => DriverIO[A]): DriverIO[A] = FF.liftF[DriverOp, A](HandleErrorWith(fa, f))
  def raiseError[A](err: Throwable): DriverIO[A] = FF.liftF[DriverOp, A](RaiseError(err))
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): DriverIO[A] = FF.liftF[DriverOp, A](Async1(k))
  def asyncF[A](k: (Either[Throwable, A] => Unit) => DriverIO[Unit]): DriverIO[A] = FF.liftF[DriverOp, A](AsyncF(k))
  def bracketCase[A, B](acquire: DriverIO[A])(use: A => DriverIO[B])(release: (A, ExitCase[Throwable]) => DriverIO[Unit]): DriverIO[B] = FF.liftF[DriverOp, B](BracketCase(acquire, use, release))
  val shift: DriverIO[Unit] = FF.liftF[DriverOp, Unit](Shift)
  def evalOn[A](ec: ExecutionContext)(fa: DriverIO[A]) = FF.liftF[DriverOp, A](EvalOn(ec, fa))

  // Logger
  def error(message: => String): DriverIO[Unit] = FF.liftF[DriverOp, Unit](LogError(() => message))
  def warn(message: => String): DriverIO[Unit] = FF.liftF[DriverOp, Unit](LogWarn(() => message))
  def info(message: => String): DriverIO[Unit] = FF.liftF[DriverOp, Unit](LogInfo(() => message))
  def debug(message: => String): DriverIO[Unit] = FF.liftF[DriverOp, Unit](LogDebug(() => message))
  def trace(message: => String): DriverIO[Unit] = FF.liftF[DriverOp, Unit](LogTrace(() => message))

  // Smart constructors for Driver-specific operations.
  def acceptsURL(a: String): DriverIO[Boolean] = FF.liftF(AcceptsURL(a))
  def connect(a: String, b: Properties): DriverIO[Connection] = FF.liftF(Connect(a, b))
  val getMajorVersion: DriverIO[Int] = FF.liftF(GetMajorVersion)
  val getMinorVersion: DriverIO[Int] = FF.liftF(GetMinorVersion)
  val getParentLogger: DriverIO[Logger] = FF.liftF(GetParentLogger)
  def getPropertyInfo(a: String, b: Properties): DriverIO[Array[DriverPropertyInfo]] = FF.liftF(GetPropertyInfo(a, b))
  val jdbcCompliant: DriverIO[Boolean] = FF.liftF(JdbcCompliant)

  // DriverIO is an Async
  implicit val AsyncDriverIO: Async[DriverIO] =
    new Async[DriverIO] {
      val asyncM = FF.catsFreeMonadForFree[DriverOp]
      def bracketCase[A, B](acquire: DriverIO[A])(use: A => DriverIO[B])(release: (A, ExitCase[Throwable]) => DriverIO[Unit]): DriverIO[B] = module.bracketCase(acquire)(use)(release)
      def pure[A](x: A): DriverIO[A] = asyncM.pure(x)
      def handleErrorWith[A](fa: DriverIO[A])(f: Throwable => DriverIO[A]): DriverIO[A] = module.handleErrorWith(fa, f)
      def raiseError[A](e: Throwable): DriverIO[A] = module.raiseError(e)
      def async[A](k: (Either[Throwable,A] => Unit) => Unit): DriverIO[A] = module.async(k)
      def asyncF[A](k: (Either[Throwable,A] => Unit) => DriverIO[Unit]): DriverIO[A] = module.asyncF(k)
      def flatMap[A, B](fa: DriverIO[A])(f: A => DriverIO[B]): DriverIO[B] = asyncM.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => DriverIO[Either[A, B]]): DriverIO[B] = asyncM.tailRecM(a)(f)
      def suspend[A](thunk: => DriverIO[A]): DriverIO[A] = asyncM.flatten(module.delay(thunk))
    }

  // DriverIO is a ContextShift
  implicit val ContextShiftDriverIO: ContextShift[DriverIO] =
    new ContextShift[DriverIO] {
      def shift: DriverIO[Unit] = module.shift
      def evalOn[A](ec: ExecutionContext)(fa: DriverIO[A]) = module.evalOn(ec)(fa)
    }

  // DriverIO is a MessageLogger
  implicit val MessageLoggerDriverIO: MessageLogger[DriverIO] =
    new MessageLogger[DriverIO] {
      def error(message: => String): DriverIO[Unit] =  module.error(message)
      def warn(message: => String): DriverIO[Unit] = module.warn(message)
      def info(message: => String): DriverIO[Unit] = module.info(message)
      def debug(message: => String): DriverIO[Unit] =  module.debug(message)
      def trace(message: => String): DriverIO[Unit] =  module.trace(message)
    }
}

