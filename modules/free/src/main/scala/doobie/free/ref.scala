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
import java.sql.Ref
import java.util.Map

@silent("deprecated")
object ref { module =>

  // Algebra of operations for Ref. Each accepts a visitor as an alternative to pattern-matching.
  sealed trait RefOp[A] {
    def visit[F[_]](v: RefOp.Visitor[F]): F[A]
  }

  // Free monad over RefOp.
  type RefIO[A] = FF[RefOp, A]

  // Module of instances and constructors of RefOp.
  object RefOp {

    // Given a Ref we can embed a RefIO program in any algebra that understands embedding.
    implicit val RefOpEmbeddable: Embeddable[RefOp, Ref] =
      new Embeddable[RefOp, Ref] {
        def embed[A](j: Ref, fa: FF[RefOp, A]) = Embedded.Ref(j, fa)
      }

    // Interface for a natural transformation RefOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (RefOp ~> F) {
      final def apply[A](fa: RefOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](message: => String, f: Ref => A): F[A]
      def embed[A](e: Embedded[A]): F[A]

      // Async
      def delay[A](a: () => A): F[A]
      def handleErrorWith[A](fa: RefIO[A], f: Throwable => RefIO[A]): F[A]
      def raiseError[A](e: Throwable): F[A]
      def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]
      def asyncF[A](k: (Either[Throwable, A] => Unit) => RefIO[Unit]): F[A]
      def bracketCase[A, B](acquire: RefIO[A])(use: A => RefIO[B])(release: (A, ExitCase[Throwable]) => RefIO[Unit]): F[B]
      def shift: F[Unit]
      def evalOn[A](ec: ExecutionContext)(fa: RefIO[A]): F[A]

      // Logger
      def error(message: => String): F[Unit]
      def warn(message: => String): F[Unit]
      def info(message: => String): F[Unit]
      def debug(message: => String): F[Unit]
      def trace(message: => String): F[Unit]

      // Ref
      def getBaseTypeName: F[String]
      def getObject: F[AnyRef]
      def getObject(a: Map[String, Class[_]]): F[AnyRef]
      def setObject(a: AnyRef): F[Unit]

    }

    // Common operations for all algebras.
    final case class Raw[A](message: () => String, f: Ref => A) extends RefOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(message(), f)
    }
    final case class Embed[A](e: Embedded[A]) extends RefOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class Delay[A](a: () => A) extends RefOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.delay(a)
    }
    final case class HandleErrorWith[A](fa: RefIO[A], f: Throwable => RefIO[A]) extends RefOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa, f)
    }
    final case class RaiseError[A](e: Throwable) extends RefOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raiseError(e)
    }
    final case class Async1[A](k: (Either[Throwable, A] => Unit) => Unit) extends RefOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.async(k)
    }
    final case class AsyncF[A](k: (Either[Throwable, A] => Unit) => RefIO[Unit]) extends RefOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.asyncF(k)
    }
    final case class BracketCase[A, B](acquire: RefIO[A], use: A => RefIO[B], release: (A, ExitCase[Throwable]) => RefIO[Unit]) extends RefOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.bracketCase(acquire)(use)(release)
    }
    final case object Shift extends RefOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.shift
    }
    final case class EvalOn[A](ec: ExecutionContext, fa: RefIO[A]) extends RefOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.evalOn(ec)(fa)
    }
    final case class LogError(message: () => String) extends RefOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.error(message()): F[Unit]
    }
    final case class LogWarn(message: () => String) extends RefOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.warn(message()): F[Unit]
    }
    final case class LogInfo(message: () => String) extends RefOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.info(message()): F[Unit]
    }
    final case class LogDebug(message: () => String) extends RefOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.debug(message()): F[Unit]
    }
    final case class LogTrace(message: () => String) extends RefOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.trace(message()): F[Unit]
    }

    // Ref-specific operations.
    final case object GetBaseTypeName extends RefOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getBaseTypeName
    }
    final case object GetObject extends RefOp[AnyRef] {
      def visit[F[_]](v: Visitor[F]) = v.getObject
    }
    final case class  GetObject1(a: Map[String, Class[_]]) extends RefOp[AnyRef] {
      def visit[F[_]](v: Visitor[F]) = v.getObject(a)
    }
    final case class  SetObject(a: AnyRef) extends RefOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setObject(a)
    }

  }
  import RefOp._

  // Smart constructors for operations common to all algebras.
  val unit: RefIO[Unit] = FF.pure[RefOp, Unit](())
  def pure[A](a: A): RefIO[A] = FF.pure[RefOp, A](a)
  def raw[A](message: => String)(f: Ref => A): RefIO[A] = FF.liftF(Raw(() => message, f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[RefOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def delay[A](a: => A): RefIO[A] = FF.liftF(Delay(() => a))
  def handleErrorWith[A](fa: RefIO[A], f: Throwable => RefIO[A]): RefIO[A] = FF.liftF[RefOp, A](HandleErrorWith(fa, f))
  def raiseError[A](err: Throwable): RefIO[A] = FF.liftF[RefOp, A](RaiseError(err))
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): RefIO[A] = FF.liftF[RefOp, A](Async1(k))
  def asyncF[A](k: (Either[Throwable, A] => Unit) => RefIO[Unit]): RefIO[A] = FF.liftF[RefOp, A](AsyncF(k))
  def bracketCase[A, B](acquire: RefIO[A])(use: A => RefIO[B])(release: (A, ExitCase[Throwable]) => RefIO[Unit]): RefIO[B] = FF.liftF[RefOp, B](BracketCase(acquire, use, release))
  val shift: RefIO[Unit] = FF.liftF[RefOp, Unit](Shift)
  def evalOn[A](ec: ExecutionContext)(fa: RefIO[A]) = FF.liftF[RefOp, A](EvalOn(ec, fa))

  // Logger
  def error(message: => String): RefIO[Unit] = FF.liftF[RefOp, Unit](LogError(() => message))
  def warn(message: => String): RefIO[Unit] = FF.liftF[RefOp, Unit](LogWarn(() => message))
  def info(message: => String): RefIO[Unit] = FF.liftF[RefOp, Unit](LogInfo(() => message))
  def debug(message: => String): RefIO[Unit] = FF.liftF[RefOp, Unit](LogDebug(() => message))
  def trace(message: => String): RefIO[Unit] = FF.liftF[RefOp, Unit](LogTrace(() => message))

  // Smart constructors for Ref-specific operations.
  val getBaseTypeName: RefIO[String] = FF.liftF(GetBaseTypeName)
  val getObject: RefIO[AnyRef] = FF.liftF(GetObject)
  def getObject(a: Map[String, Class[_]]): RefIO[AnyRef] = FF.liftF(GetObject1(a))
  def setObject(a: AnyRef): RefIO[Unit] = FF.liftF(SetObject(a))

  // RefIO is an Async
  implicit val AsyncRefIO: Async[RefIO] =
    new Async[RefIO] {
      val asyncM = FF.catsFreeMonadForFree[RefOp]
      def bracketCase[A, B](acquire: RefIO[A])(use: A => RefIO[B])(release: (A, ExitCase[Throwable]) => RefIO[Unit]): RefIO[B] = module.bracketCase(acquire)(use)(release)
      def pure[A](x: A): RefIO[A] = asyncM.pure(x)
      def handleErrorWith[A](fa: RefIO[A])(f: Throwable => RefIO[A]): RefIO[A] = module.handleErrorWith(fa, f)
      def raiseError[A](e: Throwable): RefIO[A] = module.raiseError(e)
      def async[A](k: (Either[Throwable,A] => Unit) => Unit): RefIO[A] = module.async(k)
      def asyncF[A](k: (Either[Throwable,A] => Unit) => RefIO[Unit]): RefIO[A] = module.asyncF(k)
      def flatMap[A, B](fa: RefIO[A])(f: A => RefIO[B]): RefIO[B] = asyncM.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => RefIO[Either[A, B]]): RefIO[B] = asyncM.tailRecM(a)(f)
      def suspend[A](thunk: => RefIO[A]): RefIO[A] = asyncM.flatten(module.delay(thunk))
    }

  // RefIO is a ContextShift
  implicit val ContextShiftRefIO: ContextShift[RefIO] =
    new ContextShift[RefIO] {
      def shift: RefIO[Unit] = module.shift
      def evalOn[A](ec: ExecutionContext)(fa: RefIO[A]) = module.evalOn(ec)(fa)
    }

  // RefIO is a MessageLogger
  implicit val MessageLoggerRefIO: MessageLogger[RefIO] =
    new MessageLogger[RefIO] {
      def error(message: => String): RefIO[Unit] =  module.error(message)
      def warn(message: => String): RefIO[Unit] = module.warn(message)
      def info(message: => String): RefIO[Unit] = module.info(message)
      def debug(message: => String): RefIO[Unit] =  module.debug(message)
      def trace(message: => String): RefIO[Unit] =  module.trace(message)
    }
}

