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

import java.lang.String
import java.sql.ResultSet
import org.postgresql.fastpath.FastpathArg
import org.postgresql.fastpath.{ Fastpath => PGFastpath }

@silent("deprecated")
object fastpath { module =>

  // Algebra of operations for PGFastpath. Each accepts a visitor as an alternative to pattern-matching.
  sealed trait FastpathOp[A] {
    def visit[F[_]](v: FastpathOp.Visitor[F]): F[A]
  }

  // Free monad over FastpathOp.
  type FastpathIO[A] = FF[FastpathOp, A]

  // Module of instances and constructors of FastpathOp.
  object FastpathOp {

    // Given a PGFastpath we can embed a FastpathIO program in any algebra that understands embedding.
    implicit val FastpathOpEmbeddable: Embeddable[FastpathOp, PGFastpath] =
      new Embeddable[FastpathOp, PGFastpath] {
        def embed[A](j: PGFastpath, fa: FF[FastpathOp, A]) = Embedded.Fastpath(j, fa)
      }

    // Interface for a natural transformation FastpathOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (FastpathOp ~> F) {
      final def apply[A](fa: FastpathOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](message: => String, f: PGFastpath => A): F[A]
      def embed[A](e: Embedded[A]): F[A]

      // Async
      def delay[A](a: () => A): F[A]
      def handleErrorWith[A](fa: FastpathIO[A], f: Throwable => FastpathIO[A]): F[A]
      def raiseError[A](e: Throwable): F[A]
      def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]
      def asyncF[A](k: (Either[Throwable, A] => Unit) => FastpathIO[Unit]): F[A]
      def bracketCase[A, B](acquire: FastpathIO[A])(use: A => FastpathIO[B])(release: (A, ExitCase[Throwable]) => FastpathIO[Unit]): F[B]
      def shift: F[Unit]
      def evalOn[A](ec: ExecutionContext)(fa: FastpathIO[A]): F[A]

      // Logger
      def error(message: => String): F[Unit]
      def warn(message: => String): F[Unit]
      def info(message: => String): F[Unit]
      def debug(message: => String): F[Unit]
      def trace(message: => String): F[Unit]

      // PGFastpath
      def addFunction(a: String, b: Int): F[Unit]
      def addFunctions(a: ResultSet): F[Unit]
      def fastpath(a: Int, b: Array[FastpathArg]): F[Array[Byte]]
      def fastpath(a: Int, b: Boolean, c: Array[FastpathArg]): F[AnyRef]
      def fastpath(a: String, b: Array[FastpathArg]): F[Array[Byte]]
      def fastpath(a: String, b: Boolean, c: Array[FastpathArg]): F[AnyRef]
      def getData(a: String, b: Array[FastpathArg]): F[Array[Byte]]
      def getID(a: String): F[Int]
      def getInteger(a: String, b: Array[FastpathArg]): F[Int]
      def getLong(a: String, b: Array[FastpathArg]): F[Long]
      def getOID(a: String, b: Array[FastpathArg]): F[Long]

    }

    // Common operations for all algebras.
    final case class Raw[A](message: () => String, f: PGFastpath => A) extends FastpathOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(message(), f)
    }
    final case class Embed[A](e: Embedded[A]) extends FastpathOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class Delay[A](a: () => A) extends FastpathOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.delay(a)
    }
    final case class HandleErrorWith[A](fa: FastpathIO[A], f: Throwable => FastpathIO[A]) extends FastpathOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa, f)
    }
    final case class RaiseError[A](e: Throwable) extends FastpathOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raiseError(e)
    }
    final case class Async1[A](k: (Either[Throwable, A] => Unit) => Unit) extends FastpathOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.async(k)
    }
    final case class AsyncF[A](k: (Either[Throwable, A] => Unit) => FastpathIO[Unit]) extends FastpathOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.asyncF(k)
    }
    final case class BracketCase[A, B](acquire: FastpathIO[A], use: A => FastpathIO[B], release: (A, ExitCase[Throwable]) => FastpathIO[Unit]) extends FastpathOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.bracketCase(acquire)(use)(release)
    }
    final case object Shift extends FastpathOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.shift
    }
    final case class EvalOn[A](ec: ExecutionContext, fa: FastpathIO[A]) extends FastpathOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.evalOn(ec)(fa)
    }
    final case class LogError(message: () => String) extends FastpathOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.error(message()): F[Unit]
    }
    final case class LogWarn(message: () => String) extends FastpathOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.warn(message()): F[Unit]
    }
    final case class LogInfo(message: () => String) extends FastpathOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.info(message()): F[Unit]
    }
    final case class LogDebug(message: () => String) extends FastpathOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.debug(message()): F[Unit]
    }
    final case class LogTrace(message: () => String) extends FastpathOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.trace(message()): F[Unit]
    }

    // PGFastpath-specific operations.
    final case class  AddFunction(a: String, b: Int) extends FastpathOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.addFunction(a, b)
    }
    final case class  AddFunctions(a: ResultSet) extends FastpathOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.addFunctions(a)
    }
    final case class  Fastpath(a: Int, b: Array[FastpathArg]) extends FastpathOp[Array[Byte]] {
      def visit[F[_]](v: Visitor[F]) = v.fastpath(a, b)
    }
    final case class  Fastpath1(a: Int, b: Boolean, c: Array[FastpathArg]) extends FastpathOp[AnyRef] {
      def visit[F[_]](v: Visitor[F]) = v.fastpath(a, b, c)
    }
    final case class  Fastpath2(a: String, b: Array[FastpathArg]) extends FastpathOp[Array[Byte]] {
      def visit[F[_]](v: Visitor[F]) = v.fastpath(a, b)
    }
    final case class  Fastpath3(a: String, b: Boolean, c: Array[FastpathArg]) extends FastpathOp[AnyRef] {
      def visit[F[_]](v: Visitor[F]) = v.fastpath(a, b, c)
    }
    final case class  GetData(a: String, b: Array[FastpathArg]) extends FastpathOp[Array[Byte]] {
      def visit[F[_]](v: Visitor[F]) = v.getData(a, b)
    }
    final case class  GetID(a: String) extends FastpathOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getID(a)
    }
    final case class  GetInteger(a: String, b: Array[FastpathArg]) extends FastpathOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getInteger(a, b)
    }
    final case class  GetLong(a: String, b: Array[FastpathArg]) extends FastpathOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.getLong(a, b)
    }
    final case class  GetOID(a: String, b: Array[FastpathArg]) extends FastpathOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.getOID(a, b)
    }

  }
  import FastpathOp._

  // Smart constructors for operations common to all algebras.
  val unit: FastpathIO[Unit] = FF.pure[FastpathOp, Unit](())
  def pure[A](a: A): FastpathIO[A] = FF.pure[FastpathOp, A](a)
  def raw[A](message: => String)(f: PGFastpath => A): FastpathIO[A] = FF.liftF(Raw(() => message, f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[FastpathOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def delay[A](a: => A): FastpathIO[A] = FF.liftF(Delay(() => a))
  def handleErrorWith[A](fa: FastpathIO[A], f: Throwable => FastpathIO[A]): FastpathIO[A] = FF.liftF[FastpathOp, A](HandleErrorWith(fa, f))
  def raiseError[A](err: Throwable): FastpathIO[A] = FF.liftF[FastpathOp, A](RaiseError(err))
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): FastpathIO[A] = FF.liftF[FastpathOp, A](Async1(k))
  def asyncF[A](k: (Either[Throwable, A] => Unit) => FastpathIO[Unit]): FastpathIO[A] = FF.liftF[FastpathOp, A](AsyncF(k))
  def bracketCase[A, B](acquire: FastpathIO[A])(use: A => FastpathIO[B])(release: (A, ExitCase[Throwable]) => FastpathIO[Unit]): FastpathIO[B] = FF.liftF[FastpathOp, B](BracketCase(acquire, use, release))
  val shift: FastpathIO[Unit] = FF.liftF[FastpathOp, Unit](Shift)
  def evalOn[A](ec: ExecutionContext)(fa: FastpathIO[A]) = FF.liftF[FastpathOp, A](EvalOn(ec, fa))

  // Logger
  def error(message: => String): FastpathIO[Unit] = FF.liftF[FastpathOp, Unit](LogError(() => message))
  def warn(message: => String): FastpathIO[Unit] = FF.liftF[FastpathOp, Unit](LogWarn(() => message))
  def info(message: => String): FastpathIO[Unit] = FF.liftF[FastpathOp, Unit](LogInfo(() => message))
  def debug(message: => String): FastpathIO[Unit] = FF.liftF[FastpathOp, Unit](LogDebug(() => message))
  def trace(message: => String): FastpathIO[Unit] = FF.liftF[FastpathOp, Unit](LogTrace(() => message))

  // Smart constructors for Fastpath-specific operations.
  def addFunction(a: String, b: Int): FastpathIO[Unit] = FF.liftF(AddFunction(a, b))
  def addFunctions(a: ResultSet): FastpathIO[Unit] = FF.liftF(AddFunctions(a))
  def fastpath(a: Int, b: Array[FastpathArg]): FastpathIO[Array[Byte]] = FF.liftF(Fastpath(a, b))
  def fastpath(a: Int, b: Boolean, c: Array[FastpathArg]): FastpathIO[AnyRef] = FF.liftF(Fastpath1(a, b, c))
  def fastpath(a: String, b: Array[FastpathArg]): FastpathIO[Array[Byte]] = FF.liftF(Fastpath2(a, b))
  def fastpath(a: String, b: Boolean, c: Array[FastpathArg]): FastpathIO[AnyRef] = FF.liftF(Fastpath3(a, b, c))
  def getData(a: String, b: Array[FastpathArg]): FastpathIO[Array[Byte]] = FF.liftF(GetData(a, b))
  def getID(a: String): FastpathIO[Int] = FF.liftF(GetID(a))
  def getInteger(a: String, b: Array[FastpathArg]): FastpathIO[Int] = FF.liftF(GetInteger(a, b))
  def getLong(a: String, b: Array[FastpathArg]): FastpathIO[Long] = FF.liftF(GetLong(a, b))
  def getOID(a: String, b: Array[FastpathArg]): FastpathIO[Long] = FF.liftF(GetOID(a, b))

  // FastpathIO is an Async
  implicit val AsyncFastpathIO: Async[FastpathIO] =
    new Async[FastpathIO] {
      val asyncM = FF.catsFreeMonadForFree[FastpathOp]
      def bracketCase[A, B](acquire: FastpathIO[A])(use: A => FastpathIO[B])(release: (A, ExitCase[Throwable]) => FastpathIO[Unit]): FastpathIO[B] = module.bracketCase(acquire)(use)(release)
      def pure[A](x: A): FastpathIO[A] = asyncM.pure(x)
      def handleErrorWith[A](fa: FastpathIO[A])(f: Throwable => FastpathIO[A]): FastpathIO[A] = module.handleErrorWith(fa, f)
      def raiseError[A](e: Throwable): FastpathIO[A] = module.raiseError(e)
      def async[A](k: (Either[Throwable,A] => Unit) => Unit): FastpathIO[A] = module.async(k)
      def asyncF[A](k: (Either[Throwable,A] => Unit) => FastpathIO[Unit]): FastpathIO[A] = module.asyncF(k)
      def flatMap[A, B](fa: FastpathIO[A])(f: A => FastpathIO[B]): FastpathIO[B] = asyncM.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => FastpathIO[Either[A, B]]): FastpathIO[B] = asyncM.tailRecM(a)(f)
      def suspend[A](thunk: => FastpathIO[A]): FastpathIO[A] = asyncM.flatten(module.delay(thunk))
    }

  // FastpathIO is a ContextShift
  implicit val ContextShiftFastpathIO: ContextShift[FastpathIO] =
    new ContextShift[FastpathIO] {
      def shift: FastpathIO[Unit] = module.shift
      def evalOn[A](ec: ExecutionContext)(fa: FastpathIO[A]) = module.evalOn(ec)(fa)
    }

  // FastpathIO is a MessageLogger
  implicit val MessageLoggerFastpathIO: MessageLogger[FastpathIO] =
    new MessageLogger[FastpathIO] {
      def error(message: => String): FastpathIO[Unit] =  module.error(message)
      def warn(message: => String): FastpathIO[Unit] = module.warn(message)
      def info(message: => String): FastpathIO[Unit] = module.info(message)
      def debug(message: => String): FastpathIO[Unit] =  module.debug(message)
      def trace(message: => String): FastpathIO[Unit] =  module.trace(message)
    }
}

