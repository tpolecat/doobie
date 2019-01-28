// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.free

import cats.~>
import cats.effect.{ Async, ContextShift, ExitCase }
import cats.free.{ Free => FF } // alias because some algebras have an op called Free
import scala.concurrent.ExecutionContext

import java.lang.String
import java.sql.SQLData
import java.sql.SQLInput
import java.sql.SQLOutput

@com.github.ghik.silencer.silent // deprecations, unused variables, etc.
@SuppressWarnings(Array("org.wartremover.warts.Overloading"))
object sqldata { module =>

  // Algebra of operations for SQLData. Each accepts a visitor as an alternatie to pattern-matching.
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
        def embed[A](j: SQLData, fa: FF[SQLDataOp, A]) = Embedded.SQLData(j, fa)
      }

    // Interface for a natural tansformation SQLDataOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (SQLDataOp ~> F) {
      final def apply[A](fa: SQLDataOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](f: Env[SQLData] => A): F[A]
      def embed[A](e: Embedded[A]): F[A]
      def delay[A](a: () => A): F[A]
      def handleErrorWith[A](fa: SQLDataIO[A], f: Throwable => SQLDataIO[A]): F[A]
      def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]
      def asyncF[A](k: (Either[Throwable, A] => Unit) => SQLDataIO[Unit]): F[A]
      def bracketCase[A, B](acquire: SQLDataIO[A])(use: A => SQLDataIO[B])(release: (A, ExitCase[Throwable]) => SQLDataIO[Unit]): F[B]
      def shift: F[Unit]
      def evalOn[A](ec: ExecutionContext)(fa: SQLDataIO[A]): F[A]
      def liftE[G[_]](env: Env[SQLData] => G ~> SQLDataIO): F[G ~> SQLDataIO]

      // SQLData
      def getSQLTypeName: F[String]
      def readSQL(a: SQLInput, b: String): F[Unit]
      def writeSQL(a: SQLOutput): F[Unit]

    }

    // Common operations for all algebras.
    final case class Raw[A](f: Env[SQLData] => A) extends SQLDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(f)
    }
    final case class Embed[A](e: Embedded[A]) extends SQLDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class Delay[A](a: () => A) extends SQLDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.delay(a)
    }
    final case class HandleErrorWith[A](fa: SQLDataIO[A], f: Throwable => SQLDataIO[A]) extends SQLDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa, f)
    }
    final case class Async1[A](k: (Either[Throwable, A] => Unit) => Unit) extends SQLDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.async(k)
    }
    final case class AsyncF[A](k: (Either[Throwable, A] => Unit) => SQLDataIO[Unit]) extends SQLDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.asyncF(k)
    }
    final case class BracketCase[A, B](acquire: SQLDataIO[A], use: A => SQLDataIO[B], release: (A, ExitCase[Throwable]) => SQLDataIO[Unit]) extends SQLDataOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.bracketCase(acquire)(use)(release)
    }
    final case object Shift extends SQLDataOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.shift
    }
    final case class EvalOn[A](ec: ExecutionContext, fa: SQLDataIO[A]) extends SQLDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.evalOn(ec)(fa)
    }
    final case class LiftE[G[_]](env: Env[SQLData] => G ~> SQLDataIO) extends SQLDataOp[G ~> SQLDataIO] {
      def visit[F[_]](v: Visitor[F]) = v.liftE(env)
    }

    // SQLData-specific operations.
    final case object GetSQLTypeName extends SQLDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getSQLTypeName
    }
    final case class  ReadSQL(a: SQLInput, b: String) extends SQLDataOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.readSQL(a, b)
    }
    final case class  WriteSQL(a: SQLOutput) extends SQLDataOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeSQL(a)
    }

  }
  import SQLDataOp._

  // Smart constructors for operations common to all algebras.
  val unit: SQLDataIO[Unit] = FF.pure[SQLDataOp, Unit](())
  def pure[A](a: A): SQLDataIO[A] = FF.pure[SQLDataOp, A](a)
  def raw[A](f: Env[SQLData] => A): SQLDataIO[A] = FF.liftF(Raw(f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[SQLDataOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def delay[A](a: => A): SQLDataIO[A] = FF.liftF(Delay(() => a))
  def handleErrorWith[A](fa: SQLDataIO[A], f: Throwable => SQLDataIO[A]): SQLDataIO[A] = FF.liftF[SQLDataOp, A](HandleErrorWith(fa, f))
  def raiseError[A](err: Throwable): SQLDataIO[A] = delay(throw err)
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): SQLDataIO[A] = FF.liftF[SQLDataOp, A](Async1(k))
  def asyncF[A](k: (Either[Throwable, A] => Unit) => SQLDataIO[Unit]): SQLDataIO[A] = FF.liftF[SQLDataOp, A](AsyncF(k))
  def bracketCase[A, B](acquire: SQLDataIO[A])(use: A => SQLDataIO[B])(release: (A, ExitCase[Throwable]) => SQLDataIO[Unit]): SQLDataIO[B] = FF.liftF[SQLDataOp, B](BracketCase(acquire, use, release))
  val shift: SQLDataIO[Unit] = FF.liftF[SQLDataOp, Unit](Shift)
  def evalOn[A](ec: ExecutionContext)(fa: SQLDataIO[A]) = FF.liftF[SQLDataOp, A](EvalOn(ec, fa))
  def liftE[F[_]](env: Env[SQLData] => F ~> SQLDataIO) = FF.liftF[SQLDataOp, F ~> SQLDataIO](LiftE(env))

  // Smart constructors for SQLData-specific operations.
  val getSQLTypeName: SQLDataIO[String] = FF.liftF(GetSQLTypeName)
  def readSQL(a: SQLInput, b: String): SQLDataIO[Unit] = FF.liftF(ReadSQL(a, b))
  def writeSQL(a: SQLOutput): SQLDataIO[Unit] = FF.liftF(WriteSQL(a))

  // SQLDataIO is an Async
  implicit val AsyncSQLDataIO: Async[SQLDataIO] =
    new Async[SQLDataIO] {
      val asyncM = FF.catsFreeMonadForFree[SQLDataOp]
      def bracketCase[A, B](acquire: SQLDataIO[A])(use: A => SQLDataIO[B])(release: (A, ExitCase[Throwable]) => SQLDataIO[Unit]): SQLDataIO[B] = module.bracketCase(acquire)(use)(release)
      def pure[A](x: A): SQLDataIO[A] = asyncM.pure(x)
      def handleErrorWith[A](fa: SQLDataIO[A])(f: Throwable => SQLDataIO[A]): SQLDataIO[A] = module.handleErrorWith(fa, f)
      def raiseError[A](e: Throwable): SQLDataIO[A] = module.raiseError(e)
      def async[A](k: (Either[Throwable,A] => Unit) => Unit): SQLDataIO[A] = module.async(k)
      def asyncF[A](k: (Either[Throwable,A] => Unit) => SQLDataIO[Unit]): SQLDataIO[A] = module.asyncF(k)
      def flatMap[A, B](fa: SQLDataIO[A])(f: A => SQLDataIO[B]): SQLDataIO[B] = asyncM.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => SQLDataIO[Either[A, B]]): SQLDataIO[B] = asyncM.tailRecM(a)(f)
      def suspend[A](thunk: => SQLDataIO[A]): SQLDataIO[A] = asyncM.flatten(module.delay(thunk))
    }

  // SQLDataIO is a ContextShift
  implicit val ContextShiftSQLDataIO: ContextShift[SQLDataIO] =
    new ContextShift[SQLDataIO] {
      def shift: SQLDataIO[Unit] = module.shift
      def evalOn[A](ec: ExecutionContext)(fa: SQLDataIO[A]) = module.evalOn(ec)(fa)
    }
}

