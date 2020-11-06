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
import java.io.Reader
import java.lang.Class
import java.lang.String
import java.math.BigDecimal
import java.net.URL
import java.sql.Blob
import java.sql.Clob
import java.sql.Date
import java.sql.NClob
import java.sql.Ref
import java.sql.RowId
import java.sql.SQLInput
import java.sql.SQLXML
import java.sql.Time
import java.sql.Timestamp
import java.sql.{ Array => SqlArray }

@silent("deprecated")
object sqlinput { module =>

  // Algebra of operations for SQLInput. Each accepts a visitor as an alternative to pattern-matching.
  sealed trait SQLInputOp[A] {
    def visit[F[_]](v: SQLInputOp.Visitor[F]): F[A]
  }

  // Free monad over SQLInputOp.
  type SQLInputIO[A] = FF[SQLInputOp, A]

  // Module of instances and constructors of SQLInputOp.
  object SQLInputOp {

    // Given a SQLInput we can embed a SQLInputIO program in any algebra that understands embedding.
    implicit val SQLInputOpEmbeddable: Embeddable[SQLInputOp, SQLInput] =
      new Embeddable[SQLInputOp, SQLInput] {
        def embed[A](j: SQLInput, fa: FF[SQLInputOp, A]) = Embedded.SQLInput(j, fa)
      }

    // Interface for a natural transformation SQLInputOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (SQLInputOp ~> F) {
      final def apply[A](fa: SQLInputOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](message: => String, f: SQLInput => A): F[A]
      def embed[A](e: Embedded[A]): F[A]

      // Async
      def delay[A](a: () => A): F[A]
      def handleErrorWith[A](fa: SQLInputIO[A], f: Throwable => SQLInputIO[A]): F[A]
      def raiseError[A](e: Throwable): F[A]
      def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]
      def asyncF[A](k: (Either[Throwable, A] => Unit) => SQLInputIO[Unit]): F[A]
      def bracketCase[A, B](acquire: SQLInputIO[A])(use: A => SQLInputIO[B])(release: (A, ExitCase[Throwable]) => SQLInputIO[Unit]): F[B]
      def shift: F[Unit]
      def evalOn[A](ec: ExecutionContext)(fa: SQLInputIO[A]): F[A]

      // Logger
      def error(message: => String): F[Unit]
      def warn(message: => String): F[Unit]
      def info(message: => String): F[Unit]
      def debug(message: => String): F[Unit]
      def trace(message: => String): F[Unit]

      // SQLInput
      def readArray: F[SqlArray]
      def readAsciiStream: F[InputStream]
      def readBigDecimal: F[BigDecimal]
      def readBinaryStream: F[InputStream]
      def readBlob: F[Blob]
      def readBoolean: F[Boolean]
      def readByte: F[Byte]
      def readBytes: F[Array[Byte]]
      def readCharacterStream: F[Reader]
      def readClob: F[Clob]
      def readDate: F[Date]
      def readDouble: F[Double]
      def readFloat: F[Float]
      def readInt: F[Int]
      def readLong: F[Long]
      def readNClob: F[NClob]
      def readNString: F[String]
      def readObject: F[AnyRef]
      def readObject[T](a: Class[T]): F[T]
      def readRef: F[Ref]
      def readRowId: F[RowId]
      def readSQLXML: F[SQLXML]
      def readShort: F[Short]
      def readString: F[String]
      def readTime: F[Time]
      def readTimestamp: F[Timestamp]
      def readURL: F[URL]
      def wasNull: F[Boolean]

    }

    // Common operations for all algebras.
    final case class Raw[A](message: () => String, f: SQLInput => A) extends SQLInputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(message(), f)
    }
    final case class Embed[A](e: Embedded[A]) extends SQLInputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class Delay[A](a: () => A) extends SQLInputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.delay(a)
    }
    final case class HandleErrorWith[A](fa: SQLInputIO[A], f: Throwable => SQLInputIO[A]) extends SQLInputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa, f)
    }
    final case class RaiseError[A](e: Throwable) extends SQLInputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raiseError(e)
    }
    final case class Async1[A](k: (Either[Throwable, A] => Unit) => Unit) extends SQLInputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.async(k)
    }
    final case class AsyncF[A](k: (Either[Throwable, A] => Unit) => SQLInputIO[Unit]) extends SQLInputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.asyncF(k)
    }
    final case class BracketCase[A, B](acquire: SQLInputIO[A], use: A => SQLInputIO[B], release: (A, ExitCase[Throwable]) => SQLInputIO[Unit]) extends SQLInputOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.bracketCase(acquire)(use)(release)
    }
    final case object Shift extends SQLInputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.shift
    }
    final case class EvalOn[A](ec: ExecutionContext, fa: SQLInputIO[A]) extends SQLInputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.evalOn(ec)(fa)
    }
    final case class LogError(message: () => String) extends SQLInputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.error(message()): F[Unit]
    }
    final case class LogWarn(message: () => String) extends SQLInputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.warn(message()): F[Unit]
    }
    final case class LogInfo(message: () => String) extends SQLInputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.info(message()): F[Unit]
    }
    final case class LogDebug(message: () => String) extends SQLInputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.debug(message()): F[Unit]
    }
    final case class LogTrace(message: () => String) extends SQLInputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.trace(message()): F[Unit]
    }

    // SQLInput-specific operations.
    final case object ReadArray extends SQLInputOp[SqlArray] {
      def visit[F[_]](v: Visitor[F]) = v.readArray
    }
    final case object ReadAsciiStream extends SQLInputOp[InputStream] {
      def visit[F[_]](v: Visitor[F]) = v.readAsciiStream
    }
    final case object ReadBigDecimal extends SQLInputOp[BigDecimal] {
      def visit[F[_]](v: Visitor[F]) = v.readBigDecimal
    }
    final case object ReadBinaryStream extends SQLInputOp[InputStream] {
      def visit[F[_]](v: Visitor[F]) = v.readBinaryStream
    }
    final case object ReadBlob extends SQLInputOp[Blob] {
      def visit[F[_]](v: Visitor[F]) = v.readBlob
    }
    final case object ReadBoolean extends SQLInputOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.readBoolean
    }
    final case object ReadByte extends SQLInputOp[Byte] {
      def visit[F[_]](v: Visitor[F]) = v.readByte
    }
    final case object ReadBytes extends SQLInputOp[Array[Byte]] {
      def visit[F[_]](v: Visitor[F]) = v.readBytes
    }
    final case object ReadCharacterStream extends SQLInputOp[Reader] {
      def visit[F[_]](v: Visitor[F]) = v.readCharacterStream
    }
    final case object ReadClob extends SQLInputOp[Clob] {
      def visit[F[_]](v: Visitor[F]) = v.readClob
    }
    final case object ReadDate extends SQLInputOp[Date] {
      def visit[F[_]](v: Visitor[F]) = v.readDate
    }
    final case object ReadDouble extends SQLInputOp[Double] {
      def visit[F[_]](v: Visitor[F]) = v.readDouble
    }
    final case object ReadFloat extends SQLInputOp[Float] {
      def visit[F[_]](v: Visitor[F]) = v.readFloat
    }
    final case object ReadInt extends SQLInputOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.readInt
    }
    final case object ReadLong extends SQLInputOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.readLong
    }
    final case object ReadNClob extends SQLInputOp[NClob] {
      def visit[F[_]](v: Visitor[F]) = v.readNClob
    }
    final case object ReadNString extends SQLInputOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.readNString
    }
    final case object ReadObject extends SQLInputOp[AnyRef] {
      def visit[F[_]](v: Visitor[F]) = v.readObject
    }
    final case class  ReadObject1[T](a: Class[T]) extends SQLInputOp[T] {
      def visit[F[_]](v: Visitor[F]) = v.readObject(a)
    }
    final case object ReadRef extends SQLInputOp[Ref] {
      def visit[F[_]](v: Visitor[F]) = v.readRef
    }
    final case object ReadRowId extends SQLInputOp[RowId] {
      def visit[F[_]](v: Visitor[F]) = v.readRowId
    }
    final case object ReadSQLXML extends SQLInputOp[SQLXML] {
      def visit[F[_]](v: Visitor[F]) = v.readSQLXML
    }
    final case object ReadShort extends SQLInputOp[Short] {
      def visit[F[_]](v: Visitor[F]) = v.readShort
    }
    final case object ReadString extends SQLInputOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.readString
    }
    final case object ReadTime extends SQLInputOp[Time] {
      def visit[F[_]](v: Visitor[F]) = v.readTime
    }
    final case object ReadTimestamp extends SQLInputOp[Timestamp] {
      def visit[F[_]](v: Visitor[F]) = v.readTimestamp
    }
    final case object ReadURL extends SQLInputOp[URL] {
      def visit[F[_]](v: Visitor[F]) = v.readURL
    }
    final case object WasNull extends SQLInputOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.wasNull
    }

  }
  import SQLInputOp._

  // Smart constructors for operations common to all algebras.
  val unit: SQLInputIO[Unit] = FF.pure[SQLInputOp, Unit](())
  def pure[A](a: A): SQLInputIO[A] = FF.pure[SQLInputOp, A](a)
  def raw[A](message: => String)(f: SQLInput => A): SQLInputIO[A] = FF.liftF(Raw(() => message, f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[SQLInputOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def delay[A](a: => A): SQLInputIO[A] = FF.liftF(Delay(() => a))
  def handleErrorWith[A](fa: SQLInputIO[A], f: Throwable => SQLInputIO[A]): SQLInputIO[A] = FF.liftF[SQLInputOp, A](HandleErrorWith(fa, f))
  def raiseError[A](err: Throwable): SQLInputIO[A] = FF.liftF[SQLInputOp, A](RaiseError(err))
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): SQLInputIO[A] = FF.liftF[SQLInputOp, A](Async1(k))
  def asyncF[A](k: (Either[Throwable, A] => Unit) => SQLInputIO[Unit]): SQLInputIO[A] = FF.liftF[SQLInputOp, A](AsyncF(k))
  def bracketCase[A, B](acquire: SQLInputIO[A])(use: A => SQLInputIO[B])(release: (A, ExitCase[Throwable]) => SQLInputIO[Unit]): SQLInputIO[B] = FF.liftF[SQLInputOp, B](BracketCase(acquire, use, release))
  val shift: SQLInputIO[Unit] = FF.liftF[SQLInputOp, Unit](Shift)
  def evalOn[A](ec: ExecutionContext)(fa: SQLInputIO[A]) = FF.liftF[SQLInputOp, A](EvalOn(ec, fa))

  // Logger
  def error(message: => String): SQLInputIO[Unit] = FF.liftF[SQLInputOp, Unit](LogError(() => message))
  def warn(message: => String): SQLInputIO[Unit] = FF.liftF[SQLInputOp, Unit](LogWarn(() => message))
  def info(message: => String): SQLInputIO[Unit] = FF.liftF[SQLInputOp, Unit](LogInfo(() => message))
  def debug(message: => String): SQLInputIO[Unit] = FF.liftF[SQLInputOp, Unit](LogDebug(() => message))
  def trace(message: => String): SQLInputIO[Unit] = FF.liftF[SQLInputOp, Unit](LogTrace(() => message))

  // Smart constructors for SQLInput-specific operations.
  val readArray: SQLInputIO[SqlArray] = FF.liftF(ReadArray)
  val readAsciiStream: SQLInputIO[InputStream] = FF.liftF(ReadAsciiStream)
  val readBigDecimal: SQLInputIO[BigDecimal] = FF.liftF(ReadBigDecimal)
  val readBinaryStream: SQLInputIO[InputStream] = FF.liftF(ReadBinaryStream)
  val readBlob: SQLInputIO[Blob] = FF.liftF(ReadBlob)
  val readBoolean: SQLInputIO[Boolean] = FF.liftF(ReadBoolean)
  val readByte: SQLInputIO[Byte] = FF.liftF(ReadByte)
  val readBytes: SQLInputIO[Array[Byte]] = FF.liftF(ReadBytes)
  val readCharacterStream: SQLInputIO[Reader] = FF.liftF(ReadCharacterStream)
  val readClob: SQLInputIO[Clob] = FF.liftF(ReadClob)
  val readDate: SQLInputIO[Date] = FF.liftF(ReadDate)
  val readDouble: SQLInputIO[Double] = FF.liftF(ReadDouble)
  val readFloat: SQLInputIO[Float] = FF.liftF(ReadFloat)
  val readInt: SQLInputIO[Int] = FF.liftF(ReadInt)
  val readLong: SQLInputIO[Long] = FF.liftF(ReadLong)
  val readNClob: SQLInputIO[NClob] = FF.liftF(ReadNClob)
  val readNString: SQLInputIO[String] = FF.liftF(ReadNString)
  val readObject: SQLInputIO[AnyRef] = FF.liftF(ReadObject)
  def readObject[T](a: Class[T]): SQLInputIO[T] = FF.liftF(ReadObject1(a))
  val readRef: SQLInputIO[Ref] = FF.liftF(ReadRef)
  val readRowId: SQLInputIO[RowId] = FF.liftF(ReadRowId)
  val readSQLXML: SQLInputIO[SQLXML] = FF.liftF(ReadSQLXML)
  val readShort: SQLInputIO[Short] = FF.liftF(ReadShort)
  val readString: SQLInputIO[String] = FF.liftF(ReadString)
  val readTime: SQLInputIO[Time] = FF.liftF(ReadTime)
  val readTimestamp: SQLInputIO[Timestamp] = FF.liftF(ReadTimestamp)
  val readURL: SQLInputIO[URL] = FF.liftF(ReadURL)
  val wasNull: SQLInputIO[Boolean] = FF.liftF(WasNull)

  // SQLInputIO is an Async
  implicit val AsyncSQLInputIO: Async[SQLInputIO] =
    new Async[SQLInputIO] {
      val asyncM = FF.catsFreeMonadForFree[SQLInputOp]
      def bracketCase[A, B](acquire: SQLInputIO[A])(use: A => SQLInputIO[B])(release: (A, ExitCase[Throwable]) => SQLInputIO[Unit]): SQLInputIO[B] = module.bracketCase(acquire)(use)(release)
      def pure[A](x: A): SQLInputIO[A] = asyncM.pure(x)
      def handleErrorWith[A](fa: SQLInputIO[A])(f: Throwable => SQLInputIO[A]): SQLInputIO[A] = module.handleErrorWith(fa, f)
      def raiseError[A](e: Throwable): SQLInputIO[A] = module.raiseError(e)
      def async[A](k: (Either[Throwable,A] => Unit) => Unit): SQLInputIO[A] = module.async(k)
      def asyncF[A](k: (Either[Throwable,A] => Unit) => SQLInputIO[Unit]): SQLInputIO[A] = module.asyncF(k)
      def flatMap[A, B](fa: SQLInputIO[A])(f: A => SQLInputIO[B]): SQLInputIO[B] = asyncM.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => SQLInputIO[Either[A, B]]): SQLInputIO[B] = asyncM.tailRecM(a)(f)
      def suspend[A](thunk: => SQLInputIO[A]): SQLInputIO[A] = asyncM.flatten(module.delay(thunk))
    }

  // SQLInputIO is a ContextShift
  implicit val ContextShiftSQLInputIO: ContextShift[SQLInputIO] =
    new ContextShift[SQLInputIO] {
      def shift: SQLInputIO[Unit] = module.shift
      def evalOn[A](ec: ExecutionContext)(fa: SQLInputIO[A]) = module.evalOn(ec)(fa)
    }

  // SQLInputIO is a MessageLogger
  implicit val MessageLoggerSQLInputIO: MessageLogger[SQLInputIO] =
    new MessageLogger[SQLInputIO] {
      def error(message: => String): SQLInputIO[Unit] =  module.error(message)
      def warn(message: => String): SQLInputIO[Unit] = module.warn(message)
      def info(message: => String): SQLInputIO[Unit] = module.info(message)
      def debug(message: => String): SQLInputIO[Unit] =  module.debug(message)
      def trace(message: => String): SQLInputIO[Unit] =  module.trace(message)
    }
}

