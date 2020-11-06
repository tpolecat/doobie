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
import java.lang.String
import java.math.BigDecimal
import java.net.URL
import java.sql.Blob
import java.sql.Clob
import java.sql.Date
import java.sql.NClob
import java.sql.Ref
import java.sql.RowId
import java.sql.SQLData
import java.sql.SQLOutput
import java.sql.SQLType
import java.sql.SQLXML
import java.sql.Struct
import java.sql.Time
import java.sql.Timestamp
import java.sql.{ Array => SqlArray }

@silent("deprecated")
object sqloutput { module =>

  // Algebra of operations for SQLOutput. Each accepts a visitor as an alternative to pattern-matching.
  sealed trait SQLOutputOp[A] {
    def visit[F[_]](v: SQLOutputOp.Visitor[F]): F[A]
  }

  // Free monad over SQLOutputOp.
  type SQLOutputIO[A] = FF[SQLOutputOp, A]

  // Module of instances and constructors of SQLOutputOp.
  object SQLOutputOp {

    // Given a SQLOutput we can embed a SQLOutputIO program in any algebra that understands embedding.
    implicit val SQLOutputOpEmbeddable: Embeddable[SQLOutputOp, SQLOutput] =
      new Embeddable[SQLOutputOp, SQLOutput] {
        def embed[A](j: SQLOutput, fa: FF[SQLOutputOp, A]) = Embedded.SQLOutput(j, fa)
      }

    // Interface for a natural transformation SQLOutputOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (SQLOutputOp ~> F) {
      final def apply[A](fa: SQLOutputOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](message: => String, f: SQLOutput => A): F[A]
      def embed[A](e: Embedded[A]): F[A]

      // Async
      def delay[A](a: () => A): F[A]
      def handleErrorWith[A](fa: SQLOutputIO[A], f: Throwable => SQLOutputIO[A]): F[A]
      def raiseError[A](e: Throwable): F[A]
      def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]
      def asyncF[A](k: (Either[Throwable, A] => Unit) => SQLOutputIO[Unit]): F[A]
      def bracketCase[A, B](acquire: SQLOutputIO[A])(use: A => SQLOutputIO[B])(release: (A, ExitCase[Throwable]) => SQLOutputIO[Unit]): F[B]
      def shift: F[Unit]
      def evalOn[A](ec: ExecutionContext)(fa: SQLOutputIO[A]): F[A]

      // Logger
      def error(message: => String): F[Unit]
      def warn(message: => String): F[Unit]
      def info(message: => String): F[Unit]
      def debug(message: => String): F[Unit]
      def trace(message: => String): F[Unit]

      // SQLOutput
      def writeArray(a: SqlArray): F[Unit]
      def writeAsciiStream(a: InputStream): F[Unit]
      def writeBigDecimal(a: BigDecimal): F[Unit]
      def writeBinaryStream(a: InputStream): F[Unit]
      def writeBlob(a: Blob): F[Unit]
      def writeBoolean(a: Boolean): F[Unit]
      def writeByte(a: Byte): F[Unit]
      def writeBytes(a: Array[Byte]): F[Unit]
      def writeCharacterStream(a: Reader): F[Unit]
      def writeClob(a: Clob): F[Unit]
      def writeDate(a: Date): F[Unit]
      def writeDouble(a: Double): F[Unit]
      def writeFloat(a: Float): F[Unit]
      def writeInt(a: Int): F[Unit]
      def writeLong(a: Long): F[Unit]
      def writeNClob(a: NClob): F[Unit]
      def writeNString(a: String): F[Unit]
      def writeObject(a: AnyRef, b: SQLType): F[Unit]
      def writeObject(a: SQLData): F[Unit]
      def writeRef(a: Ref): F[Unit]
      def writeRowId(a: RowId): F[Unit]
      def writeSQLXML(a: SQLXML): F[Unit]
      def writeShort(a: Short): F[Unit]
      def writeString(a: String): F[Unit]
      def writeStruct(a: Struct): F[Unit]
      def writeTime(a: Time): F[Unit]
      def writeTimestamp(a: Timestamp): F[Unit]
      def writeURL(a: URL): F[Unit]

    }

    // Common operations for all algebras.
    final case class Raw[A](message: () => String, f: SQLOutput => A) extends SQLOutputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(message(), f)
    }
    final case class Embed[A](e: Embedded[A]) extends SQLOutputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class Delay[A](a: () => A) extends SQLOutputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.delay(a)
    }
    final case class HandleErrorWith[A](fa: SQLOutputIO[A], f: Throwable => SQLOutputIO[A]) extends SQLOutputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa, f)
    }
    final case class RaiseError[A](e: Throwable) extends SQLOutputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raiseError(e)
    }
    final case class Async1[A](k: (Either[Throwable, A] => Unit) => Unit) extends SQLOutputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.async(k)
    }
    final case class AsyncF[A](k: (Either[Throwable, A] => Unit) => SQLOutputIO[Unit]) extends SQLOutputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.asyncF(k)
    }
    final case class BracketCase[A, B](acquire: SQLOutputIO[A], use: A => SQLOutputIO[B], release: (A, ExitCase[Throwable]) => SQLOutputIO[Unit]) extends SQLOutputOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.bracketCase(acquire)(use)(release)
    }
    final case object Shift extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.shift
    }
    final case class EvalOn[A](ec: ExecutionContext, fa: SQLOutputIO[A]) extends SQLOutputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.evalOn(ec)(fa)
    }
    final case class LogError(message: () => String) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.error(message()): F[Unit]
    }
    final case class LogWarn(message: () => String) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.warn(message()): F[Unit]
    }
    final case class LogInfo(message: () => String) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.info(message()): F[Unit]
    }
    final case class LogDebug(message: () => String) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.debug(message()): F[Unit]
    }
    final case class LogTrace(message: () => String) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.trace(message()): F[Unit]
    }

    // SQLOutput-specific operations.
    final case class  WriteArray(a: SqlArray) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeArray(a)
    }
    final case class  WriteAsciiStream(a: InputStream) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeAsciiStream(a)
    }
    final case class  WriteBigDecimal(a: BigDecimal) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeBigDecimal(a)
    }
    final case class  WriteBinaryStream(a: InputStream) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeBinaryStream(a)
    }
    final case class  WriteBlob(a: Blob) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeBlob(a)
    }
    final case class  WriteBoolean(a: Boolean) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeBoolean(a)
    }
    final case class  WriteByte(a: Byte) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeByte(a)
    }
    final case class  WriteBytes(a: Array[Byte]) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeBytes(a)
    }
    final case class  WriteCharacterStream(a: Reader) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeCharacterStream(a)
    }
    final case class  WriteClob(a: Clob) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeClob(a)
    }
    final case class  WriteDate(a: Date) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeDate(a)
    }
    final case class  WriteDouble(a: Double) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeDouble(a)
    }
    final case class  WriteFloat(a: Float) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeFloat(a)
    }
    final case class  WriteInt(a: Int) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeInt(a)
    }
    final case class  WriteLong(a: Long) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeLong(a)
    }
    final case class  WriteNClob(a: NClob) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeNClob(a)
    }
    final case class  WriteNString(a: String) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeNString(a)
    }
    final case class  WriteObject(a: AnyRef, b: SQLType) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeObject(a, b)
    }
    final case class  WriteObject1(a: SQLData) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeObject(a)
    }
    final case class  WriteRef(a: Ref) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeRef(a)
    }
    final case class  WriteRowId(a: RowId) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeRowId(a)
    }
    final case class  WriteSQLXML(a: SQLXML) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeSQLXML(a)
    }
    final case class  WriteShort(a: Short) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeShort(a)
    }
    final case class  WriteString(a: String) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeString(a)
    }
    final case class  WriteStruct(a: Struct) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeStruct(a)
    }
    final case class  WriteTime(a: Time) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeTime(a)
    }
    final case class  WriteTimestamp(a: Timestamp) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeTimestamp(a)
    }
    final case class  WriteURL(a: URL) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeURL(a)
    }

  }
  import SQLOutputOp._

  // Smart constructors for operations common to all algebras.
  val unit: SQLOutputIO[Unit] = FF.pure[SQLOutputOp, Unit](())
  def pure[A](a: A): SQLOutputIO[A] = FF.pure[SQLOutputOp, A](a)
  def raw[A](message: => String)(f: SQLOutput => A): SQLOutputIO[A] = FF.liftF(Raw(() => message, f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[SQLOutputOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def delay[A](a: => A): SQLOutputIO[A] = FF.liftF(Delay(() => a))
  def handleErrorWith[A](fa: SQLOutputIO[A], f: Throwable => SQLOutputIO[A]): SQLOutputIO[A] = FF.liftF[SQLOutputOp, A](HandleErrorWith(fa, f))
  def raiseError[A](err: Throwable): SQLOutputIO[A] = FF.liftF[SQLOutputOp, A](RaiseError(err))
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): SQLOutputIO[A] = FF.liftF[SQLOutputOp, A](Async1(k))
  def asyncF[A](k: (Either[Throwable, A] => Unit) => SQLOutputIO[Unit]): SQLOutputIO[A] = FF.liftF[SQLOutputOp, A](AsyncF(k))
  def bracketCase[A, B](acquire: SQLOutputIO[A])(use: A => SQLOutputIO[B])(release: (A, ExitCase[Throwable]) => SQLOutputIO[Unit]): SQLOutputIO[B] = FF.liftF[SQLOutputOp, B](BracketCase(acquire, use, release))
  val shift: SQLOutputIO[Unit] = FF.liftF[SQLOutputOp, Unit](Shift)
  def evalOn[A](ec: ExecutionContext)(fa: SQLOutputIO[A]) = FF.liftF[SQLOutputOp, A](EvalOn(ec, fa))

  // Logger
  def error(message: => String): SQLOutputIO[Unit] = FF.liftF[SQLOutputOp, Unit](LogError(() => message))
  def warn(message: => String): SQLOutputIO[Unit] = FF.liftF[SQLOutputOp, Unit](LogWarn(() => message))
  def info(message: => String): SQLOutputIO[Unit] = FF.liftF[SQLOutputOp, Unit](LogInfo(() => message))
  def debug(message: => String): SQLOutputIO[Unit] = FF.liftF[SQLOutputOp, Unit](LogDebug(() => message))
  def trace(message: => String): SQLOutputIO[Unit] = FF.liftF[SQLOutputOp, Unit](LogTrace(() => message))

  // Smart constructors for SQLOutput-specific operations.
  def writeArray(a: SqlArray): SQLOutputIO[Unit] = FF.liftF(WriteArray(a))
  def writeAsciiStream(a: InputStream): SQLOutputIO[Unit] = FF.liftF(WriteAsciiStream(a))
  def writeBigDecimal(a: BigDecimal): SQLOutputIO[Unit] = FF.liftF(WriteBigDecimal(a))
  def writeBinaryStream(a: InputStream): SQLOutputIO[Unit] = FF.liftF(WriteBinaryStream(a))
  def writeBlob(a: Blob): SQLOutputIO[Unit] = FF.liftF(WriteBlob(a))
  def writeBoolean(a: Boolean): SQLOutputIO[Unit] = FF.liftF(WriteBoolean(a))
  def writeByte(a: Byte): SQLOutputIO[Unit] = FF.liftF(WriteByte(a))
  def writeBytes(a: Array[Byte]): SQLOutputIO[Unit] = FF.liftF(WriteBytes(a))
  def writeCharacterStream(a: Reader): SQLOutputIO[Unit] = FF.liftF(WriteCharacterStream(a))
  def writeClob(a: Clob): SQLOutputIO[Unit] = FF.liftF(WriteClob(a))
  def writeDate(a: Date): SQLOutputIO[Unit] = FF.liftF(WriteDate(a))
  def writeDouble(a: Double): SQLOutputIO[Unit] = FF.liftF(WriteDouble(a))
  def writeFloat(a: Float): SQLOutputIO[Unit] = FF.liftF(WriteFloat(a))
  def writeInt(a: Int): SQLOutputIO[Unit] = FF.liftF(WriteInt(a))
  def writeLong(a: Long): SQLOutputIO[Unit] = FF.liftF(WriteLong(a))
  def writeNClob(a: NClob): SQLOutputIO[Unit] = FF.liftF(WriteNClob(a))
  def writeNString(a: String): SQLOutputIO[Unit] = FF.liftF(WriteNString(a))
  def writeObject(a: AnyRef, b: SQLType): SQLOutputIO[Unit] = FF.liftF(WriteObject(a, b))
  def writeObject(a: SQLData): SQLOutputIO[Unit] = FF.liftF(WriteObject1(a))
  def writeRef(a: Ref): SQLOutputIO[Unit] = FF.liftF(WriteRef(a))
  def writeRowId(a: RowId): SQLOutputIO[Unit] = FF.liftF(WriteRowId(a))
  def writeSQLXML(a: SQLXML): SQLOutputIO[Unit] = FF.liftF(WriteSQLXML(a))
  def writeShort(a: Short): SQLOutputIO[Unit] = FF.liftF(WriteShort(a))
  def writeString(a: String): SQLOutputIO[Unit] = FF.liftF(WriteString(a))
  def writeStruct(a: Struct): SQLOutputIO[Unit] = FF.liftF(WriteStruct(a))
  def writeTime(a: Time): SQLOutputIO[Unit] = FF.liftF(WriteTime(a))
  def writeTimestamp(a: Timestamp): SQLOutputIO[Unit] = FF.liftF(WriteTimestamp(a))
  def writeURL(a: URL): SQLOutputIO[Unit] = FF.liftF(WriteURL(a))

  // SQLOutputIO is an Async
  implicit val AsyncSQLOutputIO: Async[SQLOutputIO] =
    new Async[SQLOutputIO] {
      val asyncM = FF.catsFreeMonadForFree[SQLOutputOp]
      def bracketCase[A, B](acquire: SQLOutputIO[A])(use: A => SQLOutputIO[B])(release: (A, ExitCase[Throwable]) => SQLOutputIO[Unit]): SQLOutputIO[B] = module.bracketCase(acquire)(use)(release)
      def pure[A](x: A): SQLOutputIO[A] = asyncM.pure(x)
      def handleErrorWith[A](fa: SQLOutputIO[A])(f: Throwable => SQLOutputIO[A]): SQLOutputIO[A] = module.handleErrorWith(fa, f)
      def raiseError[A](e: Throwable): SQLOutputIO[A] = module.raiseError(e)
      def async[A](k: (Either[Throwable,A] => Unit) => Unit): SQLOutputIO[A] = module.async(k)
      def asyncF[A](k: (Either[Throwable,A] => Unit) => SQLOutputIO[Unit]): SQLOutputIO[A] = module.asyncF(k)
      def flatMap[A, B](fa: SQLOutputIO[A])(f: A => SQLOutputIO[B]): SQLOutputIO[B] = asyncM.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => SQLOutputIO[Either[A, B]]): SQLOutputIO[B] = asyncM.tailRecM(a)(f)
      def suspend[A](thunk: => SQLOutputIO[A]): SQLOutputIO[A] = asyncM.flatten(module.delay(thunk))
    }

  // SQLOutputIO is a ContextShift
  implicit val ContextShiftSQLOutputIO: ContextShift[SQLOutputIO] =
    new ContextShift[SQLOutputIO] {
      def shift: SQLOutputIO[Unit] = module.shift
      def evalOn[A](ec: ExecutionContext)(fa: SQLOutputIO[A]) = module.evalOn(ec)(fa)
    }

  // SQLOutputIO is a MessageLogger
  implicit val MessageLoggerSQLOutputIO: MessageLogger[SQLOutputIO] =
    new MessageLogger[SQLOutputIO] {
      def error(message: => String): SQLOutputIO[Unit] =  module.error(message)
      def warn(message: => String): SQLOutputIO[Unit] = module.warn(message)
      def info(message: => String): SQLOutputIO[Unit] = module.info(message)
      def debug(message: => String): SQLOutputIO[Unit] =  module.debug(message)
      def trace(message: => String): SQLOutputIO[Unit] =  module.trace(message)
    }
}

