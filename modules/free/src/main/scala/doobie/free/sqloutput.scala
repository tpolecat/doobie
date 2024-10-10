// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

// format: off

package doobie.free

import cats.{~>, Applicative, Semigroup, Monoid}
import cats.effect.kernel.{ CancelScope, Poll, Sync }
import cats.free.{ Free as FF } // alias because some algebras have an op called Free
import doobie.util.log.LogEvent
import doobie.WeakAsync
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

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
import java.sql.{ Array as SqlArray }

// This file is Auto-generated using FreeGen2.scala
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
        def embed[A](j: SQLOutput, fa: FF[SQLOutputOp, A]): Embedded.SQLOutput[A] = Embedded.SQLOutput(j, fa)
      }

    // Interface for a natural transformation SQLOutputOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (SQLOutputOp ~> F) {
      final def apply[A](fa: SQLOutputOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](f: SQLOutput => A): F[A]
      def embed[A](e: Embedded[A]): F[A]
      def raiseError[A](e: Throwable): F[A]
      def handleErrorWith[A](fa: SQLOutputIO[A])(f: Throwable => SQLOutputIO[A]): F[A]
      def monotonic: F[FiniteDuration]
      def realTime: F[FiniteDuration]
      def delay[A](thunk: => A): F[A]
      def suspend[A](hint: Sync.Type)(thunk: => A): F[A]
      def forceR[A, B](fa: SQLOutputIO[A])(fb: SQLOutputIO[B]): F[B]
      def uncancelable[A](body: Poll[SQLOutputIO] => SQLOutputIO[A]): F[A]
      def poll[A](poll: Any, fa: SQLOutputIO[A]): F[A]
      def canceled: F[Unit]
      def onCancel[A](fa: SQLOutputIO[A], fin: SQLOutputIO[Unit]): F[A]
      def fromFuture[A](fut: SQLOutputIO[Future[A]]): F[A]
      def fromFutureCancelable[A](fut: SQLOutputIO[(Future[A], SQLOutputIO[Unit])]): F[A]
      def cancelable[A](fa: SQLOutputIO[A], fin: SQLOutputIO[Unit]): F[A]
      def performLogging(event: LogEvent): F[Unit]

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
    final case class Raw[A](f: SQLOutput => A) extends SQLOutputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(f)
    }
    final case class Embed[A](e: Embedded[A]) extends SQLOutputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class RaiseError[A](e: Throwable) extends SQLOutputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raiseError(e)
    }
    final case class HandleErrorWith[A](fa: SQLOutputIO[A], f: Throwable => SQLOutputIO[A]) extends SQLOutputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa)(f)
    }
    case object Monotonic extends SQLOutputOp[FiniteDuration] {
      def visit[F[_]](v: Visitor[F]) = v.monotonic
    }
    case object Realtime extends SQLOutputOp[FiniteDuration] {
      def visit[F[_]](v: Visitor[F]) = v.realTime
    }
    case class Suspend[A](hint: Sync.Type, thunk: () => A) extends SQLOutputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.suspend(hint)(thunk())
    }
    case class ForceR[A, B](fa: SQLOutputIO[A], fb: SQLOutputIO[B]) extends SQLOutputOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.forceR(fa)(fb)
    }
    case class Uncancelable[A](body: Poll[SQLOutputIO] => SQLOutputIO[A]) extends SQLOutputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.uncancelable(body)
    }
    case class Poll1[A](poll: Any, fa: SQLOutputIO[A]) extends SQLOutputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.poll(poll, fa)
    }
    case object Canceled extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.canceled
    }
    case class OnCancel[A](fa: SQLOutputIO[A], fin: SQLOutputIO[Unit]) extends SQLOutputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.onCancel(fa, fin)
    }
    case class FromFuture[A](fut: SQLOutputIO[Future[A]]) extends SQLOutputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.fromFuture(fut)
    }
    case class FromFutureCancelable[A](fut: SQLOutputIO[(Future[A], SQLOutputIO[Unit])]) extends SQLOutputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.fromFutureCancelable(fut)
    }
    case class Cancelable[A](fa: SQLOutputIO[A], fin: SQLOutputIO[Unit]) extends SQLOutputOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.cancelable(fa, fin)
    }
    case class PerformLogging(event: LogEvent) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.performLogging(event)
    }

    // SQLOutput-specific operations.
    final case class WriteArray(a: SqlArray) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeArray(a)
    }
    final case class WriteAsciiStream(a: InputStream) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeAsciiStream(a)
    }
    final case class WriteBigDecimal(a: BigDecimal) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeBigDecimal(a)
    }
    final case class WriteBinaryStream(a: InputStream) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeBinaryStream(a)
    }
    final case class WriteBlob(a: Blob) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeBlob(a)
    }
    final case class WriteBoolean(a: Boolean) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeBoolean(a)
    }
    final case class WriteByte(a: Byte) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeByte(a)
    }
    final case class WriteBytes(a: Array[Byte]) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeBytes(a)
    }
    final case class WriteCharacterStream(a: Reader) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeCharacterStream(a)
    }
    final case class WriteClob(a: Clob) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeClob(a)
    }
    final case class WriteDate(a: Date) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeDate(a)
    }
    final case class WriteDouble(a: Double) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeDouble(a)
    }
    final case class WriteFloat(a: Float) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeFloat(a)
    }
    final case class WriteInt(a: Int) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeInt(a)
    }
    final case class WriteLong(a: Long) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeLong(a)
    }
    final case class WriteNClob(a: NClob) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeNClob(a)
    }
    final case class WriteNString(a: String) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeNString(a)
    }
    final case class WriteObject(a: AnyRef, b: SQLType) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeObject(a, b)
    }
    final case class WriteObject1(a: SQLData) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeObject(a)
    }
    final case class WriteRef(a: Ref) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeRef(a)
    }
    final case class WriteRowId(a: RowId) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeRowId(a)
    }
    final case class WriteSQLXML(a: SQLXML) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeSQLXML(a)
    }
    final case class WriteShort(a: Short) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeShort(a)
    }
    final case class WriteString(a: String) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeString(a)
    }
    final case class WriteStruct(a: Struct) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeStruct(a)
    }
    final case class WriteTime(a: Time) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeTime(a)
    }
    final case class WriteTimestamp(a: Timestamp) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeTimestamp(a)
    }
    final case class WriteURL(a: URL) extends SQLOutputOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.writeURL(a)
    }

  }
  import SQLOutputOp.*

  // Smart constructors for operations common to all algebras.
  val unit: SQLOutputIO[Unit] = FF.pure[SQLOutputOp, Unit](())
  def pure[A](a: A): SQLOutputIO[A] = FF.pure[SQLOutputOp, A](a)
  def raw[A](f: SQLOutput => A): SQLOutputIO[A] = FF.liftF(Raw(f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[SQLOutputOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def raiseError[A](err: Throwable): SQLOutputIO[A] = FF.liftF[SQLOutputOp, A](RaiseError(err))
  def handleErrorWith[A](fa: SQLOutputIO[A])(f: Throwable => SQLOutputIO[A]): SQLOutputIO[A] = FF.liftF[SQLOutputOp, A](HandleErrorWith(fa, f))
  val monotonic = FF.liftF[SQLOutputOp, FiniteDuration](Monotonic)
  val realtime = FF.liftF[SQLOutputOp, FiniteDuration](Realtime)
  def delay[A](thunk: => A) = FF.liftF[SQLOutputOp, A](Suspend(Sync.Type.Delay, () => thunk))
  def suspend[A](hint: Sync.Type)(thunk: => A) = FF.liftF[SQLOutputOp, A](Suspend(hint, () => thunk))
  def forceR[A, B](fa: SQLOutputIO[A])(fb: SQLOutputIO[B]) = FF.liftF[SQLOutputOp, B](ForceR(fa, fb))
  def uncancelable[A](body: Poll[SQLOutputIO] => SQLOutputIO[A]) = FF.liftF[SQLOutputOp, A](Uncancelable(body))
  def capturePoll[M[_]](mpoll: Poll[M]) = new Poll[SQLOutputIO] {
    def apply[A](fa: SQLOutputIO[A]) = FF.liftF[SQLOutputOp, A](Poll1(mpoll, fa))
  }
  val canceled = FF.liftF[SQLOutputOp, Unit](Canceled)
  def onCancel[A](fa: SQLOutputIO[A], fin: SQLOutputIO[Unit]) = FF.liftF[SQLOutputOp, A](OnCancel(fa, fin))
  def fromFuture[A](fut: SQLOutputIO[Future[A]]) = FF.liftF[SQLOutputOp, A](FromFuture(fut))
  def fromFutureCancelable[A](fut: SQLOutputIO[(Future[A], SQLOutputIO[Unit])]) = FF.liftF[SQLOutputOp, A](FromFutureCancelable(fut))
  def cancelable[A](fa: SQLOutputIO[A], fin: SQLOutputIO[Unit]) = FF.liftF[SQLOutputOp, A](Cancelable(fa, fin))
  def performLogging(event: LogEvent) = FF.liftF[SQLOutputOp, Unit](PerformLogging(event))

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

  // Typeclass instances for SQLOutputIO
  implicit val WeakAsyncSQLOutputIO: WeakAsync[SQLOutputIO] =
    new WeakAsync[SQLOutputIO] {
      val monad = FF.catsFreeMonadForFree[SQLOutputOp]
      override val applicative: Applicative[SQLOutputIO] = monad
      override val rootCancelScope: CancelScope = CancelScope.Cancelable
      override def pure[A](x: A): SQLOutputIO[A] = monad.pure(x)
      override def flatMap[A, B](fa: SQLOutputIO[A])(f: A => SQLOutputIO[B]): SQLOutputIO[B] = monad.flatMap(fa)(f)
      override def tailRecM[A, B](a: A)(f: A => SQLOutputIO[Either[A, B]]): SQLOutputIO[B] = monad.tailRecM(a)(f)
      override def raiseError[A](e: Throwable): SQLOutputIO[A] = module.raiseError(e)
      override def handleErrorWith[A](fa: SQLOutputIO[A])(f: Throwable => SQLOutputIO[A]): SQLOutputIO[A] = module.handleErrorWith(fa)(f)
      override def monotonic: SQLOutputIO[FiniteDuration] = module.monotonic
      override def realTime: SQLOutputIO[FiniteDuration] = module.realtime
      override def suspend[A](hint: Sync.Type)(thunk: => A): SQLOutputIO[A] = module.suspend(hint)(thunk)
      override def forceR[A, B](fa: SQLOutputIO[A])(fb: SQLOutputIO[B]): SQLOutputIO[B] = module.forceR(fa)(fb)
      override def uncancelable[A](body: Poll[SQLOutputIO] => SQLOutputIO[A]): SQLOutputIO[A] = module.uncancelable(body)
      override def canceled: SQLOutputIO[Unit] = module.canceled
      override def onCancel[A](fa: SQLOutputIO[A], fin: SQLOutputIO[Unit]): SQLOutputIO[A] = module.onCancel(fa, fin)
      override def fromFuture[A](fut: SQLOutputIO[Future[A]]): SQLOutputIO[A] = module.fromFuture(fut)
      override def fromFutureCancelable[A](fut: SQLOutputIO[(Future[A], SQLOutputIO[Unit])]): SQLOutputIO[A] = module.fromFutureCancelable(fut)
      override def cancelable[A](fa: SQLOutputIO[A], fin: SQLOutputIO[Unit]): SQLOutputIO[A] = module.cancelable(fa, fin)
    }
    
  implicit def MonoidSQLOutputIO[A : Monoid]: Monoid[SQLOutputIO[A]] = new Monoid[SQLOutputIO[A]] {
    override def empty: SQLOutputIO[A] = Applicative[SQLOutputIO].pure(Monoid[A].empty)
    override def combine(x: SQLOutputIO[A], y: SQLOutputIO[A]): SQLOutputIO[A] =
      Applicative[SQLOutputIO].product(x, y).map { case (x, y) => Monoid[A].combine(x, y) }
  }
 
  implicit def SemigroupSQLOutputIO[A : Semigroup]: Semigroup[SQLOutputIO[A]] = new Semigroup[SQLOutputIO[A]] {
    override def combine(x: SQLOutputIO[A], y: SQLOutputIO[A]): SQLOutputIO[A] =
      Applicative[SQLOutputIO].product(x, y).map { case (x, y) => Semigroup[A].combine(x, y) }
  }
}

