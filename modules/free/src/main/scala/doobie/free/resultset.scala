// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.free

import cats.~>
import cats.effect.{ Async, ContextShift, ExitCase }
import cats.free.{ Free => FF } // alias because some algebras have an op called Free
import scala.concurrent.ExecutionContext

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
import java.sql.ResultSet
import java.sql.ResultSetMetaData
import java.sql.RowId
import java.sql.SQLType
import java.sql.SQLWarning
import java.sql.SQLXML
import java.sql.Statement
import java.sql.Time
import java.sql.Timestamp
import java.sql.{ Array => SqlArray }
import java.util.Calendar
import java.util.Map

@com.github.ghik.silencer.silent // deprecations, unused variables, etc.
@SuppressWarnings(Array("org.wartremover.warts.Overloading"))
object resultset { module =>

  // Algebra of operations for ResultSet. Each accepts a visitor as an alternatie to pattern-matching.
  sealed trait ResultSetOp[A] {
    def visit[F[_]](v: ResultSetOp.Visitor[F]): F[A]
  }

  // Free monad over ResultSetOp.
  type ResultSetIO[A] = FF[ResultSetOp, A]

  // Module of instances and constructors of ResultSetOp.
  object ResultSetOp {

    // Given a ResultSet we can embed a ResultSetIO program in any algebra that understands embedding.
    implicit val ResultSetOpEmbeddable: Embeddable[ResultSetOp, ResultSet] =
      new Embeddable[ResultSetOp, ResultSet] {
        def embed[A](j: ResultSet, fa: FF[ResultSetOp, A]) = Embedded.ResultSet(j, fa)
      }

    // Interface for a natural tansformation ResultSetOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (ResultSetOp ~> F) {
      final def apply[A](fa: ResultSetOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](f: Env[ResultSet] => A): F[A]
      def embed[A](e: Embedded[A]): F[A]
      def delay[A](a: () => A): F[A]
      def handleErrorWith[A](fa: ResultSetIO[A], f: Throwable => ResultSetIO[A]): F[A]
      def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]
      def asyncF[A](k: (Either[Throwable, A] => Unit) => ResultSetIO[Unit]): F[A]
      def bracketCase[A, B](acquire: ResultSetIO[A])(use: A => ResultSetIO[B])(release: (A, ExitCase[Throwable]) => ResultSetIO[Unit]): F[B]
      def shift: F[Unit]
      def evalOn[A](ec: ExecutionContext)(fa: ResultSetIO[A]): F[A]
      def liftE[G[_]](env: Env[ResultSet] => G ~> ResultSetIO): F[G ~> ResultSetIO]

      // ResultSet
      def absolute(a: Int): F[Boolean]
      def afterLast: F[Unit]
      def beforeFirst: F[Unit]
      def cancelRowUpdates: F[Unit]
      def clearWarnings: F[Unit]
      def close: F[Unit]
      def deleteRow: F[Unit]
      def findColumn(a: String): F[Int]
      def first: F[Boolean]
      def getArray(a: Int): F[SqlArray]
      def getArray(a: String): F[SqlArray]
      def getAsciiStream(a: Int): F[InputStream]
      def getAsciiStream(a: String): F[InputStream]
      def getBigDecimal(a: Int): F[BigDecimal]
      def getBigDecimal(a: Int, b: Int): F[BigDecimal]
      def getBigDecimal(a: String): F[BigDecimal]
      def getBigDecimal(a: String, b: Int): F[BigDecimal]
      def getBinaryStream(a: Int): F[InputStream]
      def getBinaryStream(a: String): F[InputStream]
      def getBlob(a: Int): F[Blob]
      def getBlob(a: String): F[Blob]
      def getBoolean(a: Int): F[Boolean]
      def getBoolean(a: String): F[Boolean]
      def getByte(a: Int): F[Byte]
      def getByte(a: String): F[Byte]
      def getBytes(a: Int): F[Array[Byte]]
      def getBytes(a: String): F[Array[Byte]]
      def getCharacterStream(a: Int): F[Reader]
      def getCharacterStream(a: String): F[Reader]
      def getClob(a: Int): F[Clob]
      def getClob(a: String): F[Clob]
      def getConcurrency: F[Int]
      def getCursorName: F[String]
      def getDate(a: Int): F[Date]
      def getDate(a: Int, b: Calendar): F[Date]
      def getDate(a: String): F[Date]
      def getDate(a: String, b: Calendar): F[Date]
      def getDouble(a: Int): F[Double]
      def getDouble(a: String): F[Double]
      def getFetchDirection: F[Int]
      def getFetchSize: F[Int]
      def getFloat(a: Int): F[Float]
      def getFloat(a: String): F[Float]
      def getHoldability: F[Int]
      def getInt(a: Int): F[Int]
      def getInt(a: String): F[Int]
      def getLong(a: Int): F[Long]
      def getLong(a: String): F[Long]
      def getMetaData: F[ResultSetMetaData]
      def getNCharacterStream(a: Int): F[Reader]
      def getNCharacterStream(a: String): F[Reader]
      def getNClob(a: Int): F[NClob]
      def getNClob(a: String): F[NClob]
      def getNString(a: Int): F[String]
      def getNString(a: String): F[String]
      def getObject(a: Int): F[AnyRef]
      def getObject[T](a: Int, b: Class[T]): F[T]
      def getObject(a: Int, b: Map[String, Class[_]]): F[AnyRef]
      def getObject(a: String): F[AnyRef]
      def getObject[T](a: String, b: Class[T]): F[T]
      def getObject(a: String, b: Map[String, Class[_]]): F[AnyRef]
      def getRef(a: Int): F[Ref]
      def getRef(a: String): F[Ref]
      def getRow: F[Int]
      def getRowId(a: Int): F[RowId]
      def getRowId(a: String): F[RowId]
      def getSQLXML(a: Int): F[SQLXML]
      def getSQLXML(a: String): F[SQLXML]
      def getShort(a: Int): F[Short]
      def getShort(a: String): F[Short]
      def getStatement: F[Statement]
      def getString(a: Int): F[String]
      def getString(a: String): F[String]
      def getTime(a: Int): F[Time]
      def getTime(a: Int, b: Calendar): F[Time]
      def getTime(a: String): F[Time]
      def getTime(a: String, b: Calendar): F[Time]
      def getTimestamp(a: Int): F[Timestamp]
      def getTimestamp(a: Int, b: Calendar): F[Timestamp]
      def getTimestamp(a: String): F[Timestamp]
      def getTimestamp(a: String, b: Calendar): F[Timestamp]
      def getType: F[Int]
      def getURL(a: Int): F[URL]
      def getURL(a: String): F[URL]
      def getUnicodeStream(a: Int): F[InputStream]
      def getUnicodeStream(a: String): F[InputStream]
      def getWarnings: F[SQLWarning]
      def insertRow: F[Unit]
      def isAfterLast: F[Boolean]
      def isBeforeFirst: F[Boolean]
      def isClosed: F[Boolean]
      def isFirst: F[Boolean]
      def isLast: F[Boolean]
      def isWrapperFor(a: Class[_]): F[Boolean]
      def last: F[Boolean]
      def moveToCurrentRow: F[Unit]
      def moveToInsertRow: F[Unit]
      def next: F[Boolean]
      def previous: F[Boolean]
      def refreshRow: F[Unit]
      def relative(a: Int): F[Boolean]
      def rowDeleted: F[Boolean]
      def rowInserted: F[Boolean]
      def rowUpdated: F[Boolean]
      def setFetchDirection(a: Int): F[Unit]
      def setFetchSize(a: Int): F[Unit]
      def unwrap[T](a: Class[T]): F[T]
      def updateArray(a: Int, b: SqlArray): F[Unit]
      def updateArray(a: String, b: SqlArray): F[Unit]
      def updateAsciiStream(a: Int, b: InputStream): F[Unit]
      def updateAsciiStream(a: Int, b: InputStream, c: Int): F[Unit]
      def updateAsciiStream(a: Int, b: InputStream, c: Long): F[Unit]
      def updateAsciiStream(a: String, b: InputStream): F[Unit]
      def updateAsciiStream(a: String, b: InputStream, c: Int): F[Unit]
      def updateAsciiStream(a: String, b: InputStream, c: Long): F[Unit]
      def updateBigDecimal(a: Int, b: BigDecimal): F[Unit]
      def updateBigDecimal(a: String, b: BigDecimal): F[Unit]
      def updateBinaryStream(a: Int, b: InputStream): F[Unit]
      def updateBinaryStream(a: Int, b: InputStream, c: Int): F[Unit]
      def updateBinaryStream(a: Int, b: InputStream, c: Long): F[Unit]
      def updateBinaryStream(a: String, b: InputStream): F[Unit]
      def updateBinaryStream(a: String, b: InputStream, c: Int): F[Unit]
      def updateBinaryStream(a: String, b: InputStream, c: Long): F[Unit]
      def updateBlob(a: Int, b: Blob): F[Unit]
      def updateBlob(a: Int, b: InputStream): F[Unit]
      def updateBlob(a: Int, b: InputStream, c: Long): F[Unit]
      def updateBlob(a: String, b: Blob): F[Unit]
      def updateBlob(a: String, b: InputStream): F[Unit]
      def updateBlob(a: String, b: InputStream, c: Long): F[Unit]
      def updateBoolean(a: Int, b: Boolean): F[Unit]
      def updateBoolean(a: String, b: Boolean): F[Unit]
      def updateByte(a: Int, b: Byte): F[Unit]
      def updateByte(a: String, b: Byte): F[Unit]
      def updateBytes(a: Int, b: Array[Byte]): F[Unit]
      def updateBytes(a: String, b: Array[Byte]): F[Unit]
      def updateCharacterStream(a: Int, b: Reader): F[Unit]
      def updateCharacterStream(a: Int, b: Reader, c: Int): F[Unit]
      def updateCharacterStream(a: Int, b: Reader, c: Long): F[Unit]
      def updateCharacterStream(a: String, b: Reader): F[Unit]
      def updateCharacterStream(a: String, b: Reader, c: Int): F[Unit]
      def updateCharacterStream(a: String, b: Reader, c: Long): F[Unit]
      def updateClob(a: Int, b: Clob): F[Unit]
      def updateClob(a: Int, b: Reader): F[Unit]
      def updateClob(a: Int, b: Reader, c: Long): F[Unit]
      def updateClob(a: String, b: Clob): F[Unit]
      def updateClob(a: String, b: Reader): F[Unit]
      def updateClob(a: String, b: Reader, c: Long): F[Unit]
      def updateDate(a: Int, b: Date): F[Unit]
      def updateDate(a: String, b: Date): F[Unit]
      def updateDouble(a: Int, b: Double): F[Unit]
      def updateDouble(a: String, b: Double): F[Unit]
      def updateFloat(a: Int, b: Float): F[Unit]
      def updateFloat(a: String, b: Float): F[Unit]
      def updateInt(a: Int, b: Int): F[Unit]
      def updateInt(a: String, b: Int): F[Unit]
      def updateLong(a: Int, b: Long): F[Unit]
      def updateLong(a: String, b: Long): F[Unit]
      def updateNCharacterStream(a: Int, b: Reader): F[Unit]
      def updateNCharacterStream(a: Int, b: Reader, c: Long): F[Unit]
      def updateNCharacterStream(a: String, b: Reader): F[Unit]
      def updateNCharacterStream(a: String, b: Reader, c: Long): F[Unit]
      def updateNClob(a: Int, b: NClob): F[Unit]
      def updateNClob(a: Int, b: Reader): F[Unit]
      def updateNClob(a: Int, b: Reader, c: Long): F[Unit]
      def updateNClob(a: String, b: NClob): F[Unit]
      def updateNClob(a: String, b: Reader): F[Unit]
      def updateNClob(a: String, b: Reader, c: Long): F[Unit]
      def updateNString(a: Int, b: String): F[Unit]
      def updateNString(a: String, b: String): F[Unit]
      def updateNull(a: Int): F[Unit]
      def updateNull(a: String): F[Unit]
      def updateObject(a: Int, b: AnyRef): F[Unit]
      def updateObject(a: Int, b: AnyRef, c: Int): F[Unit]
      def updateObject(a: Int, b: AnyRef, c: SQLType): F[Unit]
      def updateObject(a: Int, b: AnyRef, c: SQLType, d: Int): F[Unit]
      def updateObject(a: String, b: AnyRef): F[Unit]
      def updateObject(a: String, b: AnyRef, c: Int): F[Unit]
      def updateObject(a: String, b: AnyRef, c: SQLType): F[Unit]
      def updateObject(a: String, b: AnyRef, c: SQLType, d: Int): F[Unit]
      def updateRef(a: Int, b: Ref): F[Unit]
      def updateRef(a: String, b: Ref): F[Unit]
      def updateRow: F[Unit]
      def updateRowId(a: Int, b: RowId): F[Unit]
      def updateRowId(a: String, b: RowId): F[Unit]
      def updateSQLXML(a: Int, b: SQLXML): F[Unit]
      def updateSQLXML(a: String, b: SQLXML): F[Unit]
      def updateShort(a: Int, b: Short): F[Unit]
      def updateShort(a: String, b: Short): F[Unit]
      def updateString(a: Int, b: String): F[Unit]
      def updateString(a: String, b: String): F[Unit]
      def updateTime(a: Int, b: Time): F[Unit]
      def updateTime(a: String, b: Time): F[Unit]
      def updateTimestamp(a: Int, b: Timestamp): F[Unit]
      def updateTimestamp(a: String, b: Timestamp): F[Unit]
      def wasNull: F[Boolean]

    }

    // Common operations for all algebras.
    final case class Raw[A](f: Env[ResultSet] => A) extends ResultSetOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(f)
    }
    final case class Embed[A](e: Embedded[A]) extends ResultSetOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class Delay[A](a: () => A) extends ResultSetOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.delay(a)
    }
    final case class HandleErrorWith[A](fa: ResultSetIO[A], f: Throwable => ResultSetIO[A]) extends ResultSetOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa, f)
    }
    final case class Async1[A](k: (Either[Throwable, A] => Unit) => Unit) extends ResultSetOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.async(k)
    }
    final case class AsyncF[A](k: (Either[Throwable, A] => Unit) => ResultSetIO[Unit]) extends ResultSetOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.asyncF(k)
    }
    final case class BracketCase[A, B](acquire: ResultSetIO[A], use: A => ResultSetIO[B], release: (A, ExitCase[Throwable]) => ResultSetIO[Unit]) extends ResultSetOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.bracketCase(acquire)(use)(release)
    }
    final case object Shift extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.shift
    }
    final case class EvalOn[A](ec: ExecutionContext, fa: ResultSetIO[A]) extends ResultSetOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.evalOn(ec)(fa)
    }
    final case class LiftE[G[_]](env: Env[ResultSet] => G ~> ResultSetIO) extends ResultSetOp[G ~> ResultSetIO] {
      def visit[F[_]](v: Visitor[F]) = v.liftE(env)
    }

    // ResultSet-specific operations.
    final case class  Absolute(a: Int) extends ResultSetOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.absolute(a)
    }
    final case object AfterLast extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.afterLast
    }
    final case object BeforeFirst extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.beforeFirst
    }
    final case object CancelRowUpdates extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.cancelRowUpdates
    }
    final case object ClearWarnings extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.clearWarnings
    }
    final case object Close extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.close
    }
    final case object DeleteRow extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.deleteRow
    }
    final case class  FindColumn(a: String) extends ResultSetOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.findColumn(a)
    }
    final case object First extends ResultSetOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.first
    }
    final case class  GetArray(a: Int) extends ResultSetOp[SqlArray] {
      def visit[F[_]](v: Visitor[F]) = v.getArray(a)
    }
    final case class  GetArray1(a: String) extends ResultSetOp[SqlArray] {
      def visit[F[_]](v: Visitor[F]) = v.getArray(a)
    }
    final case class  GetAsciiStream(a: Int) extends ResultSetOp[InputStream] {
      def visit[F[_]](v: Visitor[F]) = v.getAsciiStream(a)
    }
    final case class  GetAsciiStream1(a: String) extends ResultSetOp[InputStream] {
      def visit[F[_]](v: Visitor[F]) = v.getAsciiStream(a)
    }
    final case class  GetBigDecimal(a: Int) extends ResultSetOp[BigDecimal] {
      def visit[F[_]](v: Visitor[F]) = v.getBigDecimal(a)
    }
    final case class  GetBigDecimal1(a: Int, b: Int) extends ResultSetOp[BigDecimal] {
      def visit[F[_]](v: Visitor[F]) = v.getBigDecimal(a, b)
    }
    final case class  GetBigDecimal2(a: String) extends ResultSetOp[BigDecimal] {
      def visit[F[_]](v: Visitor[F]) = v.getBigDecimal(a)
    }
    final case class  GetBigDecimal3(a: String, b: Int) extends ResultSetOp[BigDecimal] {
      def visit[F[_]](v: Visitor[F]) = v.getBigDecimal(a, b)
    }
    final case class  GetBinaryStream(a: Int) extends ResultSetOp[InputStream] {
      def visit[F[_]](v: Visitor[F]) = v.getBinaryStream(a)
    }
    final case class  GetBinaryStream1(a: String) extends ResultSetOp[InputStream] {
      def visit[F[_]](v: Visitor[F]) = v.getBinaryStream(a)
    }
    final case class  GetBlob(a: Int) extends ResultSetOp[Blob] {
      def visit[F[_]](v: Visitor[F]) = v.getBlob(a)
    }
    final case class  GetBlob1(a: String) extends ResultSetOp[Blob] {
      def visit[F[_]](v: Visitor[F]) = v.getBlob(a)
    }
    final case class  GetBoolean(a: Int) extends ResultSetOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.getBoolean(a)
    }
    final case class  GetBoolean1(a: String) extends ResultSetOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.getBoolean(a)
    }
    final case class  GetByte(a: Int) extends ResultSetOp[Byte] {
      def visit[F[_]](v: Visitor[F]) = v.getByte(a)
    }
    final case class  GetByte1(a: String) extends ResultSetOp[Byte] {
      def visit[F[_]](v: Visitor[F]) = v.getByte(a)
    }
    final case class  GetBytes(a: Int) extends ResultSetOp[Array[Byte]] {
      def visit[F[_]](v: Visitor[F]) = v.getBytes(a)
    }
    final case class  GetBytes1(a: String) extends ResultSetOp[Array[Byte]] {
      def visit[F[_]](v: Visitor[F]) = v.getBytes(a)
    }
    final case class  GetCharacterStream(a: Int) extends ResultSetOp[Reader] {
      def visit[F[_]](v: Visitor[F]) = v.getCharacterStream(a)
    }
    final case class  GetCharacterStream1(a: String) extends ResultSetOp[Reader] {
      def visit[F[_]](v: Visitor[F]) = v.getCharacterStream(a)
    }
    final case class  GetClob(a: Int) extends ResultSetOp[Clob] {
      def visit[F[_]](v: Visitor[F]) = v.getClob(a)
    }
    final case class  GetClob1(a: String) extends ResultSetOp[Clob] {
      def visit[F[_]](v: Visitor[F]) = v.getClob(a)
    }
    final case object GetConcurrency extends ResultSetOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getConcurrency
    }
    final case object GetCursorName extends ResultSetOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getCursorName
    }
    final case class  GetDate(a: Int) extends ResultSetOp[Date] {
      def visit[F[_]](v: Visitor[F]) = v.getDate(a)
    }
    final case class  GetDate1(a: Int, b: Calendar) extends ResultSetOp[Date] {
      def visit[F[_]](v: Visitor[F]) = v.getDate(a, b)
    }
    final case class  GetDate2(a: String) extends ResultSetOp[Date] {
      def visit[F[_]](v: Visitor[F]) = v.getDate(a)
    }
    final case class  GetDate3(a: String, b: Calendar) extends ResultSetOp[Date] {
      def visit[F[_]](v: Visitor[F]) = v.getDate(a, b)
    }
    final case class  GetDouble(a: Int) extends ResultSetOp[Double] {
      def visit[F[_]](v: Visitor[F]) = v.getDouble(a)
    }
    final case class  GetDouble1(a: String) extends ResultSetOp[Double] {
      def visit[F[_]](v: Visitor[F]) = v.getDouble(a)
    }
    final case object GetFetchDirection extends ResultSetOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getFetchDirection
    }
    final case object GetFetchSize extends ResultSetOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getFetchSize
    }
    final case class  GetFloat(a: Int) extends ResultSetOp[Float] {
      def visit[F[_]](v: Visitor[F]) = v.getFloat(a)
    }
    final case class  GetFloat1(a: String) extends ResultSetOp[Float] {
      def visit[F[_]](v: Visitor[F]) = v.getFloat(a)
    }
    final case object GetHoldability extends ResultSetOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getHoldability
    }
    final case class  GetInt(a: Int) extends ResultSetOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getInt(a)
    }
    final case class  GetInt1(a: String) extends ResultSetOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getInt(a)
    }
    final case class  GetLong(a: Int) extends ResultSetOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.getLong(a)
    }
    final case class  GetLong1(a: String) extends ResultSetOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.getLong(a)
    }
    final case object GetMetaData extends ResultSetOp[ResultSetMetaData] {
      def visit[F[_]](v: Visitor[F]) = v.getMetaData
    }
    final case class  GetNCharacterStream(a: Int) extends ResultSetOp[Reader] {
      def visit[F[_]](v: Visitor[F]) = v.getNCharacterStream(a)
    }
    final case class  GetNCharacterStream1(a: String) extends ResultSetOp[Reader] {
      def visit[F[_]](v: Visitor[F]) = v.getNCharacterStream(a)
    }
    final case class  GetNClob(a: Int) extends ResultSetOp[NClob] {
      def visit[F[_]](v: Visitor[F]) = v.getNClob(a)
    }
    final case class  GetNClob1(a: String) extends ResultSetOp[NClob] {
      def visit[F[_]](v: Visitor[F]) = v.getNClob(a)
    }
    final case class  GetNString(a: Int) extends ResultSetOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getNString(a)
    }
    final case class  GetNString1(a: String) extends ResultSetOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getNString(a)
    }
    final case class  GetObject(a: Int) extends ResultSetOp[AnyRef] {
      def visit[F[_]](v: Visitor[F]) = v.getObject(a)
    }
    final case class  GetObject1[T](a: Int, b: Class[T]) extends ResultSetOp[T] {
      def visit[F[_]](v: Visitor[F]) = v.getObject(a, b)
    }
    final case class  GetObject2(a: Int, b: Map[String, Class[_]]) extends ResultSetOp[AnyRef] {
      def visit[F[_]](v: Visitor[F]) = v.getObject(a, b)
    }
    final case class  GetObject3(a: String) extends ResultSetOp[AnyRef] {
      def visit[F[_]](v: Visitor[F]) = v.getObject(a)
    }
    final case class  GetObject4[T](a: String, b: Class[T]) extends ResultSetOp[T] {
      def visit[F[_]](v: Visitor[F]) = v.getObject(a, b)
    }
    final case class  GetObject5(a: String, b: Map[String, Class[_]]) extends ResultSetOp[AnyRef] {
      def visit[F[_]](v: Visitor[F]) = v.getObject(a, b)
    }
    final case class  GetRef(a: Int) extends ResultSetOp[Ref] {
      def visit[F[_]](v: Visitor[F]) = v.getRef(a)
    }
    final case class  GetRef1(a: String) extends ResultSetOp[Ref] {
      def visit[F[_]](v: Visitor[F]) = v.getRef(a)
    }
    final case object GetRow extends ResultSetOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getRow
    }
    final case class  GetRowId(a: Int) extends ResultSetOp[RowId] {
      def visit[F[_]](v: Visitor[F]) = v.getRowId(a)
    }
    final case class  GetRowId1(a: String) extends ResultSetOp[RowId] {
      def visit[F[_]](v: Visitor[F]) = v.getRowId(a)
    }
    final case class  GetSQLXML(a: Int) extends ResultSetOp[SQLXML] {
      def visit[F[_]](v: Visitor[F]) = v.getSQLXML(a)
    }
    final case class  GetSQLXML1(a: String) extends ResultSetOp[SQLXML] {
      def visit[F[_]](v: Visitor[F]) = v.getSQLXML(a)
    }
    final case class  GetShort(a: Int) extends ResultSetOp[Short] {
      def visit[F[_]](v: Visitor[F]) = v.getShort(a)
    }
    final case class  GetShort1(a: String) extends ResultSetOp[Short] {
      def visit[F[_]](v: Visitor[F]) = v.getShort(a)
    }
    final case object GetStatement extends ResultSetOp[Statement] {
      def visit[F[_]](v: Visitor[F]) = v.getStatement
    }
    final case class  GetString(a: Int) extends ResultSetOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getString(a)
    }
    final case class  GetString1(a: String) extends ResultSetOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getString(a)
    }
    final case class  GetTime(a: Int) extends ResultSetOp[Time] {
      def visit[F[_]](v: Visitor[F]) = v.getTime(a)
    }
    final case class  GetTime1(a: Int, b: Calendar) extends ResultSetOp[Time] {
      def visit[F[_]](v: Visitor[F]) = v.getTime(a, b)
    }
    final case class  GetTime2(a: String) extends ResultSetOp[Time] {
      def visit[F[_]](v: Visitor[F]) = v.getTime(a)
    }
    final case class  GetTime3(a: String, b: Calendar) extends ResultSetOp[Time] {
      def visit[F[_]](v: Visitor[F]) = v.getTime(a, b)
    }
    final case class  GetTimestamp(a: Int) extends ResultSetOp[Timestamp] {
      def visit[F[_]](v: Visitor[F]) = v.getTimestamp(a)
    }
    final case class  GetTimestamp1(a: Int, b: Calendar) extends ResultSetOp[Timestamp] {
      def visit[F[_]](v: Visitor[F]) = v.getTimestamp(a, b)
    }
    final case class  GetTimestamp2(a: String) extends ResultSetOp[Timestamp] {
      def visit[F[_]](v: Visitor[F]) = v.getTimestamp(a)
    }
    final case class  GetTimestamp3(a: String, b: Calendar) extends ResultSetOp[Timestamp] {
      def visit[F[_]](v: Visitor[F]) = v.getTimestamp(a, b)
    }
    final case object GetType extends ResultSetOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getType
    }
    final case class  GetURL(a: Int) extends ResultSetOp[URL] {
      def visit[F[_]](v: Visitor[F]) = v.getURL(a)
    }
    final case class  GetURL1(a: String) extends ResultSetOp[URL] {
      def visit[F[_]](v: Visitor[F]) = v.getURL(a)
    }
    final case class  GetUnicodeStream(a: Int) extends ResultSetOp[InputStream] {
      def visit[F[_]](v: Visitor[F]) = v.getUnicodeStream(a)
    }
    final case class  GetUnicodeStream1(a: String) extends ResultSetOp[InputStream] {
      def visit[F[_]](v: Visitor[F]) = v.getUnicodeStream(a)
    }
    final case object GetWarnings extends ResultSetOp[SQLWarning] {
      def visit[F[_]](v: Visitor[F]) = v.getWarnings
    }
    final case object InsertRow extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.insertRow
    }
    final case object IsAfterLast extends ResultSetOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isAfterLast
    }
    final case object IsBeforeFirst extends ResultSetOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isBeforeFirst
    }
    final case object IsClosed extends ResultSetOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isClosed
    }
    final case object IsFirst extends ResultSetOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isFirst
    }
    final case object IsLast extends ResultSetOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isLast
    }
    final case class  IsWrapperFor(a: Class[_]) extends ResultSetOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isWrapperFor(a)
    }
    final case object Last extends ResultSetOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.last
    }
    final case object MoveToCurrentRow extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.moveToCurrentRow
    }
    final case object MoveToInsertRow extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.moveToInsertRow
    }
    final case object Next extends ResultSetOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.next
    }
    final case object Previous extends ResultSetOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.previous
    }
    final case object RefreshRow extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.refreshRow
    }
    final case class  Relative(a: Int) extends ResultSetOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.relative(a)
    }
    final case object RowDeleted extends ResultSetOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.rowDeleted
    }
    final case object RowInserted extends ResultSetOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.rowInserted
    }
    final case object RowUpdated extends ResultSetOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.rowUpdated
    }
    final case class  SetFetchDirection(a: Int) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setFetchDirection(a)
    }
    final case class  SetFetchSize(a: Int) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setFetchSize(a)
    }
    final case class  Unwrap[T](a: Class[T]) extends ResultSetOp[T] {
      def visit[F[_]](v: Visitor[F]) = v.unwrap(a)
    }
    final case class  UpdateArray(a: Int, b: SqlArray) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateArray(a, b)
    }
    final case class  UpdateArray1(a: String, b: SqlArray) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateArray(a, b)
    }
    final case class  UpdateAsciiStream(a: Int, b: InputStream) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateAsciiStream(a, b)
    }
    final case class  UpdateAsciiStream1(a: Int, b: InputStream, c: Int) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateAsciiStream(a, b, c)
    }
    final case class  UpdateAsciiStream2(a: Int, b: InputStream, c: Long) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateAsciiStream(a, b, c)
    }
    final case class  UpdateAsciiStream3(a: String, b: InputStream) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateAsciiStream(a, b)
    }
    final case class  UpdateAsciiStream4(a: String, b: InputStream, c: Int) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateAsciiStream(a, b, c)
    }
    final case class  UpdateAsciiStream5(a: String, b: InputStream, c: Long) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateAsciiStream(a, b, c)
    }
    final case class  UpdateBigDecimal(a: Int, b: BigDecimal) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateBigDecimal(a, b)
    }
    final case class  UpdateBigDecimal1(a: String, b: BigDecimal) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateBigDecimal(a, b)
    }
    final case class  UpdateBinaryStream(a: Int, b: InputStream) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateBinaryStream(a, b)
    }
    final case class  UpdateBinaryStream1(a: Int, b: InputStream, c: Int) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateBinaryStream(a, b, c)
    }
    final case class  UpdateBinaryStream2(a: Int, b: InputStream, c: Long) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateBinaryStream(a, b, c)
    }
    final case class  UpdateBinaryStream3(a: String, b: InputStream) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateBinaryStream(a, b)
    }
    final case class  UpdateBinaryStream4(a: String, b: InputStream, c: Int) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateBinaryStream(a, b, c)
    }
    final case class  UpdateBinaryStream5(a: String, b: InputStream, c: Long) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateBinaryStream(a, b, c)
    }
    final case class  UpdateBlob(a: Int, b: Blob) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateBlob(a, b)
    }
    final case class  UpdateBlob1(a: Int, b: InputStream) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateBlob(a, b)
    }
    final case class  UpdateBlob2(a: Int, b: InputStream, c: Long) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateBlob(a, b, c)
    }
    final case class  UpdateBlob3(a: String, b: Blob) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateBlob(a, b)
    }
    final case class  UpdateBlob4(a: String, b: InputStream) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateBlob(a, b)
    }
    final case class  UpdateBlob5(a: String, b: InputStream, c: Long) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateBlob(a, b, c)
    }
    final case class  UpdateBoolean(a: Int, b: Boolean) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateBoolean(a, b)
    }
    final case class  UpdateBoolean1(a: String, b: Boolean) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateBoolean(a, b)
    }
    final case class  UpdateByte(a: Int, b: Byte) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateByte(a, b)
    }
    final case class  UpdateByte1(a: String, b: Byte) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateByte(a, b)
    }
    final case class  UpdateBytes(a: Int, b: Array[Byte]) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateBytes(a, b)
    }
    final case class  UpdateBytes1(a: String, b: Array[Byte]) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateBytes(a, b)
    }
    final case class  UpdateCharacterStream(a: Int, b: Reader) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateCharacterStream(a, b)
    }
    final case class  UpdateCharacterStream1(a: Int, b: Reader, c: Int) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateCharacterStream(a, b, c)
    }
    final case class  UpdateCharacterStream2(a: Int, b: Reader, c: Long) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateCharacterStream(a, b, c)
    }
    final case class  UpdateCharacterStream3(a: String, b: Reader) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateCharacterStream(a, b)
    }
    final case class  UpdateCharacterStream4(a: String, b: Reader, c: Int) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateCharacterStream(a, b, c)
    }
    final case class  UpdateCharacterStream5(a: String, b: Reader, c: Long) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateCharacterStream(a, b, c)
    }
    final case class  UpdateClob(a: Int, b: Clob) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateClob(a, b)
    }
    final case class  UpdateClob1(a: Int, b: Reader) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateClob(a, b)
    }
    final case class  UpdateClob2(a: Int, b: Reader, c: Long) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateClob(a, b, c)
    }
    final case class  UpdateClob3(a: String, b: Clob) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateClob(a, b)
    }
    final case class  UpdateClob4(a: String, b: Reader) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateClob(a, b)
    }
    final case class  UpdateClob5(a: String, b: Reader, c: Long) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateClob(a, b, c)
    }
    final case class  UpdateDate(a: Int, b: Date) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateDate(a, b)
    }
    final case class  UpdateDate1(a: String, b: Date) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateDate(a, b)
    }
    final case class  UpdateDouble(a: Int, b: Double) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateDouble(a, b)
    }
    final case class  UpdateDouble1(a: String, b: Double) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateDouble(a, b)
    }
    final case class  UpdateFloat(a: Int, b: Float) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateFloat(a, b)
    }
    final case class  UpdateFloat1(a: String, b: Float) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateFloat(a, b)
    }
    final case class  UpdateInt(a: Int, b: Int) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateInt(a, b)
    }
    final case class  UpdateInt1(a: String, b: Int) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateInt(a, b)
    }
    final case class  UpdateLong(a: Int, b: Long) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateLong(a, b)
    }
    final case class  UpdateLong1(a: String, b: Long) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateLong(a, b)
    }
    final case class  UpdateNCharacterStream(a: Int, b: Reader) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateNCharacterStream(a, b)
    }
    final case class  UpdateNCharacterStream1(a: Int, b: Reader, c: Long) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateNCharacterStream(a, b, c)
    }
    final case class  UpdateNCharacterStream2(a: String, b: Reader) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateNCharacterStream(a, b)
    }
    final case class  UpdateNCharacterStream3(a: String, b: Reader, c: Long) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateNCharacterStream(a, b, c)
    }
    final case class  UpdateNClob(a: Int, b: NClob) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateNClob(a, b)
    }
    final case class  UpdateNClob1(a: Int, b: Reader) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateNClob(a, b)
    }
    final case class  UpdateNClob2(a: Int, b: Reader, c: Long) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateNClob(a, b, c)
    }
    final case class  UpdateNClob3(a: String, b: NClob) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateNClob(a, b)
    }
    final case class  UpdateNClob4(a: String, b: Reader) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateNClob(a, b)
    }
    final case class  UpdateNClob5(a: String, b: Reader, c: Long) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateNClob(a, b, c)
    }
    final case class  UpdateNString(a: Int, b: String) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateNString(a, b)
    }
    final case class  UpdateNString1(a: String, b: String) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateNString(a, b)
    }
    final case class  UpdateNull(a: Int) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateNull(a)
    }
    final case class  UpdateNull1(a: String) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateNull(a)
    }
    final case class  UpdateObject(a: Int, b: AnyRef) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateObject(a, b)
    }
    final case class  UpdateObject1(a: Int, b: AnyRef, c: Int) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateObject(a, b, c)
    }
    final case class  UpdateObject2(a: Int, b: AnyRef, c: SQLType) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateObject(a, b, c)
    }
    final case class  UpdateObject3(a: Int, b: AnyRef, c: SQLType, d: Int) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateObject(a, b, c, d)
    }
    final case class  UpdateObject4(a: String, b: AnyRef) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateObject(a, b)
    }
    final case class  UpdateObject5(a: String, b: AnyRef, c: Int) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateObject(a, b, c)
    }
    final case class  UpdateObject6(a: String, b: AnyRef, c: SQLType) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateObject(a, b, c)
    }
    final case class  UpdateObject7(a: String, b: AnyRef, c: SQLType, d: Int) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateObject(a, b, c, d)
    }
    final case class  UpdateRef(a: Int, b: Ref) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateRef(a, b)
    }
    final case class  UpdateRef1(a: String, b: Ref) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateRef(a, b)
    }
    final case object UpdateRow extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateRow
    }
    final case class  UpdateRowId(a: Int, b: RowId) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateRowId(a, b)
    }
    final case class  UpdateRowId1(a: String, b: RowId) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateRowId(a, b)
    }
    final case class  UpdateSQLXML(a: Int, b: SQLXML) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateSQLXML(a, b)
    }
    final case class  UpdateSQLXML1(a: String, b: SQLXML) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateSQLXML(a, b)
    }
    final case class  UpdateShort(a: Int, b: Short) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateShort(a, b)
    }
    final case class  UpdateShort1(a: String, b: Short) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateShort(a, b)
    }
    final case class  UpdateString(a: Int, b: String) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateString(a, b)
    }
    final case class  UpdateString1(a: String, b: String) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateString(a, b)
    }
    final case class  UpdateTime(a: Int, b: Time) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateTime(a, b)
    }
    final case class  UpdateTime1(a: String, b: Time) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateTime(a, b)
    }
    final case class  UpdateTimestamp(a: Int, b: Timestamp) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateTimestamp(a, b)
    }
    final case class  UpdateTimestamp1(a: String, b: Timestamp) extends ResultSetOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.updateTimestamp(a, b)
    }
    final case object WasNull extends ResultSetOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.wasNull
    }

  }
  import ResultSetOp._

  // Smart constructors for operations common to all algebras.
  val unit: ResultSetIO[Unit] = FF.pure[ResultSetOp, Unit](())
  def pure[A](a: A): ResultSetIO[A] = FF.pure[ResultSetOp, A](a)
  def raw[A](f: Env[ResultSet] => A): ResultSetIO[A] = FF.liftF(Raw(f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[ResultSetOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def delay[A](a: => A): ResultSetIO[A] = FF.liftF(Delay(() => a))
  def handleErrorWith[A](fa: ResultSetIO[A], f: Throwable => ResultSetIO[A]): ResultSetIO[A] = FF.liftF[ResultSetOp, A](HandleErrorWith(fa, f))
  def raiseError[A](err: Throwable): ResultSetIO[A] = delay(throw err)
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): ResultSetIO[A] = FF.liftF[ResultSetOp, A](Async1(k))
  def asyncF[A](k: (Either[Throwable, A] => Unit) => ResultSetIO[Unit]): ResultSetIO[A] = FF.liftF[ResultSetOp, A](AsyncF(k))
  def bracketCase[A, B](acquire: ResultSetIO[A])(use: A => ResultSetIO[B])(release: (A, ExitCase[Throwable]) => ResultSetIO[Unit]): ResultSetIO[B] = FF.liftF[ResultSetOp, B](BracketCase(acquire, use, release))
  val shift: ResultSetIO[Unit] = FF.liftF[ResultSetOp, Unit](Shift)
  def evalOn[A](ec: ExecutionContext)(fa: ResultSetIO[A]) = FF.liftF[ResultSetOp, A](EvalOn(ec, fa))
  def liftE[F[_]](env: Env[ResultSet] => F ~> ResultSetIO) = FF.liftF[ResultSetOp, F ~> ResultSetIO](LiftE(env))

  // Smart constructors for ResultSet-specific operations.
  def absolute(a: Int): ResultSetIO[Boolean] = FF.liftF(Absolute(a))
  val afterLast: ResultSetIO[Unit] = FF.liftF(AfterLast)
  val beforeFirst: ResultSetIO[Unit] = FF.liftF(BeforeFirst)
  val cancelRowUpdates: ResultSetIO[Unit] = FF.liftF(CancelRowUpdates)
  val clearWarnings: ResultSetIO[Unit] = FF.liftF(ClearWarnings)
  val close: ResultSetIO[Unit] = FF.liftF(Close)
  val deleteRow: ResultSetIO[Unit] = FF.liftF(DeleteRow)
  def findColumn(a: String): ResultSetIO[Int] = FF.liftF(FindColumn(a))
  val first: ResultSetIO[Boolean] = FF.liftF(First)
  def getArray(a: Int): ResultSetIO[SqlArray] = FF.liftF(GetArray(a))
  def getArray(a: String): ResultSetIO[SqlArray] = FF.liftF(GetArray1(a))
  def getAsciiStream(a: Int): ResultSetIO[InputStream] = FF.liftF(GetAsciiStream(a))
  def getAsciiStream(a: String): ResultSetIO[InputStream] = FF.liftF(GetAsciiStream1(a))
  def getBigDecimal(a: Int): ResultSetIO[BigDecimal] = FF.liftF(GetBigDecimal(a))
  def getBigDecimal(a: Int, b: Int): ResultSetIO[BigDecimal] = FF.liftF(GetBigDecimal1(a, b))
  def getBigDecimal(a: String): ResultSetIO[BigDecimal] = FF.liftF(GetBigDecimal2(a))
  def getBigDecimal(a: String, b: Int): ResultSetIO[BigDecimal] = FF.liftF(GetBigDecimal3(a, b))
  def getBinaryStream(a: Int): ResultSetIO[InputStream] = FF.liftF(GetBinaryStream(a))
  def getBinaryStream(a: String): ResultSetIO[InputStream] = FF.liftF(GetBinaryStream1(a))
  def getBlob(a: Int): ResultSetIO[Blob] = FF.liftF(GetBlob(a))
  def getBlob(a: String): ResultSetIO[Blob] = FF.liftF(GetBlob1(a))
  def getBoolean(a: Int): ResultSetIO[Boolean] = FF.liftF(GetBoolean(a))
  def getBoolean(a: String): ResultSetIO[Boolean] = FF.liftF(GetBoolean1(a))
  def getByte(a: Int): ResultSetIO[Byte] = FF.liftF(GetByte(a))
  def getByte(a: String): ResultSetIO[Byte] = FF.liftF(GetByte1(a))
  def getBytes(a: Int): ResultSetIO[Array[Byte]] = FF.liftF(GetBytes(a))
  def getBytes(a: String): ResultSetIO[Array[Byte]] = FF.liftF(GetBytes1(a))
  def getCharacterStream(a: Int): ResultSetIO[Reader] = FF.liftF(GetCharacterStream(a))
  def getCharacterStream(a: String): ResultSetIO[Reader] = FF.liftF(GetCharacterStream1(a))
  def getClob(a: Int): ResultSetIO[Clob] = FF.liftF(GetClob(a))
  def getClob(a: String): ResultSetIO[Clob] = FF.liftF(GetClob1(a))
  val getConcurrency: ResultSetIO[Int] = FF.liftF(GetConcurrency)
  val getCursorName: ResultSetIO[String] = FF.liftF(GetCursorName)
  def getDate(a: Int): ResultSetIO[Date] = FF.liftF(GetDate(a))
  def getDate(a: Int, b: Calendar): ResultSetIO[Date] = FF.liftF(GetDate1(a, b))
  def getDate(a: String): ResultSetIO[Date] = FF.liftF(GetDate2(a))
  def getDate(a: String, b: Calendar): ResultSetIO[Date] = FF.liftF(GetDate3(a, b))
  def getDouble(a: Int): ResultSetIO[Double] = FF.liftF(GetDouble(a))
  def getDouble(a: String): ResultSetIO[Double] = FF.liftF(GetDouble1(a))
  val getFetchDirection: ResultSetIO[Int] = FF.liftF(GetFetchDirection)
  val getFetchSize: ResultSetIO[Int] = FF.liftF(GetFetchSize)
  def getFloat(a: Int): ResultSetIO[Float] = FF.liftF(GetFloat(a))
  def getFloat(a: String): ResultSetIO[Float] = FF.liftF(GetFloat1(a))
  val getHoldability: ResultSetIO[Int] = FF.liftF(GetHoldability)
  def getInt(a: Int): ResultSetIO[Int] = FF.liftF(GetInt(a))
  def getInt(a: String): ResultSetIO[Int] = FF.liftF(GetInt1(a))
  def getLong(a: Int): ResultSetIO[Long] = FF.liftF(GetLong(a))
  def getLong(a: String): ResultSetIO[Long] = FF.liftF(GetLong1(a))
  val getMetaData: ResultSetIO[ResultSetMetaData] = FF.liftF(GetMetaData)
  def getNCharacterStream(a: Int): ResultSetIO[Reader] = FF.liftF(GetNCharacterStream(a))
  def getNCharacterStream(a: String): ResultSetIO[Reader] = FF.liftF(GetNCharacterStream1(a))
  def getNClob(a: Int): ResultSetIO[NClob] = FF.liftF(GetNClob(a))
  def getNClob(a: String): ResultSetIO[NClob] = FF.liftF(GetNClob1(a))
  def getNString(a: Int): ResultSetIO[String] = FF.liftF(GetNString(a))
  def getNString(a: String): ResultSetIO[String] = FF.liftF(GetNString1(a))
  def getObject(a: Int): ResultSetIO[AnyRef] = FF.liftF(GetObject(a))
  def getObject[T](a: Int, b: Class[T]): ResultSetIO[T] = FF.liftF(GetObject1(a, b))
  def getObject(a: Int, b: Map[String, Class[_]]): ResultSetIO[AnyRef] = FF.liftF(GetObject2(a, b))
  def getObject(a: String): ResultSetIO[AnyRef] = FF.liftF(GetObject3(a))
  def getObject[T](a: String, b: Class[T]): ResultSetIO[T] = FF.liftF(GetObject4(a, b))
  def getObject(a: String, b: Map[String, Class[_]]): ResultSetIO[AnyRef] = FF.liftF(GetObject5(a, b))
  def getRef(a: Int): ResultSetIO[Ref] = FF.liftF(GetRef(a))
  def getRef(a: String): ResultSetIO[Ref] = FF.liftF(GetRef1(a))
  val getRow: ResultSetIO[Int] = FF.liftF(GetRow)
  def getRowId(a: Int): ResultSetIO[RowId] = FF.liftF(GetRowId(a))
  def getRowId(a: String): ResultSetIO[RowId] = FF.liftF(GetRowId1(a))
  def getSQLXML(a: Int): ResultSetIO[SQLXML] = FF.liftF(GetSQLXML(a))
  def getSQLXML(a: String): ResultSetIO[SQLXML] = FF.liftF(GetSQLXML1(a))
  def getShort(a: Int): ResultSetIO[Short] = FF.liftF(GetShort(a))
  def getShort(a: String): ResultSetIO[Short] = FF.liftF(GetShort1(a))
  val getStatement: ResultSetIO[Statement] = FF.liftF(GetStatement)
  def getString(a: Int): ResultSetIO[String] = FF.liftF(GetString(a))
  def getString(a: String): ResultSetIO[String] = FF.liftF(GetString1(a))
  def getTime(a: Int): ResultSetIO[Time] = FF.liftF(GetTime(a))
  def getTime(a: Int, b: Calendar): ResultSetIO[Time] = FF.liftF(GetTime1(a, b))
  def getTime(a: String): ResultSetIO[Time] = FF.liftF(GetTime2(a))
  def getTime(a: String, b: Calendar): ResultSetIO[Time] = FF.liftF(GetTime3(a, b))
  def getTimestamp(a: Int): ResultSetIO[Timestamp] = FF.liftF(GetTimestamp(a))
  def getTimestamp(a: Int, b: Calendar): ResultSetIO[Timestamp] = FF.liftF(GetTimestamp1(a, b))
  def getTimestamp(a: String): ResultSetIO[Timestamp] = FF.liftF(GetTimestamp2(a))
  def getTimestamp(a: String, b: Calendar): ResultSetIO[Timestamp] = FF.liftF(GetTimestamp3(a, b))
  val getType: ResultSetIO[Int] = FF.liftF(GetType)
  def getURL(a: Int): ResultSetIO[URL] = FF.liftF(GetURL(a))
  def getURL(a: String): ResultSetIO[URL] = FF.liftF(GetURL1(a))
  def getUnicodeStream(a: Int): ResultSetIO[InputStream] = FF.liftF(GetUnicodeStream(a))
  def getUnicodeStream(a: String): ResultSetIO[InputStream] = FF.liftF(GetUnicodeStream1(a))
  val getWarnings: ResultSetIO[SQLWarning] = FF.liftF(GetWarnings)
  val insertRow: ResultSetIO[Unit] = FF.liftF(InsertRow)
  val isAfterLast: ResultSetIO[Boolean] = FF.liftF(IsAfterLast)
  val isBeforeFirst: ResultSetIO[Boolean] = FF.liftF(IsBeforeFirst)
  val isClosed: ResultSetIO[Boolean] = FF.liftF(IsClosed)
  val isFirst: ResultSetIO[Boolean] = FF.liftF(IsFirst)
  val isLast: ResultSetIO[Boolean] = FF.liftF(IsLast)
  def isWrapperFor(a: Class[_]): ResultSetIO[Boolean] = FF.liftF(IsWrapperFor(a))
  val last: ResultSetIO[Boolean] = FF.liftF(Last)
  val moveToCurrentRow: ResultSetIO[Unit] = FF.liftF(MoveToCurrentRow)
  val moveToInsertRow: ResultSetIO[Unit] = FF.liftF(MoveToInsertRow)
  val next: ResultSetIO[Boolean] = FF.liftF(Next)
  val previous: ResultSetIO[Boolean] = FF.liftF(Previous)
  val refreshRow: ResultSetIO[Unit] = FF.liftF(RefreshRow)
  def relative(a: Int): ResultSetIO[Boolean] = FF.liftF(Relative(a))
  val rowDeleted: ResultSetIO[Boolean] = FF.liftF(RowDeleted)
  val rowInserted: ResultSetIO[Boolean] = FF.liftF(RowInserted)
  val rowUpdated: ResultSetIO[Boolean] = FF.liftF(RowUpdated)
  def setFetchDirection(a: Int): ResultSetIO[Unit] = FF.liftF(SetFetchDirection(a))
  def setFetchSize(a: Int): ResultSetIO[Unit] = FF.liftF(SetFetchSize(a))
  def unwrap[T](a: Class[T]): ResultSetIO[T] = FF.liftF(Unwrap(a))
  def updateArray(a: Int, b: SqlArray): ResultSetIO[Unit] = FF.liftF(UpdateArray(a, b))
  def updateArray(a: String, b: SqlArray): ResultSetIO[Unit] = FF.liftF(UpdateArray1(a, b))
  def updateAsciiStream(a: Int, b: InputStream): ResultSetIO[Unit] = FF.liftF(UpdateAsciiStream(a, b))
  def updateAsciiStream(a: Int, b: InputStream, c: Int): ResultSetIO[Unit] = FF.liftF(UpdateAsciiStream1(a, b, c))
  def updateAsciiStream(a: Int, b: InputStream, c: Long): ResultSetIO[Unit] = FF.liftF(UpdateAsciiStream2(a, b, c))
  def updateAsciiStream(a: String, b: InputStream): ResultSetIO[Unit] = FF.liftF(UpdateAsciiStream3(a, b))
  def updateAsciiStream(a: String, b: InputStream, c: Int): ResultSetIO[Unit] = FF.liftF(UpdateAsciiStream4(a, b, c))
  def updateAsciiStream(a: String, b: InputStream, c: Long): ResultSetIO[Unit] = FF.liftF(UpdateAsciiStream5(a, b, c))
  def updateBigDecimal(a: Int, b: BigDecimal): ResultSetIO[Unit] = FF.liftF(UpdateBigDecimal(a, b))
  def updateBigDecimal(a: String, b: BigDecimal): ResultSetIO[Unit] = FF.liftF(UpdateBigDecimal1(a, b))
  def updateBinaryStream(a: Int, b: InputStream): ResultSetIO[Unit] = FF.liftF(UpdateBinaryStream(a, b))
  def updateBinaryStream(a: Int, b: InputStream, c: Int): ResultSetIO[Unit] = FF.liftF(UpdateBinaryStream1(a, b, c))
  def updateBinaryStream(a: Int, b: InputStream, c: Long): ResultSetIO[Unit] = FF.liftF(UpdateBinaryStream2(a, b, c))
  def updateBinaryStream(a: String, b: InputStream): ResultSetIO[Unit] = FF.liftF(UpdateBinaryStream3(a, b))
  def updateBinaryStream(a: String, b: InputStream, c: Int): ResultSetIO[Unit] = FF.liftF(UpdateBinaryStream4(a, b, c))
  def updateBinaryStream(a: String, b: InputStream, c: Long): ResultSetIO[Unit] = FF.liftF(UpdateBinaryStream5(a, b, c))
  def updateBlob(a: Int, b: Blob): ResultSetIO[Unit] = FF.liftF(UpdateBlob(a, b))
  def updateBlob(a: Int, b: InputStream): ResultSetIO[Unit] = FF.liftF(UpdateBlob1(a, b))
  def updateBlob(a: Int, b: InputStream, c: Long): ResultSetIO[Unit] = FF.liftF(UpdateBlob2(a, b, c))
  def updateBlob(a: String, b: Blob): ResultSetIO[Unit] = FF.liftF(UpdateBlob3(a, b))
  def updateBlob(a: String, b: InputStream): ResultSetIO[Unit] = FF.liftF(UpdateBlob4(a, b))
  def updateBlob(a: String, b: InputStream, c: Long): ResultSetIO[Unit] = FF.liftF(UpdateBlob5(a, b, c))
  def updateBoolean(a: Int, b: Boolean): ResultSetIO[Unit] = FF.liftF(UpdateBoolean(a, b))
  def updateBoolean(a: String, b: Boolean): ResultSetIO[Unit] = FF.liftF(UpdateBoolean1(a, b))
  def updateByte(a: Int, b: Byte): ResultSetIO[Unit] = FF.liftF(UpdateByte(a, b))
  def updateByte(a: String, b: Byte): ResultSetIO[Unit] = FF.liftF(UpdateByte1(a, b))
  def updateBytes(a: Int, b: Array[Byte]): ResultSetIO[Unit] = FF.liftF(UpdateBytes(a, b))
  def updateBytes(a: String, b: Array[Byte]): ResultSetIO[Unit] = FF.liftF(UpdateBytes1(a, b))
  def updateCharacterStream(a: Int, b: Reader): ResultSetIO[Unit] = FF.liftF(UpdateCharacterStream(a, b))
  def updateCharacterStream(a: Int, b: Reader, c: Int): ResultSetIO[Unit] = FF.liftF(UpdateCharacterStream1(a, b, c))
  def updateCharacterStream(a: Int, b: Reader, c: Long): ResultSetIO[Unit] = FF.liftF(UpdateCharacterStream2(a, b, c))
  def updateCharacterStream(a: String, b: Reader): ResultSetIO[Unit] = FF.liftF(UpdateCharacterStream3(a, b))
  def updateCharacterStream(a: String, b: Reader, c: Int): ResultSetIO[Unit] = FF.liftF(UpdateCharacterStream4(a, b, c))
  def updateCharacterStream(a: String, b: Reader, c: Long): ResultSetIO[Unit] = FF.liftF(UpdateCharacterStream5(a, b, c))
  def updateClob(a: Int, b: Clob): ResultSetIO[Unit] = FF.liftF(UpdateClob(a, b))
  def updateClob(a: Int, b: Reader): ResultSetIO[Unit] = FF.liftF(UpdateClob1(a, b))
  def updateClob(a: Int, b: Reader, c: Long): ResultSetIO[Unit] = FF.liftF(UpdateClob2(a, b, c))
  def updateClob(a: String, b: Clob): ResultSetIO[Unit] = FF.liftF(UpdateClob3(a, b))
  def updateClob(a: String, b: Reader): ResultSetIO[Unit] = FF.liftF(UpdateClob4(a, b))
  def updateClob(a: String, b: Reader, c: Long): ResultSetIO[Unit] = FF.liftF(UpdateClob5(a, b, c))
  def updateDate(a: Int, b: Date): ResultSetIO[Unit] = FF.liftF(UpdateDate(a, b))
  def updateDate(a: String, b: Date): ResultSetIO[Unit] = FF.liftF(UpdateDate1(a, b))
  def updateDouble(a: Int, b: Double): ResultSetIO[Unit] = FF.liftF(UpdateDouble(a, b))
  def updateDouble(a: String, b: Double): ResultSetIO[Unit] = FF.liftF(UpdateDouble1(a, b))
  def updateFloat(a: Int, b: Float): ResultSetIO[Unit] = FF.liftF(UpdateFloat(a, b))
  def updateFloat(a: String, b: Float): ResultSetIO[Unit] = FF.liftF(UpdateFloat1(a, b))
  def updateInt(a: Int, b: Int): ResultSetIO[Unit] = FF.liftF(UpdateInt(a, b))
  def updateInt(a: String, b: Int): ResultSetIO[Unit] = FF.liftF(UpdateInt1(a, b))
  def updateLong(a: Int, b: Long): ResultSetIO[Unit] = FF.liftF(UpdateLong(a, b))
  def updateLong(a: String, b: Long): ResultSetIO[Unit] = FF.liftF(UpdateLong1(a, b))
  def updateNCharacterStream(a: Int, b: Reader): ResultSetIO[Unit] = FF.liftF(UpdateNCharacterStream(a, b))
  def updateNCharacterStream(a: Int, b: Reader, c: Long): ResultSetIO[Unit] = FF.liftF(UpdateNCharacterStream1(a, b, c))
  def updateNCharacterStream(a: String, b: Reader): ResultSetIO[Unit] = FF.liftF(UpdateNCharacterStream2(a, b))
  def updateNCharacterStream(a: String, b: Reader, c: Long): ResultSetIO[Unit] = FF.liftF(UpdateNCharacterStream3(a, b, c))
  def updateNClob(a: Int, b: NClob): ResultSetIO[Unit] = FF.liftF(UpdateNClob(a, b))
  def updateNClob(a: Int, b: Reader): ResultSetIO[Unit] = FF.liftF(UpdateNClob1(a, b))
  def updateNClob(a: Int, b: Reader, c: Long): ResultSetIO[Unit] = FF.liftF(UpdateNClob2(a, b, c))
  def updateNClob(a: String, b: NClob): ResultSetIO[Unit] = FF.liftF(UpdateNClob3(a, b))
  def updateNClob(a: String, b: Reader): ResultSetIO[Unit] = FF.liftF(UpdateNClob4(a, b))
  def updateNClob(a: String, b: Reader, c: Long): ResultSetIO[Unit] = FF.liftF(UpdateNClob5(a, b, c))
  def updateNString(a: Int, b: String): ResultSetIO[Unit] = FF.liftF(UpdateNString(a, b))
  def updateNString(a: String, b: String): ResultSetIO[Unit] = FF.liftF(UpdateNString1(a, b))
  def updateNull(a: Int): ResultSetIO[Unit] = FF.liftF(UpdateNull(a))
  def updateNull(a: String): ResultSetIO[Unit] = FF.liftF(UpdateNull1(a))
  def updateObject(a: Int, b: AnyRef): ResultSetIO[Unit] = FF.liftF(UpdateObject(a, b))
  def updateObject(a: Int, b: AnyRef, c: Int): ResultSetIO[Unit] = FF.liftF(UpdateObject1(a, b, c))
  def updateObject(a: Int, b: AnyRef, c: SQLType): ResultSetIO[Unit] = FF.liftF(UpdateObject2(a, b, c))
  def updateObject(a: Int, b: AnyRef, c: SQLType, d: Int): ResultSetIO[Unit] = FF.liftF(UpdateObject3(a, b, c, d))
  def updateObject(a: String, b: AnyRef): ResultSetIO[Unit] = FF.liftF(UpdateObject4(a, b))
  def updateObject(a: String, b: AnyRef, c: Int): ResultSetIO[Unit] = FF.liftF(UpdateObject5(a, b, c))
  def updateObject(a: String, b: AnyRef, c: SQLType): ResultSetIO[Unit] = FF.liftF(UpdateObject6(a, b, c))
  def updateObject(a: String, b: AnyRef, c: SQLType, d: Int): ResultSetIO[Unit] = FF.liftF(UpdateObject7(a, b, c, d))
  def updateRef(a: Int, b: Ref): ResultSetIO[Unit] = FF.liftF(UpdateRef(a, b))
  def updateRef(a: String, b: Ref): ResultSetIO[Unit] = FF.liftF(UpdateRef1(a, b))
  val updateRow: ResultSetIO[Unit] = FF.liftF(UpdateRow)
  def updateRowId(a: Int, b: RowId): ResultSetIO[Unit] = FF.liftF(UpdateRowId(a, b))
  def updateRowId(a: String, b: RowId): ResultSetIO[Unit] = FF.liftF(UpdateRowId1(a, b))
  def updateSQLXML(a: Int, b: SQLXML): ResultSetIO[Unit] = FF.liftF(UpdateSQLXML(a, b))
  def updateSQLXML(a: String, b: SQLXML): ResultSetIO[Unit] = FF.liftF(UpdateSQLXML1(a, b))
  def updateShort(a: Int, b: Short): ResultSetIO[Unit] = FF.liftF(UpdateShort(a, b))
  def updateShort(a: String, b: Short): ResultSetIO[Unit] = FF.liftF(UpdateShort1(a, b))
  def updateString(a: Int, b: String): ResultSetIO[Unit] = FF.liftF(UpdateString(a, b))
  def updateString(a: String, b: String): ResultSetIO[Unit] = FF.liftF(UpdateString1(a, b))
  def updateTime(a: Int, b: Time): ResultSetIO[Unit] = FF.liftF(UpdateTime(a, b))
  def updateTime(a: String, b: Time): ResultSetIO[Unit] = FF.liftF(UpdateTime1(a, b))
  def updateTimestamp(a: Int, b: Timestamp): ResultSetIO[Unit] = FF.liftF(UpdateTimestamp(a, b))
  def updateTimestamp(a: String, b: Timestamp): ResultSetIO[Unit] = FF.liftF(UpdateTimestamp1(a, b))
  val wasNull: ResultSetIO[Boolean] = FF.liftF(WasNull)

  // ResultSetIO is an Async
  implicit val AsyncResultSetIO: Async[ResultSetIO] =
    new Async[ResultSetIO] {
      val asyncM = FF.catsFreeMonadForFree[ResultSetOp]
      def bracketCase[A, B](acquire: ResultSetIO[A])(use: A => ResultSetIO[B])(release: (A, ExitCase[Throwable]) => ResultSetIO[Unit]): ResultSetIO[B] = module.bracketCase(acquire)(use)(release)
      def pure[A](x: A): ResultSetIO[A] = asyncM.pure(x)
      def handleErrorWith[A](fa: ResultSetIO[A])(f: Throwable => ResultSetIO[A]): ResultSetIO[A] = module.handleErrorWith(fa, f)
      def raiseError[A](e: Throwable): ResultSetIO[A] = module.raiseError(e)
      def async[A](k: (Either[Throwable,A] => Unit) => Unit): ResultSetIO[A] = module.async(k)
      def asyncF[A](k: (Either[Throwable,A] => Unit) => ResultSetIO[Unit]): ResultSetIO[A] = module.asyncF(k)
      def flatMap[A, B](fa: ResultSetIO[A])(f: A => ResultSetIO[B]): ResultSetIO[B] = asyncM.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => ResultSetIO[Either[A, B]]): ResultSetIO[B] = asyncM.tailRecM(a)(f)
      def suspend[A](thunk: => ResultSetIO[A]): ResultSetIO[A] = asyncM.flatten(module.delay(thunk))
    }

  // ResultSetIO is a ContextShift
  implicit val ContextShiftResultSetIO: ContextShift[ResultSetIO] =
    new ContextShift[ResultSetIO] {
      def shift: ResultSetIO[Unit] = module.shift
      def evalOn[A](ec: ExecutionContext)(fa: ResultSetIO[A]) = module.evalOn(ec)(fa)
    }
}

