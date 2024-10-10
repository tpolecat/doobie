// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

// format: off

package doobie.free

// Library imports
import cats.~>
import cats.data.Kleisli
import cats.effect.kernel.{ Poll, Sync }
import cats.free.Free
import doobie.WeakAsync
import doobie.util.log.{LogEvent, LogHandler}
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

// Types referenced in the JDBC API
import java.io.InputStream
import java.io.Reader
import java.lang.Class
import java.lang.String
import java.math.BigDecimal
import java.net.URL
import java.sql.Blob
import java.sql.CallableStatement
import java.sql.Clob
import java.sql.Connection
import java.sql.DatabaseMetaData
import java.sql.Date
import java.sql.Driver
import java.sql.NClob
import java.sql.ParameterMetaData
import java.sql.PreparedStatement
import java.sql.Ref
import java.sql.ResultSet
import java.sql.ResultSetMetaData
import java.sql.RowId
import java.sql.RowIdLifetime
import java.sql.SQLData
import java.sql.SQLInput
import java.sql.SQLOutput
import java.sql.SQLType
import java.sql.SQLWarning
import java.sql.SQLXML
import java.sql.Savepoint
import java.sql.ShardingKey
import java.sql.Statement
import java.sql.Struct
import java.sql.Time
import java.sql.Timestamp
import java.sql.{ Array as SqlArray }
import java.util.Calendar
import java.util.Properties
import java.util.concurrent.Executor
import java.util.logging.Logger

// Algebras and free monads thereof referenced by our interpreter.
import doobie.free.nclob.{ NClobIO, NClobOp }
import doobie.free.blob.{ BlobIO, BlobOp }
import doobie.free.clob.{ ClobIO, ClobOp }
import doobie.free.databasemetadata.{ DatabaseMetaDataIO, DatabaseMetaDataOp }
import doobie.free.driver.{ DriverIO, DriverOp }
import doobie.free.ref.{ RefIO, RefOp }
import doobie.free.sqldata.{ SQLDataIO, SQLDataOp }
import doobie.free.sqlinput.{ SQLInputIO, SQLInputOp }
import doobie.free.sqloutput.{ SQLOutputIO, SQLOutputOp }
import doobie.free.connection.{ ConnectionIO, ConnectionOp }
import doobie.free.statement.{ StatementIO, StatementOp }
import doobie.free.preparedstatement.{ PreparedStatementIO, PreparedStatementOp }
import doobie.free.callablestatement.{ CallableStatementIO, CallableStatementOp }
import doobie.free.resultset.{ ResultSetIO, ResultSetOp }

object KleisliInterpreter {
  def apply[M[_]: WeakAsync](logHandler: LogHandler[M]): KleisliInterpreter[M] =
    new KleisliInterpreter[M](logHandler)
}

// Family of interpreters into Kleisli arrows for some monad M.
class KleisliInterpreter[M[_]](logHandler: LogHandler[M])(implicit val asyncM: WeakAsync[M]) { outer =>

  // The 14 interpreters, with definitions below. These can be overridden to customize behavior.
  lazy val NClobInterpreter: NClobOp ~> Kleisli[M, NClob, *] = new NClobInterpreter { }
  lazy val BlobInterpreter: BlobOp ~> Kleisli[M, Blob, *] = new BlobInterpreter { }
  lazy val ClobInterpreter: ClobOp ~> Kleisli[M, Clob, *] = new ClobInterpreter { }
  lazy val DatabaseMetaDataInterpreter: DatabaseMetaDataOp ~> Kleisli[M, DatabaseMetaData, *] = new DatabaseMetaDataInterpreter { }
  lazy val DriverInterpreter: DriverOp ~> Kleisli[M, Driver, *] = new DriverInterpreter { }
  lazy val RefInterpreter: RefOp ~> Kleisli[M, Ref, *] = new RefInterpreter { }
  lazy val SQLDataInterpreter: SQLDataOp ~> Kleisli[M, SQLData, *] = new SQLDataInterpreter { }
  lazy val SQLInputInterpreter: SQLInputOp ~> Kleisli[M, SQLInput, *] = new SQLInputInterpreter { }
  lazy val SQLOutputInterpreter: SQLOutputOp ~> Kleisli[M, SQLOutput, *] = new SQLOutputInterpreter { }
  lazy val ConnectionInterpreter: ConnectionOp ~> Kleisli[M, Connection, *] = new ConnectionInterpreter { }
  lazy val StatementInterpreter: StatementOp ~> Kleisli[M, Statement, *] = new StatementInterpreter { }
  lazy val PreparedStatementInterpreter: PreparedStatementOp ~> Kleisli[M, PreparedStatement, *] = new PreparedStatementInterpreter { }
  lazy val CallableStatementInterpreter: CallableStatementOp ~> Kleisli[M, CallableStatement, *] = new CallableStatementInterpreter { }
  lazy val ResultSetInterpreter: ResultSetOp ~> Kleisli[M, ResultSet, *] = new ResultSetInterpreter { }

  // Some methods are common to all interpreters and can be overridden to change behavior globally.
  def primitive[J, A](f: J => A): Kleisli[M, J, A] = Kleisli { a =>
    // primitive JDBC methods throw exceptions and so do we when reading values
    // so catch any non-fatal exceptions and lift them into the effect
    try {
      asyncM.blocking(f(a))
    } catch {
      case scala.util.control.NonFatal(e) => asyncM.raiseError(e)
    }
  }
  def raw[J, A](f: J => A): Kleisli[M, J, A] = primitive(f)
  def raiseError[J, A](e: Throwable): Kleisli[M, J, A] = Kleisli(_ => asyncM.raiseError(e))
  def monotonic[J]: Kleisli[M, J, FiniteDuration] = Kleisli(_ => asyncM.monotonic)
  def realTime[J]: Kleisli[M, J, FiniteDuration] = Kleisli(_ => asyncM.realTime)
  def delay[J, A](thunk: => A): Kleisli[M, J, A] = Kleisli(_ => asyncM.delay(thunk))
  def suspend[J, A](hint: Sync.Type)(thunk: => A): Kleisli[M, J, A] = Kleisli(_ => asyncM.suspend(hint)(thunk))
  def canceled[J]: Kleisli[M, J, Unit] = Kleisli(_ => asyncM.canceled)

  // for operations using free structures we call the interpreter recursively
  def handleErrorWith[G[_], J, A](interpreter: G ~> Kleisli[M, J, *])(fa: Free[G, A])(f: Throwable => Free[G, A]): Kleisli[M, J, A] = Kleisli (j =>
    asyncM.handleErrorWith(fa.foldMap(interpreter).run(j))(f.andThen(_.foldMap(interpreter).run(j)))
  )
  def forceR[G[_], J, A, B](interpreter: G ~> Kleisli[M, J, *])(fa: Free[G, A])(fb: Free[G, B]): Kleisli[M, J, B] = Kleisli (j =>
    asyncM.forceR(fa.foldMap(interpreter).run(j))(fb.foldMap(interpreter).run(j))
  )
  def uncancelable[G[_], J, A](interpreter: G ~> Kleisli[M, J, *], capture: Poll[M] => Poll[Free[G, *]])(body: Poll[Free[G, *]] => Free[G, A]): Kleisli[M, J, A] = Kleisli(j =>
    asyncM.uncancelable(body.compose(capture).andThen(_.foldMap(interpreter).run(j)))
  )
  def poll[G[_], J, A](interpreter: G ~> Kleisli[M, J, *])(mpoll: Any, fa: Free[G, A]): Kleisli[M, J, A] = Kleisli(j =>
    mpoll.asInstanceOf[Poll[M]].apply(fa.foldMap(interpreter).run(j))
  )
  def onCancel[G[_], J, A](interpreter: G ~> Kleisli[M, J, *])(fa: Free[G, A], fin: Free[G, Unit]): Kleisli[M, J, A] = Kleisli (j =>
    asyncM.onCancel(fa.foldMap(interpreter).run(j), fin.foldMap(interpreter).run(j))
  )
  def fromFuture[G[_], J, A](interpreter: G ~> Kleisli[M, J, *])(fut: Free[G, Future[A]]): Kleisli[M, J, A] = Kleisli(j =>
    asyncM.fromFuture(fut.foldMap(interpreter).run(j))
  )
  def fromFutureCancelable[G[_], J, A](interpreter: G ~> Kleisli[M, J, *])(fut: Free[G, (Future[A], Free[G, Unit])]): Kleisli[M, J, A] = Kleisli(j =>
    asyncM.fromFutureCancelable(fut.map { case (f, g) => (f, g.foldMap(interpreter).run(j)) }.foldMap(interpreter).run(j))
  )
  def cancelable[G[_], J, A](interpreter: G ~> Kleisli[M, J, *])(fa: Free[G, A], fin: Free[G, Unit]): Kleisli[M, J, A] = Kleisli (j =>
    asyncM.cancelable(fa.foldMap(interpreter).run(j), fin.foldMap(interpreter).run(j))
  )
  def embed[J, A](e: Embedded[A]): Kleisli[M, J, A] =
    e match {
      case Embedded.NClob(j, fa) => Kleisli(_ => fa.foldMap(NClobInterpreter).run(j))
      case Embedded.Blob(j, fa) => Kleisli(_ => fa.foldMap(BlobInterpreter).run(j))
      case Embedded.Clob(j, fa) => Kleisli(_ => fa.foldMap(ClobInterpreter).run(j))
      case Embedded.DatabaseMetaData(j, fa) => Kleisli(_ => fa.foldMap(DatabaseMetaDataInterpreter).run(j))
      case Embedded.Driver(j, fa) => Kleisli(_ => fa.foldMap(DriverInterpreter).run(j))
      case Embedded.Ref(j, fa) => Kleisli(_ => fa.foldMap(RefInterpreter).run(j))
      case Embedded.SQLData(j, fa) => Kleisli(_ => fa.foldMap(SQLDataInterpreter).run(j))
      case Embedded.SQLInput(j, fa) => Kleisli(_ => fa.foldMap(SQLInputInterpreter).run(j))
      case Embedded.SQLOutput(j, fa) => Kleisli(_ => fa.foldMap(SQLOutputInterpreter).run(j))
      case Embedded.Connection(j, fa) => Kleisli(_ => fa.foldMap(ConnectionInterpreter).run(j))
      case Embedded.Statement(j, fa) => Kleisli(_ => fa.foldMap(StatementInterpreter).run(j))
      case Embedded.PreparedStatement(j, fa) => Kleisli(_ => fa.foldMap(PreparedStatementInterpreter).run(j))
      case Embedded.CallableStatement(j, fa) => Kleisli(_ => fa.foldMap(CallableStatementInterpreter).run(j))
      case Embedded.ResultSet(j, fa) => Kleisli(_ => fa.foldMap(ResultSetInterpreter).run(j))
    }

  // Interpreters
  trait NClobInterpreter extends NClobOp.Visitor[Kleisli[M, NClob, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](f: NClob => A): Kleisli[M, NClob, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, NClob, A] = outer.embed(e)
    override def raiseError[A](e: Throwable): Kleisli[M, NClob, A] = outer.raiseError(e)
    override def monotonic: Kleisli[M, NClob, FiniteDuration] = outer.monotonic[NClob]
    override def realTime: Kleisli[M, NClob, FiniteDuration] = outer.realTime[NClob]
    override def delay[A](thunk: => A): Kleisli[M, NClob, A] = outer.delay(thunk)
    override def suspend[A](hint: Sync.Type)(thunk: => A): Kleisli[M, NClob, A] = outer.suspend(hint)(thunk)
    override def canceled: Kleisli[M, NClob, Unit] = outer.canceled[NClob]

    override def performLogging(event: LogEvent): Kleisli[M, NClob, Unit] = Kleisli(_ => logHandler.run(event))

    // for operations using NClobIO we must call ourself recursively
    override def handleErrorWith[A](fa: NClobIO[A])(f: Throwable => NClobIO[A]): Kleisli[M, NClob, A] = outer.handleErrorWith(this)(fa)(f)
    override def forceR[A, B](fa: NClobIO[A])(fb: NClobIO[B]): Kleisli[M, NClob, B] = outer.forceR(this)(fa)(fb)
    override def uncancelable[A](body: Poll[NClobIO] => NClobIO[A]): Kleisli[M, NClob, A] = outer.uncancelable(this, doobie.free.nclob.capturePoll)(body)
    override def poll[A](poll: Any, fa: NClobIO[A]): Kleisli[M, NClob, A] = outer.poll(this)(poll, fa)
    override def onCancel[A](fa: NClobIO[A], fin: NClobIO[Unit]): Kleisli[M, NClob, A] = outer.onCancel(this)(fa, fin)
    override def fromFuture[A](fut: NClobIO[Future[A]]): Kleisli[M, NClob, A] = outer.fromFuture(this)(fut)
    override def fromFutureCancelable[A](fut: NClobIO[(Future[A], NClobIO[Unit])]): Kleisli[M, NClob, A] = outer.fromFutureCancelable(this)(fut)
    override def cancelable[A](fa: NClobIO[A], fin: NClobIO[Unit]): Kleisli[M, NClob, A] = outer.cancelable(this)(fa, fin)


    // domain-specific operations are implemented in terms of `primitive`
    override def free: Kleisli[M, NClob, Unit] = primitive(_.free)
    override def getAsciiStream: Kleisli[M, NClob, InputStream] = primitive(_.getAsciiStream)
    override def getCharacterStream: Kleisli[M, NClob, Reader] = primitive(_.getCharacterStream)
    override def getCharacterStream(a: Long, b: Long) = primitive(_.getCharacterStream(a, b))
    override def getSubString(a: Long, b: Int) = primitive(_.getSubString(a, b))
    override def length: Kleisli[M, NClob, Long] = primitive(_.length)
    override def position(a: Clob, b: Long) = primitive(_.position(a, b))
    override def position(a: String, b: Long) = primitive(_.position(a, b))
    override def setAsciiStream(a: Long) = primitive(_.setAsciiStream(a))
    override def setCharacterStream(a: Long) = primitive(_.setCharacterStream(a))
    override def setString(a: Long, b: String) = primitive(_.setString(a, b))
    override def setString(a: Long, b: String, c: Int, d: Int) = primitive(_.setString(a, b, c, d))
    override def truncate(a: Long) = primitive(_.truncate(a))

  }

  trait BlobInterpreter extends BlobOp.Visitor[Kleisli[M, Blob, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](f: Blob => A): Kleisli[M, Blob, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Blob, A] = outer.embed(e)
    override def raiseError[A](e: Throwable): Kleisli[M, Blob, A] = outer.raiseError(e)
    override def monotonic: Kleisli[M, Blob, FiniteDuration] = outer.monotonic[Blob]
    override def realTime: Kleisli[M, Blob, FiniteDuration] = outer.realTime[Blob]
    override def delay[A](thunk: => A): Kleisli[M, Blob, A] = outer.delay(thunk)
    override def suspend[A](hint: Sync.Type)(thunk: => A): Kleisli[M, Blob, A] = outer.suspend(hint)(thunk)
    override def canceled: Kleisli[M, Blob, Unit] = outer.canceled[Blob]

    override def performLogging(event: LogEvent): Kleisli[M, Blob, Unit] = Kleisli(_ => logHandler.run(event))

    // for operations using BlobIO we must call ourself recursively
    override def handleErrorWith[A](fa: BlobIO[A])(f: Throwable => BlobIO[A]): Kleisli[M, Blob, A] = outer.handleErrorWith(this)(fa)(f)
    override def forceR[A, B](fa: BlobIO[A])(fb: BlobIO[B]): Kleisli[M, Blob, B] = outer.forceR(this)(fa)(fb)
    override def uncancelable[A](body: Poll[BlobIO] => BlobIO[A]): Kleisli[M, Blob, A] = outer.uncancelable(this, doobie.free.blob.capturePoll)(body)
    override def poll[A](poll: Any, fa: BlobIO[A]): Kleisli[M, Blob, A] = outer.poll(this)(poll, fa)
    override def onCancel[A](fa: BlobIO[A], fin: BlobIO[Unit]): Kleisli[M, Blob, A] = outer.onCancel(this)(fa, fin)
    override def fromFuture[A](fut: BlobIO[Future[A]]): Kleisli[M, Blob, A] = outer.fromFuture(this)(fut)
    override def fromFutureCancelable[A](fut: BlobIO[(Future[A], BlobIO[Unit])]): Kleisli[M, Blob, A] = outer.fromFutureCancelable(this)(fut)
    override def cancelable[A](fa: BlobIO[A], fin: BlobIO[Unit]): Kleisli[M, Blob, A] = outer.cancelable(this)(fa, fin)


    // domain-specific operations are implemented in terms of `primitive`
    override def free: Kleisli[M, Blob, Unit] = primitive(_.free)
    override def getBinaryStream: Kleisli[M, Blob, InputStream] = primitive(_.getBinaryStream)
    override def getBinaryStream(a: Long, b: Long) = primitive(_.getBinaryStream(a, b))
    override def getBytes(a: Long, b: Int) = primitive(_.getBytes(a, b))
    override def length: Kleisli[M, Blob, Long] = primitive(_.length)
    override def position(a: Array[Byte], b: Long) = primitive(_.position(a, b))
    override def position(a: Blob, b: Long) = primitive(_.position(a, b))
    override def setBinaryStream(a: Long) = primitive(_.setBinaryStream(a))
    override def setBytes(a: Long, b: Array[Byte]) = primitive(_.setBytes(a, b))
    override def setBytes(a: Long, b: Array[Byte], c: Int, d: Int) = primitive(_.setBytes(a, b, c, d))
    override def truncate(a: Long) = primitive(_.truncate(a))

  }

  trait ClobInterpreter extends ClobOp.Visitor[Kleisli[M, Clob, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](f: Clob => A): Kleisli[M, Clob, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Clob, A] = outer.embed(e)
    override def raiseError[A](e: Throwable): Kleisli[M, Clob, A] = outer.raiseError(e)
    override def monotonic: Kleisli[M, Clob, FiniteDuration] = outer.monotonic[Clob]
    override def realTime: Kleisli[M, Clob, FiniteDuration] = outer.realTime[Clob]
    override def delay[A](thunk: => A): Kleisli[M, Clob, A] = outer.delay(thunk)
    override def suspend[A](hint: Sync.Type)(thunk: => A): Kleisli[M, Clob, A] = outer.suspend(hint)(thunk)
    override def canceled: Kleisli[M, Clob, Unit] = outer.canceled[Clob]

    override def performLogging(event: LogEvent): Kleisli[M, Clob, Unit] = Kleisli(_ => logHandler.run(event))

    // for operations using ClobIO we must call ourself recursively
    override def handleErrorWith[A](fa: ClobIO[A])(f: Throwable => ClobIO[A]): Kleisli[M, Clob, A] = outer.handleErrorWith(this)(fa)(f)
    override def forceR[A, B](fa: ClobIO[A])(fb: ClobIO[B]): Kleisli[M, Clob, B] = outer.forceR(this)(fa)(fb)
    override def uncancelable[A](body: Poll[ClobIO] => ClobIO[A]): Kleisli[M, Clob, A] = outer.uncancelable(this, doobie.free.clob.capturePoll)(body)
    override def poll[A](poll: Any, fa: ClobIO[A]): Kleisli[M, Clob, A] = outer.poll(this)(poll, fa)
    override def onCancel[A](fa: ClobIO[A], fin: ClobIO[Unit]): Kleisli[M, Clob, A] = outer.onCancel(this)(fa, fin)
    override def fromFuture[A](fut: ClobIO[Future[A]]): Kleisli[M, Clob, A] = outer.fromFuture(this)(fut)
    override def fromFutureCancelable[A](fut: ClobIO[(Future[A], ClobIO[Unit])]): Kleisli[M, Clob, A] = outer.fromFutureCancelable(this)(fut)
    override def cancelable[A](fa: ClobIO[A], fin: ClobIO[Unit]): Kleisli[M, Clob, A] = outer.cancelable(this)(fa, fin)


    // domain-specific operations are implemented in terms of `primitive`
    override def free: Kleisli[M, Clob, Unit] = primitive(_.free)
    override def getAsciiStream: Kleisli[M, Clob, InputStream] = primitive(_.getAsciiStream)
    override def getCharacterStream: Kleisli[M, Clob, Reader] = primitive(_.getCharacterStream)
    override def getCharacterStream(a: Long, b: Long) = primitive(_.getCharacterStream(a, b))
    override def getSubString(a: Long, b: Int) = primitive(_.getSubString(a, b))
    override def length: Kleisli[M, Clob, Long] = primitive(_.length)
    override def position(a: Clob, b: Long) = primitive(_.position(a, b))
    override def position(a: String, b: Long) = primitive(_.position(a, b))
    override def setAsciiStream(a: Long) = primitive(_.setAsciiStream(a))
    override def setCharacterStream(a: Long) = primitive(_.setCharacterStream(a))
    override def setString(a: Long, b: String) = primitive(_.setString(a, b))
    override def setString(a: Long, b: String, c: Int, d: Int) = primitive(_.setString(a, b, c, d))
    override def truncate(a: Long) = primitive(_.truncate(a))

  }

  trait DatabaseMetaDataInterpreter extends DatabaseMetaDataOp.Visitor[Kleisli[M, DatabaseMetaData, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](f: DatabaseMetaData => A): Kleisli[M, DatabaseMetaData, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, DatabaseMetaData, A] = outer.embed(e)
    override def raiseError[A](e: Throwable): Kleisli[M, DatabaseMetaData, A] = outer.raiseError(e)
    override def monotonic: Kleisli[M, DatabaseMetaData, FiniteDuration] = outer.monotonic[DatabaseMetaData]
    override def realTime: Kleisli[M, DatabaseMetaData, FiniteDuration] = outer.realTime[DatabaseMetaData]
    override def delay[A](thunk: => A): Kleisli[M, DatabaseMetaData, A] = outer.delay(thunk)
    override def suspend[A](hint: Sync.Type)(thunk: => A): Kleisli[M, DatabaseMetaData, A] = outer.suspend(hint)(thunk)
    override def canceled: Kleisli[M, DatabaseMetaData, Unit] = outer.canceled[DatabaseMetaData]

    override def performLogging(event: LogEvent): Kleisli[M, DatabaseMetaData, Unit] = Kleisli(_ => logHandler.run(event))

    // for operations using DatabaseMetaDataIO we must call ourself recursively
    override def handleErrorWith[A](fa: DatabaseMetaDataIO[A])(f: Throwable => DatabaseMetaDataIO[A]): Kleisli[M, DatabaseMetaData, A] = outer.handleErrorWith(this)(fa)(f)
    override def forceR[A, B](fa: DatabaseMetaDataIO[A])(fb: DatabaseMetaDataIO[B]): Kleisli[M, DatabaseMetaData, B] = outer.forceR(this)(fa)(fb)
    override def uncancelable[A](body: Poll[DatabaseMetaDataIO] => DatabaseMetaDataIO[A]): Kleisli[M, DatabaseMetaData, A] = outer.uncancelable(this, doobie.free.databasemetadata.capturePoll)(body)
    override def poll[A](poll: Any, fa: DatabaseMetaDataIO[A]): Kleisli[M, DatabaseMetaData, A] = outer.poll(this)(poll, fa)
    override def onCancel[A](fa: DatabaseMetaDataIO[A], fin: DatabaseMetaDataIO[Unit]): Kleisli[M, DatabaseMetaData, A] = outer.onCancel(this)(fa, fin)
    override def fromFuture[A](fut: DatabaseMetaDataIO[Future[A]]): Kleisli[M, DatabaseMetaData, A] = outer.fromFuture(this)(fut)
    override def fromFutureCancelable[A](fut: DatabaseMetaDataIO[(Future[A], DatabaseMetaDataIO[Unit])]): Kleisli[M, DatabaseMetaData, A] = outer.fromFutureCancelable(this)(fut)
    override def cancelable[A](fa: DatabaseMetaDataIO[A], fin: DatabaseMetaDataIO[Unit]): Kleisli[M, DatabaseMetaData, A] = outer.cancelable(this)(fa, fin)


    // domain-specific operations are implemented in terms of `primitive`
    override def allProceduresAreCallable: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.allProceduresAreCallable)
    override def allTablesAreSelectable: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.allTablesAreSelectable)
    override def autoCommitFailureClosesAllResultSets: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.autoCommitFailureClosesAllResultSets)
    override def dataDefinitionCausesTransactionCommit: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.dataDefinitionCausesTransactionCommit)
    override def dataDefinitionIgnoredInTransactions: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.dataDefinitionIgnoredInTransactions)
    override def deletesAreDetected(a: Int) = primitive(_.deletesAreDetected(a))
    override def doesMaxRowSizeIncludeBlobs: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.doesMaxRowSizeIncludeBlobs)
    override def generatedKeyAlwaysReturned: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.generatedKeyAlwaysReturned)
    override def getAttributes(a: String, b: String, c: String, d: String) = primitive(_.getAttributes(a, b, c, d))
    override def getBestRowIdentifier(a: String, b: String, c: String, d: Int, e: Boolean) = primitive(_.getBestRowIdentifier(a, b, c, d, e))
    override def getCatalogSeparator: Kleisli[M, DatabaseMetaData, String] = primitive(_.getCatalogSeparator)
    override def getCatalogTerm: Kleisli[M, DatabaseMetaData, String] = primitive(_.getCatalogTerm)
    override def getCatalogs: Kleisli[M, DatabaseMetaData, ResultSet] = primitive(_.getCatalogs)
    override def getClientInfoProperties: Kleisli[M, DatabaseMetaData, ResultSet] = primitive(_.getClientInfoProperties)
    override def getColumnPrivileges(a: String, b: String, c: String, d: String) = primitive(_.getColumnPrivileges(a, b, c, d))
    override def getColumns(a: String, b: String, c: String, d: String) = primitive(_.getColumns(a, b, c, d))
    override def getConnection: Kleisli[M, DatabaseMetaData, Connection] = primitive(_.getConnection)
    override def getCrossReference(a: String, b: String, c: String, d: String, e: String, f: String) = primitive(_.getCrossReference(a, b, c, d, e, f))
    override def getDatabaseMajorVersion: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getDatabaseMajorVersion)
    override def getDatabaseMinorVersion: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getDatabaseMinorVersion)
    override def getDatabaseProductName: Kleisli[M, DatabaseMetaData, String] = primitive(_.getDatabaseProductName)
    override def getDatabaseProductVersion: Kleisli[M, DatabaseMetaData, String] = primitive(_.getDatabaseProductVersion)
    override def getDefaultTransactionIsolation: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getDefaultTransactionIsolation)
    override def getDriverMajorVersion: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getDriverMajorVersion)
    override def getDriverMinorVersion: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getDriverMinorVersion)
    override def getDriverName: Kleisli[M, DatabaseMetaData, String] = primitive(_.getDriverName)
    override def getDriverVersion: Kleisli[M, DatabaseMetaData, String] = primitive(_.getDriverVersion)
    override def getExportedKeys(a: String, b: String, c: String) = primitive(_.getExportedKeys(a, b, c))
    override def getExtraNameCharacters: Kleisli[M, DatabaseMetaData, String] = primitive(_.getExtraNameCharacters)
    override def getFunctionColumns(a: String, b: String, c: String, d: String) = primitive(_.getFunctionColumns(a, b, c, d))
    override def getFunctions(a: String, b: String, c: String) = primitive(_.getFunctions(a, b, c))
    override def getIdentifierQuoteString: Kleisli[M, DatabaseMetaData, String] = primitive(_.getIdentifierQuoteString)
    override def getImportedKeys(a: String, b: String, c: String) = primitive(_.getImportedKeys(a, b, c))
    override def getIndexInfo(a: String, b: String, c: String, d: Boolean, e: Boolean) = primitive(_.getIndexInfo(a, b, c, d, e))
    override def getJDBCMajorVersion: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getJDBCMajorVersion)
    override def getJDBCMinorVersion: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getJDBCMinorVersion)
    override def getMaxBinaryLiteralLength: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getMaxBinaryLiteralLength)
    override def getMaxCatalogNameLength: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getMaxCatalogNameLength)
    override def getMaxCharLiteralLength: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getMaxCharLiteralLength)
    override def getMaxColumnNameLength: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getMaxColumnNameLength)
    override def getMaxColumnsInGroupBy: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getMaxColumnsInGroupBy)
    override def getMaxColumnsInIndex: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getMaxColumnsInIndex)
    override def getMaxColumnsInOrderBy: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getMaxColumnsInOrderBy)
    override def getMaxColumnsInSelect: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getMaxColumnsInSelect)
    override def getMaxColumnsInTable: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getMaxColumnsInTable)
    override def getMaxConnections: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getMaxConnections)
    override def getMaxCursorNameLength: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getMaxCursorNameLength)
    override def getMaxIndexLength: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getMaxIndexLength)
    override def getMaxLogicalLobSize: Kleisli[M, DatabaseMetaData, Long] = primitive(_.getMaxLogicalLobSize)
    override def getMaxProcedureNameLength: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getMaxProcedureNameLength)
    override def getMaxRowSize: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getMaxRowSize)
    override def getMaxSchemaNameLength: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getMaxSchemaNameLength)
    override def getMaxStatementLength: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getMaxStatementLength)
    override def getMaxStatements: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getMaxStatements)
    override def getMaxTableNameLength: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getMaxTableNameLength)
    override def getMaxTablesInSelect: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getMaxTablesInSelect)
    override def getMaxUserNameLength: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getMaxUserNameLength)
    override def getNumericFunctions: Kleisli[M, DatabaseMetaData, String] = primitive(_.getNumericFunctions)
    override def getPrimaryKeys(a: String, b: String, c: String) = primitive(_.getPrimaryKeys(a, b, c))
    override def getProcedureColumns(a: String, b: String, c: String, d: String) = primitive(_.getProcedureColumns(a, b, c, d))
    override def getProcedureTerm: Kleisli[M, DatabaseMetaData, String] = primitive(_.getProcedureTerm)
    override def getProcedures(a: String, b: String, c: String) = primitive(_.getProcedures(a, b, c))
    override def getPseudoColumns(a: String, b: String, c: String, d: String) = primitive(_.getPseudoColumns(a, b, c, d))
    override def getResultSetHoldability: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getResultSetHoldability)
    override def getRowIdLifetime: Kleisli[M, DatabaseMetaData, RowIdLifetime] = primitive(_.getRowIdLifetime)
    override def getSQLKeywords: Kleisli[M, DatabaseMetaData, String] = primitive(_.getSQLKeywords)
    override def getSQLStateType: Kleisli[M, DatabaseMetaData, Int] = primitive(_.getSQLStateType)
    override def getSchemaTerm: Kleisli[M, DatabaseMetaData, String] = primitive(_.getSchemaTerm)
    override def getSchemas: Kleisli[M, DatabaseMetaData, ResultSet] = primitive(_.getSchemas)
    override def getSchemas(a: String, b: String) = primitive(_.getSchemas(a, b))
    override def getSearchStringEscape: Kleisli[M, DatabaseMetaData, String] = primitive(_.getSearchStringEscape)
    override def getStringFunctions: Kleisli[M, DatabaseMetaData, String] = primitive(_.getStringFunctions)
    override def getSuperTables(a: String, b: String, c: String) = primitive(_.getSuperTables(a, b, c))
    override def getSuperTypes(a: String, b: String, c: String) = primitive(_.getSuperTypes(a, b, c))
    override def getSystemFunctions: Kleisli[M, DatabaseMetaData, String] = primitive(_.getSystemFunctions)
    override def getTablePrivileges(a: String, b: String, c: String) = primitive(_.getTablePrivileges(a, b, c))
    override def getTableTypes: Kleisli[M, DatabaseMetaData, ResultSet] = primitive(_.getTableTypes)
    override def getTables(a: String, b: String, c: String, d: Array[String]) = primitive(_.getTables(a, b, c, d))
    override def getTimeDateFunctions: Kleisli[M, DatabaseMetaData, String] = primitive(_.getTimeDateFunctions)
    override def getTypeInfo: Kleisli[M, DatabaseMetaData, ResultSet] = primitive(_.getTypeInfo)
    override def getUDTs(a: String, b: String, c: String, d: Array[Int]) = primitive(_.getUDTs(a, b, c, d))
    override def getURL: Kleisli[M, DatabaseMetaData, String] = primitive(_.getURL)
    override def getUserName: Kleisli[M, DatabaseMetaData, String] = primitive(_.getUserName)
    override def getVersionColumns(a: String, b: String, c: String) = primitive(_.getVersionColumns(a, b, c))
    override def insertsAreDetected(a: Int) = primitive(_.insertsAreDetected(a))
    override def isCatalogAtStart: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.isCatalogAtStart)
    override def isReadOnly: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.isReadOnly)
    override def isWrapperFor(a: Class[?]) = primitive(_.isWrapperFor(a))
    override def locatorsUpdateCopy: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.locatorsUpdateCopy)
    override def nullPlusNonNullIsNull: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.nullPlusNonNullIsNull)
    override def nullsAreSortedAtEnd: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.nullsAreSortedAtEnd)
    override def nullsAreSortedAtStart: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.nullsAreSortedAtStart)
    override def nullsAreSortedHigh: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.nullsAreSortedHigh)
    override def nullsAreSortedLow: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.nullsAreSortedLow)
    override def othersDeletesAreVisible(a: Int) = primitive(_.othersDeletesAreVisible(a))
    override def othersInsertsAreVisible(a: Int) = primitive(_.othersInsertsAreVisible(a))
    override def othersUpdatesAreVisible(a: Int) = primitive(_.othersUpdatesAreVisible(a))
    override def ownDeletesAreVisible(a: Int) = primitive(_.ownDeletesAreVisible(a))
    override def ownInsertsAreVisible(a: Int) = primitive(_.ownInsertsAreVisible(a))
    override def ownUpdatesAreVisible(a: Int) = primitive(_.ownUpdatesAreVisible(a))
    override def storesLowerCaseIdentifiers: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.storesLowerCaseIdentifiers)
    override def storesLowerCaseQuotedIdentifiers: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.storesLowerCaseQuotedIdentifiers)
    override def storesMixedCaseIdentifiers: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.storesMixedCaseIdentifiers)
    override def storesMixedCaseQuotedIdentifiers: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.storesMixedCaseQuotedIdentifiers)
    override def storesUpperCaseIdentifiers: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.storesUpperCaseIdentifiers)
    override def storesUpperCaseQuotedIdentifiers: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.storesUpperCaseQuotedIdentifiers)
    override def supportsANSI92EntryLevelSQL: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsANSI92EntryLevelSQL)
    override def supportsANSI92FullSQL: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsANSI92FullSQL)
    override def supportsANSI92IntermediateSQL: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsANSI92IntermediateSQL)
    override def supportsAlterTableWithAddColumn: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsAlterTableWithAddColumn)
    override def supportsAlterTableWithDropColumn: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsAlterTableWithDropColumn)
    override def supportsBatchUpdates: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsBatchUpdates)
    override def supportsCatalogsInDataManipulation: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsCatalogsInDataManipulation)
    override def supportsCatalogsInIndexDefinitions: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsCatalogsInIndexDefinitions)
    override def supportsCatalogsInPrivilegeDefinitions: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsCatalogsInPrivilegeDefinitions)
    override def supportsCatalogsInProcedureCalls: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsCatalogsInProcedureCalls)
    override def supportsCatalogsInTableDefinitions: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsCatalogsInTableDefinitions)
    override def supportsColumnAliasing: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsColumnAliasing)
    override def supportsConvert: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsConvert)
    override def supportsConvert(a: Int, b: Int) = primitive(_.supportsConvert(a, b))
    override def supportsCoreSQLGrammar: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsCoreSQLGrammar)
    override def supportsCorrelatedSubqueries: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsCorrelatedSubqueries)
    override def supportsDataDefinitionAndDataManipulationTransactions: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsDataDefinitionAndDataManipulationTransactions)
    override def supportsDataManipulationTransactionsOnly: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsDataManipulationTransactionsOnly)
    override def supportsDifferentTableCorrelationNames: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsDifferentTableCorrelationNames)
    override def supportsExpressionsInOrderBy: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsExpressionsInOrderBy)
    override def supportsExtendedSQLGrammar: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsExtendedSQLGrammar)
    override def supportsFullOuterJoins: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsFullOuterJoins)
    override def supportsGetGeneratedKeys: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsGetGeneratedKeys)
    override def supportsGroupBy: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsGroupBy)
    override def supportsGroupByBeyondSelect: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsGroupByBeyondSelect)
    override def supportsGroupByUnrelated: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsGroupByUnrelated)
    override def supportsIntegrityEnhancementFacility: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsIntegrityEnhancementFacility)
    override def supportsLikeEscapeClause: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsLikeEscapeClause)
    override def supportsLimitedOuterJoins: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsLimitedOuterJoins)
    override def supportsMinimumSQLGrammar: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsMinimumSQLGrammar)
    override def supportsMixedCaseIdentifiers: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsMixedCaseIdentifiers)
    override def supportsMixedCaseQuotedIdentifiers: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsMixedCaseQuotedIdentifiers)
    override def supportsMultipleOpenResults: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsMultipleOpenResults)
    override def supportsMultipleResultSets: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsMultipleResultSets)
    override def supportsMultipleTransactions: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsMultipleTransactions)
    override def supportsNamedParameters: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsNamedParameters)
    override def supportsNonNullableColumns: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsNonNullableColumns)
    override def supportsOpenCursorsAcrossCommit: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsOpenCursorsAcrossCommit)
    override def supportsOpenCursorsAcrossRollback: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsOpenCursorsAcrossRollback)
    override def supportsOpenStatementsAcrossCommit: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsOpenStatementsAcrossCommit)
    override def supportsOpenStatementsAcrossRollback: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsOpenStatementsAcrossRollback)
    override def supportsOrderByUnrelated: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsOrderByUnrelated)
    override def supportsOuterJoins: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsOuterJoins)
    override def supportsPositionedDelete: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsPositionedDelete)
    override def supportsPositionedUpdate: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsPositionedUpdate)
    override def supportsRefCursors: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsRefCursors)
    override def supportsResultSetConcurrency(a: Int, b: Int) = primitive(_.supportsResultSetConcurrency(a, b))
    override def supportsResultSetHoldability(a: Int) = primitive(_.supportsResultSetHoldability(a))
    override def supportsResultSetType(a: Int) = primitive(_.supportsResultSetType(a))
    override def supportsSavepoints: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsSavepoints)
    override def supportsSchemasInDataManipulation: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsSchemasInDataManipulation)
    override def supportsSchemasInIndexDefinitions: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsSchemasInIndexDefinitions)
    override def supportsSchemasInPrivilegeDefinitions: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsSchemasInPrivilegeDefinitions)
    override def supportsSchemasInProcedureCalls: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsSchemasInProcedureCalls)
    override def supportsSchemasInTableDefinitions: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsSchemasInTableDefinitions)
    override def supportsSelectForUpdate: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsSelectForUpdate)
    override def supportsSharding: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsSharding)
    override def supportsStatementPooling: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsStatementPooling)
    override def supportsStoredFunctionsUsingCallSyntax: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsStoredFunctionsUsingCallSyntax)
    override def supportsStoredProcedures: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsStoredProcedures)
    override def supportsSubqueriesInComparisons: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsSubqueriesInComparisons)
    override def supportsSubqueriesInExists: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsSubqueriesInExists)
    override def supportsSubqueriesInIns: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsSubqueriesInIns)
    override def supportsSubqueriesInQuantifieds: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsSubqueriesInQuantifieds)
    override def supportsTableCorrelationNames: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsTableCorrelationNames)
    override def supportsTransactionIsolationLevel(a: Int) = primitive(_.supportsTransactionIsolationLevel(a))
    override def supportsTransactions: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsTransactions)
    override def supportsUnion: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsUnion)
    override def supportsUnionAll: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.supportsUnionAll)
    override def unwrap[T](a: Class[T]) = primitive(_.unwrap(a))
    override def updatesAreDetected(a: Int) = primitive(_.updatesAreDetected(a))
    override def usesLocalFilePerTable: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.usesLocalFilePerTable)
    override def usesLocalFiles: Kleisli[M, DatabaseMetaData, Boolean] = primitive(_.usesLocalFiles)

  }

  trait DriverInterpreter extends DriverOp.Visitor[Kleisli[M, Driver, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](f: Driver => A): Kleisli[M, Driver, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Driver, A] = outer.embed(e)
    override def raiseError[A](e: Throwable): Kleisli[M, Driver, A] = outer.raiseError(e)
    override def monotonic: Kleisli[M, Driver, FiniteDuration] = outer.monotonic[Driver]
    override def realTime: Kleisli[M, Driver, FiniteDuration] = outer.realTime[Driver]
    override def delay[A](thunk: => A): Kleisli[M, Driver, A] = outer.delay(thunk)
    override def suspend[A](hint: Sync.Type)(thunk: => A): Kleisli[M, Driver, A] = outer.suspend(hint)(thunk)
    override def canceled: Kleisli[M, Driver, Unit] = outer.canceled[Driver]

    override def performLogging(event: LogEvent): Kleisli[M, Driver, Unit] = Kleisli(_ => logHandler.run(event))

    // for operations using DriverIO we must call ourself recursively
    override def handleErrorWith[A](fa: DriverIO[A])(f: Throwable => DriverIO[A]): Kleisli[M, Driver, A] = outer.handleErrorWith(this)(fa)(f)
    override def forceR[A, B](fa: DriverIO[A])(fb: DriverIO[B]): Kleisli[M, Driver, B] = outer.forceR(this)(fa)(fb)
    override def uncancelable[A](body: Poll[DriverIO] => DriverIO[A]): Kleisli[M, Driver, A] = outer.uncancelable(this, doobie.free.driver.capturePoll)(body)
    override def poll[A](poll: Any, fa: DriverIO[A]): Kleisli[M, Driver, A] = outer.poll(this)(poll, fa)
    override def onCancel[A](fa: DriverIO[A], fin: DriverIO[Unit]): Kleisli[M, Driver, A] = outer.onCancel(this)(fa, fin)
    override def fromFuture[A](fut: DriverIO[Future[A]]): Kleisli[M, Driver, A] = outer.fromFuture(this)(fut)
    override def fromFutureCancelable[A](fut: DriverIO[(Future[A], DriverIO[Unit])]): Kleisli[M, Driver, A] = outer.fromFutureCancelable(this)(fut)
    override def cancelable[A](fa: DriverIO[A], fin: DriverIO[Unit]): Kleisli[M, Driver, A] = outer.cancelable(this)(fa, fin)


    // domain-specific operations are implemented in terms of `primitive`
    override def acceptsURL(a: String) = primitive(_.acceptsURL(a))
    override def connect(a: String, b: Properties) = primitive(_.connect(a, b))
    override def getMajorVersion: Kleisli[M, Driver, Int] = primitive(_.getMajorVersion)
    override def getMinorVersion: Kleisli[M, Driver, Int] = primitive(_.getMinorVersion)
    override def getParentLogger: Kleisli[M, Driver, Logger] = primitive(_.getParentLogger)
    override def getPropertyInfo(a: String, b: Properties) = primitive(_.getPropertyInfo(a, b))
    override def jdbcCompliant: Kleisli[M, Driver, Boolean] = primitive(_.jdbcCompliant)

  }

  trait RefInterpreter extends RefOp.Visitor[Kleisli[M, Ref, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](f: Ref => A): Kleisli[M, Ref, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Ref, A] = outer.embed(e)
    override def raiseError[A](e: Throwable): Kleisli[M, Ref, A] = outer.raiseError(e)
    override def monotonic: Kleisli[M, Ref, FiniteDuration] = outer.monotonic[Ref]
    override def realTime: Kleisli[M, Ref, FiniteDuration] = outer.realTime[Ref]
    override def delay[A](thunk: => A): Kleisli[M, Ref, A] = outer.delay(thunk)
    override def suspend[A](hint: Sync.Type)(thunk: => A): Kleisli[M, Ref, A] = outer.suspend(hint)(thunk)
    override def canceled: Kleisli[M, Ref, Unit] = outer.canceled[Ref]

    override def performLogging(event: LogEvent): Kleisli[M, Ref, Unit] = Kleisli(_ => logHandler.run(event))

    // for operations using RefIO we must call ourself recursively
    override def handleErrorWith[A](fa: RefIO[A])(f: Throwable => RefIO[A]): Kleisli[M, Ref, A] = outer.handleErrorWith(this)(fa)(f)
    override def forceR[A, B](fa: RefIO[A])(fb: RefIO[B]): Kleisli[M, Ref, B] = outer.forceR(this)(fa)(fb)
    override def uncancelable[A](body: Poll[RefIO] => RefIO[A]): Kleisli[M, Ref, A] = outer.uncancelable(this, doobie.free.ref.capturePoll)(body)
    override def poll[A](poll: Any, fa: RefIO[A]): Kleisli[M, Ref, A] = outer.poll(this)(poll, fa)
    override def onCancel[A](fa: RefIO[A], fin: RefIO[Unit]): Kleisli[M, Ref, A] = outer.onCancel(this)(fa, fin)
    override def fromFuture[A](fut: RefIO[Future[A]]): Kleisli[M, Ref, A] = outer.fromFuture(this)(fut)
    override def fromFutureCancelable[A](fut: RefIO[(Future[A], RefIO[Unit])]): Kleisli[M, Ref, A] = outer.fromFutureCancelable(this)(fut)
    override def cancelable[A](fa: RefIO[A], fin: RefIO[Unit]): Kleisli[M, Ref, A] = outer.cancelable(this)(fa, fin)


    // domain-specific operations are implemented in terms of `primitive`
    override def getBaseTypeName: Kleisli[M, Ref, String] = primitive(_.getBaseTypeName)
    override def getObject: Kleisli[M, Ref, AnyRef] = primitive(_.getObject)
    override def getObject(a: java.util.Map[String, Class[?]]) = primitive(_.getObject(a))
    override def setObject(a: AnyRef) = primitive(_.setObject(a))

  }

  trait SQLDataInterpreter extends SQLDataOp.Visitor[Kleisli[M, SQLData, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](f: SQLData => A): Kleisli[M, SQLData, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, SQLData, A] = outer.embed(e)
    override def raiseError[A](e: Throwable): Kleisli[M, SQLData, A] = outer.raiseError(e)
    override def monotonic: Kleisli[M, SQLData, FiniteDuration] = outer.monotonic[SQLData]
    override def realTime: Kleisli[M, SQLData, FiniteDuration] = outer.realTime[SQLData]
    override def delay[A](thunk: => A): Kleisli[M, SQLData, A] = outer.delay(thunk)
    override def suspend[A](hint: Sync.Type)(thunk: => A): Kleisli[M, SQLData, A] = outer.suspend(hint)(thunk)
    override def canceled: Kleisli[M, SQLData, Unit] = outer.canceled[SQLData]

    override def performLogging(event: LogEvent): Kleisli[M, SQLData, Unit] = Kleisli(_ => logHandler.run(event))

    // for operations using SQLDataIO we must call ourself recursively
    override def handleErrorWith[A](fa: SQLDataIO[A])(f: Throwable => SQLDataIO[A]): Kleisli[M, SQLData, A] = outer.handleErrorWith(this)(fa)(f)
    override def forceR[A, B](fa: SQLDataIO[A])(fb: SQLDataIO[B]): Kleisli[M, SQLData, B] = outer.forceR(this)(fa)(fb)
    override def uncancelable[A](body: Poll[SQLDataIO] => SQLDataIO[A]): Kleisli[M, SQLData, A] = outer.uncancelable(this, doobie.free.sqldata.capturePoll)(body)
    override def poll[A](poll: Any, fa: SQLDataIO[A]): Kleisli[M, SQLData, A] = outer.poll(this)(poll, fa)
    override def onCancel[A](fa: SQLDataIO[A], fin: SQLDataIO[Unit]): Kleisli[M, SQLData, A] = outer.onCancel(this)(fa, fin)
    override def fromFuture[A](fut: SQLDataIO[Future[A]]): Kleisli[M, SQLData, A] = outer.fromFuture(this)(fut)
    override def fromFutureCancelable[A](fut: SQLDataIO[(Future[A], SQLDataIO[Unit])]): Kleisli[M, SQLData, A] = outer.fromFutureCancelable(this)(fut)
    override def cancelable[A](fa: SQLDataIO[A], fin: SQLDataIO[Unit]): Kleisli[M, SQLData, A] = outer.cancelable(this)(fa, fin)


    // domain-specific operations are implemented in terms of `primitive`
    override def getSQLTypeName: Kleisli[M, SQLData, String] = primitive(_.getSQLTypeName)
    override def readSQL(a: SQLInput, b: String) = primitive(_.readSQL(a, b))
    override def writeSQL(a: SQLOutput) = primitive(_.writeSQL(a))

  }

  trait SQLInputInterpreter extends SQLInputOp.Visitor[Kleisli[M, SQLInput, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](f: SQLInput => A): Kleisli[M, SQLInput, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, SQLInput, A] = outer.embed(e)
    override def raiseError[A](e: Throwable): Kleisli[M, SQLInput, A] = outer.raiseError(e)
    override def monotonic: Kleisli[M, SQLInput, FiniteDuration] = outer.monotonic[SQLInput]
    override def realTime: Kleisli[M, SQLInput, FiniteDuration] = outer.realTime[SQLInput]
    override def delay[A](thunk: => A): Kleisli[M, SQLInput, A] = outer.delay(thunk)
    override def suspend[A](hint: Sync.Type)(thunk: => A): Kleisli[M, SQLInput, A] = outer.suspend(hint)(thunk)
    override def canceled: Kleisli[M, SQLInput, Unit] = outer.canceled[SQLInput]

    override def performLogging(event: LogEvent): Kleisli[M, SQLInput, Unit] = Kleisli(_ => logHandler.run(event))

    // for operations using SQLInputIO we must call ourself recursively
    override def handleErrorWith[A](fa: SQLInputIO[A])(f: Throwable => SQLInputIO[A]): Kleisli[M, SQLInput, A] = outer.handleErrorWith(this)(fa)(f)
    override def forceR[A, B](fa: SQLInputIO[A])(fb: SQLInputIO[B]): Kleisli[M, SQLInput, B] = outer.forceR(this)(fa)(fb)
    override def uncancelable[A](body: Poll[SQLInputIO] => SQLInputIO[A]): Kleisli[M, SQLInput, A] = outer.uncancelable(this, doobie.free.sqlinput.capturePoll)(body)
    override def poll[A](poll: Any, fa: SQLInputIO[A]): Kleisli[M, SQLInput, A] = outer.poll(this)(poll, fa)
    override def onCancel[A](fa: SQLInputIO[A], fin: SQLInputIO[Unit]): Kleisli[M, SQLInput, A] = outer.onCancel(this)(fa, fin)
    override def fromFuture[A](fut: SQLInputIO[Future[A]]): Kleisli[M, SQLInput, A] = outer.fromFuture(this)(fut)
    override def fromFutureCancelable[A](fut: SQLInputIO[(Future[A], SQLInputIO[Unit])]): Kleisli[M, SQLInput, A] = outer.fromFutureCancelable(this)(fut)
    override def cancelable[A](fa: SQLInputIO[A], fin: SQLInputIO[Unit]): Kleisli[M, SQLInput, A] = outer.cancelable(this)(fa, fin)


    // domain-specific operations are implemented in terms of `primitive`
    override def readArray: Kleisli[M, SQLInput, SqlArray] = primitive(_.readArray)
    override def readAsciiStream: Kleisli[M, SQLInput, InputStream] = primitive(_.readAsciiStream)
    override def readBigDecimal: Kleisli[M, SQLInput, BigDecimal] = primitive(_.readBigDecimal)
    override def readBinaryStream: Kleisli[M, SQLInput, InputStream] = primitive(_.readBinaryStream)
    override def readBlob: Kleisli[M, SQLInput, Blob] = primitive(_.readBlob)
    override def readBoolean: Kleisli[M, SQLInput, Boolean] = primitive(_.readBoolean)
    override def readByte: Kleisli[M, SQLInput, Byte] = primitive(_.readByte)
    override def readBytes: Kleisli[M, SQLInput, Array[Byte]] = primitive(_.readBytes)
    override def readCharacterStream: Kleisli[M, SQLInput, Reader] = primitive(_.readCharacterStream)
    override def readClob: Kleisli[M, SQLInput, Clob] = primitive(_.readClob)
    override def readDate: Kleisli[M, SQLInput, Date] = primitive(_.readDate)
    override def readDouble: Kleisli[M, SQLInput, Double] = primitive(_.readDouble)
    override def readFloat: Kleisli[M, SQLInput, Float] = primitive(_.readFloat)
    override def readInt: Kleisli[M, SQLInput, Int] = primitive(_.readInt)
    override def readLong: Kleisli[M, SQLInput, Long] = primitive(_.readLong)
    override def readNClob: Kleisli[M, SQLInput, NClob] = primitive(_.readNClob)
    override def readNString: Kleisli[M, SQLInput, String] = primitive(_.readNString)
    override def readObject: Kleisli[M, SQLInput, AnyRef] = primitive(_.readObject)
    override def readObject[T](a: Class[T]) = primitive(_.readObject(a))
    override def readRef: Kleisli[M, SQLInput, Ref] = primitive(_.readRef)
    override def readRowId: Kleisli[M, SQLInput, RowId] = primitive(_.readRowId)
    override def readSQLXML: Kleisli[M, SQLInput, SQLXML] = primitive(_.readSQLXML)
    override def readShort: Kleisli[M, SQLInput, Short] = primitive(_.readShort)
    override def readString: Kleisli[M, SQLInput, String] = primitive(_.readString)
    override def readTime: Kleisli[M, SQLInput, Time] = primitive(_.readTime)
    override def readTimestamp: Kleisli[M, SQLInput, Timestamp] = primitive(_.readTimestamp)
    override def readURL: Kleisli[M, SQLInput, URL] = primitive(_.readURL)
    override def wasNull: Kleisli[M, SQLInput, Boolean] = primitive(_.wasNull)

  }

  trait SQLOutputInterpreter extends SQLOutputOp.Visitor[Kleisli[M, SQLOutput, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](f: SQLOutput => A): Kleisli[M, SQLOutput, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, SQLOutput, A] = outer.embed(e)
    override def raiseError[A](e: Throwable): Kleisli[M, SQLOutput, A] = outer.raiseError(e)
    override def monotonic: Kleisli[M, SQLOutput, FiniteDuration] = outer.monotonic[SQLOutput]
    override def realTime: Kleisli[M, SQLOutput, FiniteDuration] = outer.realTime[SQLOutput]
    override def delay[A](thunk: => A): Kleisli[M, SQLOutput, A] = outer.delay(thunk)
    override def suspend[A](hint: Sync.Type)(thunk: => A): Kleisli[M, SQLOutput, A] = outer.suspend(hint)(thunk)
    override def canceled: Kleisli[M, SQLOutput, Unit] = outer.canceled[SQLOutput]

    override def performLogging(event: LogEvent): Kleisli[M, SQLOutput, Unit] = Kleisli(_ => logHandler.run(event))

    // for operations using SQLOutputIO we must call ourself recursively
    override def handleErrorWith[A](fa: SQLOutputIO[A])(f: Throwable => SQLOutputIO[A]): Kleisli[M, SQLOutput, A] = outer.handleErrorWith(this)(fa)(f)
    override def forceR[A, B](fa: SQLOutputIO[A])(fb: SQLOutputIO[B]): Kleisli[M, SQLOutput, B] = outer.forceR(this)(fa)(fb)
    override def uncancelable[A](body: Poll[SQLOutputIO] => SQLOutputIO[A]): Kleisli[M, SQLOutput, A] = outer.uncancelable(this, doobie.free.sqloutput.capturePoll)(body)
    override def poll[A](poll: Any, fa: SQLOutputIO[A]): Kleisli[M, SQLOutput, A] = outer.poll(this)(poll, fa)
    override def onCancel[A](fa: SQLOutputIO[A], fin: SQLOutputIO[Unit]): Kleisli[M, SQLOutput, A] = outer.onCancel(this)(fa, fin)
    override def fromFuture[A](fut: SQLOutputIO[Future[A]]): Kleisli[M, SQLOutput, A] = outer.fromFuture(this)(fut)
    override def fromFutureCancelable[A](fut: SQLOutputIO[(Future[A], SQLOutputIO[Unit])]): Kleisli[M, SQLOutput, A] = outer.fromFutureCancelable(this)(fut)
    override def cancelable[A](fa: SQLOutputIO[A], fin: SQLOutputIO[Unit]): Kleisli[M, SQLOutput, A] = outer.cancelable(this)(fa, fin)


    // domain-specific operations are implemented in terms of `primitive`
    override def writeArray(a: SqlArray) = primitive(_.writeArray(a))
    override def writeAsciiStream(a: InputStream) = primitive(_.writeAsciiStream(a))
    override def writeBigDecimal(a: BigDecimal) = primitive(_.writeBigDecimal(a))
    override def writeBinaryStream(a: InputStream) = primitive(_.writeBinaryStream(a))
    override def writeBlob(a: Blob) = primitive(_.writeBlob(a))
    override def writeBoolean(a: Boolean) = primitive(_.writeBoolean(a))
    override def writeByte(a: Byte) = primitive(_.writeByte(a))
    override def writeBytes(a: Array[Byte]) = primitive(_.writeBytes(a))
    override def writeCharacterStream(a: Reader) = primitive(_.writeCharacterStream(a))
    override def writeClob(a: Clob) = primitive(_.writeClob(a))
    override def writeDate(a: Date) = primitive(_.writeDate(a))
    override def writeDouble(a: Double) = primitive(_.writeDouble(a))
    override def writeFloat(a: Float) = primitive(_.writeFloat(a))
    override def writeInt(a: Int) = primitive(_.writeInt(a))
    override def writeLong(a: Long) = primitive(_.writeLong(a))
    override def writeNClob(a: NClob) = primitive(_.writeNClob(a))
    override def writeNString(a: String) = primitive(_.writeNString(a))
    override def writeObject(a: AnyRef, b: SQLType) = primitive(_.writeObject(a, b))
    override def writeObject(a: SQLData) = primitive(_.writeObject(a))
    override def writeRef(a: Ref) = primitive(_.writeRef(a))
    override def writeRowId(a: RowId) = primitive(_.writeRowId(a))
    override def writeSQLXML(a: SQLXML) = primitive(_.writeSQLXML(a))
    override def writeShort(a: Short) = primitive(_.writeShort(a))
    override def writeString(a: String) = primitive(_.writeString(a))
    override def writeStruct(a: Struct) = primitive(_.writeStruct(a))
    override def writeTime(a: Time) = primitive(_.writeTime(a))
    override def writeTimestamp(a: Timestamp) = primitive(_.writeTimestamp(a))
    override def writeURL(a: URL) = primitive(_.writeURL(a))

  }

  trait ConnectionInterpreter extends ConnectionOp.Visitor[Kleisli[M, Connection, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](f: Connection => A): Kleisli[M, Connection, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Connection, A] = outer.embed(e)
    override def raiseError[A](e: Throwable): Kleisli[M, Connection, A] = outer.raiseError(e)
    override def monotonic: Kleisli[M, Connection, FiniteDuration] = outer.monotonic[Connection]
    override def realTime: Kleisli[M, Connection, FiniteDuration] = outer.realTime[Connection]
    override def delay[A](thunk: => A): Kleisli[M, Connection, A] = outer.delay(thunk)
    override def suspend[A](hint: Sync.Type)(thunk: => A): Kleisli[M, Connection, A] = outer.suspend(hint)(thunk)
    override def canceled: Kleisli[M, Connection, Unit] = outer.canceled[Connection]

    override def performLogging(event: LogEvent): Kleisli[M, Connection, Unit] = Kleisli(_ => logHandler.run(event))

    // for operations using ConnectionIO we must call ourself recursively
    override def handleErrorWith[A](fa: ConnectionIO[A])(f: Throwable => ConnectionIO[A]): Kleisli[M, Connection, A] = outer.handleErrorWith(this)(fa)(f)
    override def forceR[A, B](fa: ConnectionIO[A])(fb: ConnectionIO[B]): Kleisli[M, Connection, B] = outer.forceR(this)(fa)(fb)
    override def uncancelable[A](body: Poll[ConnectionIO] => ConnectionIO[A]): Kleisli[M, Connection, A] = outer.uncancelable(this, doobie.free.connection.capturePoll)(body)
    override def poll[A](poll: Any, fa: ConnectionIO[A]): Kleisli[M, Connection, A] = outer.poll(this)(poll, fa)
    override def onCancel[A](fa: ConnectionIO[A], fin: ConnectionIO[Unit]): Kleisli[M, Connection, A] = outer.onCancel(this)(fa, fin)
    override def fromFuture[A](fut: ConnectionIO[Future[A]]): Kleisli[M, Connection, A] = outer.fromFuture(this)(fut)
    override def fromFutureCancelable[A](fut: ConnectionIO[(Future[A], ConnectionIO[Unit])]): Kleisli[M, Connection, A] = outer.fromFutureCancelable(this)(fut)
    override def cancelable[A](fa: ConnectionIO[A], fin: ConnectionIO[Unit]): Kleisli[M, Connection, A] = outer.cancelable(this)(fa, fin)


    // domain-specific operations are implemented in terms of `primitive`
    override def abort(a: Executor) = primitive(_.abort(a))
    override def beginRequest: Kleisli[M, Connection, Unit] = primitive(_.beginRequest)
    override def clearWarnings: Kleisli[M, Connection, Unit] = primitive(_.clearWarnings)
    override def close: Kleisli[M, Connection, Unit] = primitive(_.close)
    override def commit: Kleisli[M, Connection, Unit] = primitive(_.commit)
    override def createArrayOf(a: String, b: Array[AnyRef]) = primitive(_.createArrayOf(a, b))
    override def createBlob: Kleisli[M, Connection, Blob] = primitive(_.createBlob)
    override def createClob: Kleisli[M, Connection, Clob] = primitive(_.createClob)
    override def createNClob: Kleisli[M, Connection, NClob] = primitive(_.createNClob)
    override def createSQLXML: Kleisli[M, Connection, SQLXML] = primitive(_.createSQLXML)
    override def createStatement: Kleisli[M, Connection, Statement] = primitive(_.createStatement)
    override def createStatement(a: Int, b: Int) = primitive(_.createStatement(a, b))
    override def createStatement(a: Int, b: Int, c: Int) = primitive(_.createStatement(a, b, c))
    override def createStruct(a: String, b: Array[AnyRef]) = primitive(_.createStruct(a, b))
    override def endRequest: Kleisli[M, Connection, Unit] = primitive(_.endRequest)
    override def getAutoCommit: Kleisli[M, Connection, Boolean] = primitive(_.getAutoCommit)
    override def getCatalog: Kleisli[M, Connection, String] = primitive(_.getCatalog)
    override def getClientInfo: Kleisli[M, Connection, Properties] = primitive(_.getClientInfo)
    override def getClientInfo(a: String) = primitive(_.getClientInfo(a))
    override def getHoldability: Kleisli[M, Connection, Int] = primitive(_.getHoldability)
    override def getMetaData: Kleisli[M, Connection, DatabaseMetaData] = primitive(_.getMetaData)
    override def getNetworkTimeout: Kleisli[M, Connection, Int] = primitive(_.getNetworkTimeout)
    override def getSchema: Kleisli[M, Connection, String] = primitive(_.getSchema)
    override def getTransactionIsolation: Kleisli[M, Connection, Int] = primitive(_.getTransactionIsolation)
    override def getTypeMap: Kleisli[M, Connection, java.util.Map[String, Class[?]]] = primitive(_.getTypeMap)
    override def getWarnings: Kleisli[M, Connection, SQLWarning] = primitive(_.getWarnings)
    override def isClosed: Kleisli[M, Connection, Boolean] = primitive(_.isClosed)
    override def isReadOnly: Kleisli[M, Connection, Boolean] = primitive(_.isReadOnly)
    override def isValid(a: Int) = primitive(_.isValid(a))
    override def isWrapperFor(a: Class[?]) = primitive(_.isWrapperFor(a))
    override def nativeSQL(a: String) = primitive(_.nativeSQL(a))
    override def prepareCall(a: String) = primitive(_.prepareCall(a))
    override def prepareCall(a: String, b: Int, c: Int) = primitive(_.prepareCall(a, b, c))
    override def prepareCall(a: String, b: Int, c: Int, d: Int) = primitive(_.prepareCall(a, b, c, d))
    override def prepareStatement(a: String) = primitive(_.prepareStatement(a))
    override def prepareStatement(a: String, b: Array[Int]) = primitive(_.prepareStatement(a, b))
    override def prepareStatement(a: String, b: Array[String]) = primitive(_.prepareStatement(a, b))
    override def prepareStatement(a: String, b: Int) = primitive(_.prepareStatement(a, b))
    override def prepareStatement(a: String, b: Int, c: Int) = primitive(_.prepareStatement(a, b, c))
    override def prepareStatement(a: String, b: Int, c: Int, d: Int) = primitive(_.prepareStatement(a, b, c, d))
    override def releaseSavepoint(a: Savepoint) = primitive(_.releaseSavepoint(a))
    override def rollback: Kleisli[M, Connection, Unit] = primitive(_.rollback)
    override def rollback(a: Savepoint) = primitive(_.rollback(a))
    override def setAutoCommit(a: Boolean) = primitive(_.setAutoCommit(a))
    override def setCatalog(a: String) = primitive(_.setCatalog(a))
    override def setClientInfo(a: Properties) = primitive(_.setClientInfo(a))
    override def setClientInfo(a: String, b: String) = primitive(_.setClientInfo(a, b))
    override def setHoldability(a: Int) = primitive(_.setHoldability(a))
    override def setNetworkTimeout(a: Executor, b: Int) = primitive(_.setNetworkTimeout(a, b))
    override def setReadOnly(a: Boolean) = primitive(_.setReadOnly(a))
    override def setSavepoint: Kleisli[M, Connection, Savepoint] = primitive(_.setSavepoint)
    override def setSavepoint(a: String) = primitive(_.setSavepoint(a))
    override def setSchema(a: String) = primitive(_.setSchema(a))
    override def setShardingKey(a: ShardingKey) = primitive(_.setShardingKey(a))
    override def setShardingKey(a: ShardingKey, b: ShardingKey) = primitive(_.setShardingKey(a, b))
    override def setShardingKeyIfValid(a: ShardingKey, b: Int) = primitive(_.setShardingKeyIfValid(a, b))
    override def setShardingKeyIfValid(a: ShardingKey, b: ShardingKey, c: Int) = primitive(_.setShardingKeyIfValid(a, b, c))
    override def setTransactionIsolation(a: Int) = primitive(_.setTransactionIsolation(a))
    override def setTypeMap(a: java.util.Map[String, Class[?]]) = primitive(_.setTypeMap(a))
    override def unwrap[T](a: Class[T]) = primitive(_.unwrap(a))

  }

  trait StatementInterpreter extends StatementOp.Visitor[Kleisli[M, Statement, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](f: Statement => A): Kleisli[M, Statement, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Statement, A] = outer.embed(e)
    override def raiseError[A](e: Throwable): Kleisli[M, Statement, A] = outer.raiseError(e)
    override def monotonic: Kleisli[M, Statement, FiniteDuration] = outer.monotonic[Statement]
    override def realTime: Kleisli[M, Statement, FiniteDuration] = outer.realTime[Statement]
    override def delay[A](thunk: => A): Kleisli[M, Statement, A] = outer.delay(thunk)
    override def suspend[A](hint: Sync.Type)(thunk: => A): Kleisli[M, Statement, A] = outer.suspend(hint)(thunk)
    override def canceled: Kleisli[M, Statement, Unit] = outer.canceled[Statement]

    override def performLogging(event: LogEvent): Kleisli[M, Statement, Unit] = Kleisli(_ => logHandler.run(event))

    // for operations using StatementIO we must call ourself recursively
    override def handleErrorWith[A](fa: StatementIO[A])(f: Throwable => StatementIO[A]): Kleisli[M, Statement, A] = outer.handleErrorWith(this)(fa)(f)
    override def forceR[A, B](fa: StatementIO[A])(fb: StatementIO[B]): Kleisli[M, Statement, B] = outer.forceR(this)(fa)(fb)
    override def uncancelable[A](body: Poll[StatementIO] => StatementIO[A]): Kleisli[M, Statement, A] = outer.uncancelable(this, doobie.free.statement.capturePoll)(body)
    override def poll[A](poll: Any, fa: StatementIO[A]): Kleisli[M, Statement, A] = outer.poll(this)(poll, fa)
    override def onCancel[A](fa: StatementIO[A], fin: StatementIO[Unit]): Kleisli[M, Statement, A] = outer.onCancel(this)(fa, fin)
    override def fromFuture[A](fut: StatementIO[Future[A]]): Kleisli[M, Statement, A] = outer.fromFuture(this)(fut)
    override def fromFutureCancelable[A](fut: StatementIO[(Future[A], StatementIO[Unit])]): Kleisli[M, Statement, A] = outer.fromFutureCancelable(this)(fut)
    override def cancelable[A](fa: StatementIO[A], fin: StatementIO[Unit]): Kleisli[M, Statement, A] = outer.cancelable(this)(fa, fin)


    // domain-specific operations are implemented in terms of `primitive`
    override def addBatch(a: String) = primitive(_.addBatch(a))
    override def cancel: Kleisli[M, Statement, Unit] = primitive(_.cancel)
    override def clearBatch: Kleisli[M, Statement, Unit] = primitive(_.clearBatch)
    override def clearWarnings: Kleisli[M, Statement, Unit] = primitive(_.clearWarnings)
    override def close: Kleisli[M, Statement, Unit] = primitive(_.close)
    override def closeOnCompletion: Kleisli[M, Statement, Unit] = primitive(_.closeOnCompletion)
    override def enquoteIdentifier(a: String, b: Boolean) = primitive(_.enquoteIdentifier(a, b))
    override def enquoteLiteral(a: String) = primitive(_.enquoteLiteral(a))
    override def enquoteNCharLiteral(a: String) = primitive(_.enquoteNCharLiteral(a))
    override def execute(a: String) = primitive(_.execute(a))
    override def execute(a: String, b: Array[Int]) = primitive(_.execute(a, b))
    override def execute(a: String, b: Array[String]) = primitive(_.execute(a, b))
    override def execute(a: String, b: Int) = primitive(_.execute(a, b))
    override def executeBatch: Kleisli[M, Statement, Array[Int]] = primitive(_.executeBatch)
    override def executeLargeBatch: Kleisli[M, Statement, Array[Long]] = primitive(_.executeLargeBatch)
    override def executeLargeUpdate(a: String) = primitive(_.executeLargeUpdate(a))
    override def executeLargeUpdate(a: String, b: Array[Int]) = primitive(_.executeLargeUpdate(a, b))
    override def executeLargeUpdate(a: String, b: Array[String]) = primitive(_.executeLargeUpdate(a, b))
    override def executeLargeUpdate(a: String, b: Int) = primitive(_.executeLargeUpdate(a, b))
    override def executeQuery(a: String) = primitive(_.executeQuery(a))
    override def executeUpdate(a: String) = primitive(_.executeUpdate(a))
    override def executeUpdate(a: String, b: Array[Int]) = primitive(_.executeUpdate(a, b))
    override def executeUpdate(a: String, b: Array[String]) = primitive(_.executeUpdate(a, b))
    override def executeUpdate(a: String, b: Int) = primitive(_.executeUpdate(a, b))
    override def getConnection: Kleisli[M, Statement, Connection] = primitive(_.getConnection)
    override def getFetchDirection: Kleisli[M, Statement, Int] = primitive(_.getFetchDirection)
    override def getFetchSize: Kleisli[M, Statement, Int] = primitive(_.getFetchSize)
    override def getGeneratedKeys: Kleisli[M, Statement, ResultSet] = primitive(_.getGeneratedKeys)
    override def getLargeMaxRows: Kleisli[M, Statement, Long] = primitive(_.getLargeMaxRows)
    override def getLargeUpdateCount: Kleisli[M, Statement, Long] = primitive(_.getLargeUpdateCount)
    override def getMaxFieldSize: Kleisli[M, Statement, Int] = primitive(_.getMaxFieldSize)
    override def getMaxRows: Kleisli[M, Statement, Int] = primitive(_.getMaxRows)
    override def getMoreResults: Kleisli[M, Statement, Boolean] = primitive(_.getMoreResults)
    override def getMoreResults(a: Int) = primitive(_.getMoreResults(a))
    override def getQueryTimeout: Kleisli[M, Statement, Int] = primitive(_.getQueryTimeout)
    override def getResultSet: Kleisli[M, Statement, ResultSet] = primitive(_.getResultSet)
    override def getResultSetConcurrency: Kleisli[M, Statement, Int] = primitive(_.getResultSetConcurrency)
    override def getResultSetHoldability: Kleisli[M, Statement, Int] = primitive(_.getResultSetHoldability)
    override def getResultSetType: Kleisli[M, Statement, Int] = primitive(_.getResultSetType)
    override def getUpdateCount: Kleisli[M, Statement, Int] = primitive(_.getUpdateCount)
    override def getWarnings: Kleisli[M, Statement, SQLWarning] = primitive(_.getWarnings)
    override def isCloseOnCompletion: Kleisli[M, Statement, Boolean] = primitive(_.isCloseOnCompletion)
    override def isClosed: Kleisli[M, Statement, Boolean] = primitive(_.isClosed)
    override def isPoolable: Kleisli[M, Statement, Boolean] = primitive(_.isPoolable)
    override def isSimpleIdentifier(a: String) = primitive(_.isSimpleIdentifier(a))
    override def isWrapperFor(a: Class[?]) = primitive(_.isWrapperFor(a))
    override def setCursorName(a: String) = primitive(_.setCursorName(a))
    override def setEscapeProcessing(a: Boolean) = primitive(_.setEscapeProcessing(a))
    override def setFetchDirection(a: Int) = primitive(_.setFetchDirection(a))
    override def setFetchSize(a: Int) = primitive(_.setFetchSize(a))
    override def setLargeMaxRows(a: Long) = primitive(_.setLargeMaxRows(a))
    override def setMaxFieldSize(a: Int) = primitive(_.setMaxFieldSize(a))
    override def setMaxRows(a: Int) = primitive(_.setMaxRows(a))
    override def setPoolable(a: Boolean) = primitive(_.setPoolable(a))
    override def setQueryTimeout(a: Int) = primitive(_.setQueryTimeout(a))
    override def unwrap[T](a: Class[T]) = primitive(_.unwrap(a))

  }

  trait PreparedStatementInterpreter extends PreparedStatementOp.Visitor[Kleisli[M, PreparedStatement, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](f: PreparedStatement => A): Kleisli[M, PreparedStatement, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, PreparedStatement, A] = outer.embed(e)
    override def raiseError[A](e: Throwable): Kleisli[M, PreparedStatement, A] = outer.raiseError(e)
    override def monotonic: Kleisli[M, PreparedStatement, FiniteDuration] = outer.monotonic[PreparedStatement]
    override def realTime: Kleisli[M, PreparedStatement, FiniteDuration] = outer.realTime[PreparedStatement]
    override def delay[A](thunk: => A): Kleisli[M, PreparedStatement, A] = outer.delay(thunk)
    override def suspend[A](hint: Sync.Type)(thunk: => A): Kleisli[M, PreparedStatement, A] = outer.suspend(hint)(thunk)
    override def canceled: Kleisli[M, PreparedStatement, Unit] = outer.canceled[PreparedStatement]

    override def performLogging(event: LogEvent): Kleisli[M, PreparedStatement, Unit] = Kleisli(_ => logHandler.run(event))

    // for operations using PreparedStatementIO we must call ourself recursively
    override def handleErrorWith[A](fa: PreparedStatementIO[A])(f: Throwable => PreparedStatementIO[A]): Kleisli[M, PreparedStatement, A] = outer.handleErrorWith(this)(fa)(f)
    override def forceR[A, B](fa: PreparedStatementIO[A])(fb: PreparedStatementIO[B]): Kleisli[M, PreparedStatement, B] = outer.forceR(this)(fa)(fb)
    override def uncancelable[A](body: Poll[PreparedStatementIO] => PreparedStatementIO[A]): Kleisli[M, PreparedStatement, A] = outer.uncancelable(this, doobie.free.preparedstatement.capturePoll)(body)
    override def poll[A](poll: Any, fa: PreparedStatementIO[A]): Kleisli[M, PreparedStatement, A] = outer.poll(this)(poll, fa)
    override def onCancel[A](fa: PreparedStatementIO[A], fin: PreparedStatementIO[Unit]): Kleisli[M, PreparedStatement, A] = outer.onCancel(this)(fa, fin)
    override def fromFuture[A](fut: PreparedStatementIO[Future[A]]): Kleisli[M, PreparedStatement, A] = outer.fromFuture(this)(fut)
    override def fromFutureCancelable[A](fut: PreparedStatementIO[(Future[A], PreparedStatementIO[Unit])]): Kleisli[M, PreparedStatement, A] = outer.fromFutureCancelable(this)(fut)
    override def cancelable[A](fa: PreparedStatementIO[A], fin: PreparedStatementIO[Unit]): Kleisli[M, PreparedStatement, A] = outer.cancelable(this)(fa, fin)


    // domain-specific operations are implemented in terms of `primitive`
    override def addBatch: Kleisli[M, PreparedStatement, Unit] = primitive(_.addBatch)
    override def addBatch(a: String) = primitive(_.addBatch(a))
    override def cancel: Kleisli[M, PreparedStatement, Unit] = primitive(_.cancel)
    override def clearBatch: Kleisli[M, PreparedStatement, Unit] = primitive(_.clearBatch)
    override def clearParameters: Kleisli[M, PreparedStatement, Unit] = primitive(_.clearParameters)
    override def clearWarnings: Kleisli[M, PreparedStatement, Unit] = primitive(_.clearWarnings)
    override def close: Kleisli[M, PreparedStatement, Unit] = primitive(_.close)
    override def closeOnCompletion: Kleisli[M, PreparedStatement, Unit] = primitive(_.closeOnCompletion)
    override def enquoteIdentifier(a: String, b: Boolean) = primitive(_.enquoteIdentifier(a, b))
    override def enquoteLiteral(a: String) = primitive(_.enquoteLiteral(a))
    override def enquoteNCharLiteral(a: String) = primitive(_.enquoteNCharLiteral(a))
    override def execute: Kleisli[M, PreparedStatement, Boolean] = primitive(_.execute)
    override def execute(a: String) = primitive(_.execute(a))
    override def execute(a: String, b: Array[Int]) = primitive(_.execute(a, b))
    override def execute(a: String, b: Array[String]) = primitive(_.execute(a, b))
    override def execute(a: String, b: Int) = primitive(_.execute(a, b))
    override def executeBatch: Kleisli[M, PreparedStatement, Array[Int]] = primitive(_.executeBatch)
    override def executeLargeBatch: Kleisli[M, PreparedStatement, Array[Long]] = primitive(_.executeLargeBatch)
    override def executeLargeUpdate: Kleisli[M, PreparedStatement, Long] = primitive(_.executeLargeUpdate)
    override def executeLargeUpdate(a: String) = primitive(_.executeLargeUpdate(a))
    override def executeLargeUpdate(a: String, b: Array[Int]) = primitive(_.executeLargeUpdate(a, b))
    override def executeLargeUpdate(a: String, b: Array[String]) = primitive(_.executeLargeUpdate(a, b))
    override def executeLargeUpdate(a: String, b: Int) = primitive(_.executeLargeUpdate(a, b))
    override def executeQuery: Kleisli[M, PreparedStatement, ResultSet] = primitive(_.executeQuery)
    override def executeQuery(a: String) = primitive(_.executeQuery(a))
    override def executeUpdate: Kleisli[M, PreparedStatement, Int] = primitive(_.executeUpdate)
    override def executeUpdate(a: String) = primitive(_.executeUpdate(a))
    override def executeUpdate(a: String, b: Array[Int]) = primitive(_.executeUpdate(a, b))
    override def executeUpdate(a: String, b: Array[String]) = primitive(_.executeUpdate(a, b))
    override def executeUpdate(a: String, b: Int) = primitive(_.executeUpdate(a, b))
    override def getConnection: Kleisli[M, PreparedStatement, Connection] = primitive(_.getConnection)
    override def getFetchDirection: Kleisli[M, PreparedStatement, Int] = primitive(_.getFetchDirection)
    override def getFetchSize: Kleisli[M, PreparedStatement, Int] = primitive(_.getFetchSize)
    override def getGeneratedKeys: Kleisli[M, PreparedStatement, ResultSet] = primitive(_.getGeneratedKeys)
    override def getLargeMaxRows: Kleisli[M, PreparedStatement, Long] = primitive(_.getLargeMaxRows)
    override def getLargeUpdateCount: Kleisli[M, PreparedStatement, Long] = primitive(_.getLargeUpdateCount)
    override def getMaxFieldSize: Kleisli[M, PreparedStatement, Int] = primitive(_.getMaxFieldSize)
    override def getMaxRows: Kleisli[M, PreparedStatement, Int] = primitive(_.getMaxRows)
    override def getMetaData: Kleisli[M, PreparedStatement, ResultSetMetaData] = primitive(_.getMetaData)
    override def getMoreResults: Kleisli[M, PreparedStatement, Boolean] = primitive(_.getMoreResults)
    override def getMoreResults(a: Int) = primitive(_.getMoreResults(a))
    override def getParameterMetaData: Kleisli[M, PreparedStatement, ParameterMetaData] = primitive(_.getParameterMetaData)
    override def getQueryTimeout: Kleisli[M, PreparedStatement, Int] = primitive(_.getQueryTimeout)
    override def getResultSet: Kleisli[M, PreparedStatement, ResultSet] = primitive(_.getResultSet)
    override def getResultSetConcurrency: Kleisli[M, PreparedStatement, Int] = primitive(_.getResultSetConcurrency)
    override def getResultSetHoldability: Kleisli[M, PreparedStatement, Int] = primitive(_.getResultSetHoldability)
    override def getResultSetType: Kleisli[M, PreparedStatement, Int] = primitive(_.getResultSetType)
    override def getUpdateCount: Kleisli[M, PreparedStatement, Int] = primitive(_.getUpdateCount)
    override def getWarnings: Kleisli[M, PreparedStatement, SQLWarning] = primitive(_.getWarnings)
    override def isCloseOnCompletion: Kleisli[M, PreparedStatement, Boolean] = primitive(_.isCloseOnCompletion)
    override def isClosed: Kleisli[M, PreparedStatement, Boolean] = primitive(_.isClosed)
    override def isPoolable: Kleisli[M, PreparedStatement, Boolean] = primitive(_.isPoolable)
    override def isSimpleIdentifier(a: String) = primitive(_.isSimpleIdentifier(a))
    override def isWrapperFor(a: Class[?]) = primitive(_.isWrapperFor(a))
    override def setArray(a: Int, b: SqlArray) = primitive(_.setArray(a, b))
    override def setAsciiStream(a: Int, b: InputStream) = primitive(_.setAsciiStream(a, b))
    override def setAsciiStream(a: Int, b: InputStream, c: Int) = primitive(_.setAsciiStream(a, b, c))
    override def setAsciiStream(a: Int, b: InputStream, c: Long) = primitive(_.setAsciiStream(a, b, c))
    override def setBigDecimal(a: Int, b: BigDecimal) = primitive(_.setBigDecimal(a, b))
    override def setBinaryStream(a: Int, b: InputStream) = primitive(_.setBinaryStream(a, b))
    override def setBinaryStream(a: Int, b: InputStream, c: Int) = primitive(_.setBinaryStream(a, b, c))
    override def setBinaryStream(a: Int, b: InputStream, c: Long) = primitive(_.setBinaryStream(a, b, c))
    override def setBlob(a: Int, b: Blob) = primitive(_.setBlob(a, b))
    override def setBlob(a: Int, b: InputStream) = primitive(_.setBlob(a, b))
    override def setBlob(a: Int, b: InputStream, c: Long) = primitive(_.setBlob(a, b, c))
    override def setBoolean(a: Int, b: Boolean) = primitive(_.setBoolean(a, b))
    override def setByte(a: Int, b: Byte) = primitive(_.setByte(a, b))
    override def setBytes(a: Int, b: Array[Byte]) = primitive(_.setBytes(a, b))
    override def setCharacterStream(a: Int, b: Reader) = primitive(_.setCharacterStream(a, b))
    override def setCharacterStream(a: Int, b: Reader, c: Int) = primitive(_.setCharacterStream(a, b, c))
    override def setCharacterStream(a: Int, b: Reader, c: Long) = primitive(_.setCharacterStream(a, b, c))
    override def setClob(a: Int, b: Clob) = primitive(_.setClob(a, b))
    override def setClob(a: Int, b: Reader) = primitive(_.setClob(a, b))
    override def setClob(a: Int, b: Reader, c: Long) = primitive(_.setClob(a, b, c))
    override def setCursorName(a: String) = primitive(_.setCursorName(a))
    override def setDate(a: Int, b: Date) = primitive(_.setDate(a, b))
    override def setDate(a: Int, b: Date, c: Calendar) = primitive(_.setDate(a, b, c))
    override def setDouble(a: Int, b: Double) = primitive(_.setDouble(a, b))
    override def setEscapeProcessing(a: Boolean) = primitive(_.setEscapeProcessing(a))
    override def setFetchDirection(a: Int) = primitive(_.setFetchDirection(a))
    override def setFetchSize(a: Int) = primitive(_.setFetchSize(a))
    override def setFloat(a: Int, b: Float) = primitive(_.setFloat(a, b))
    override def setInt(a: Int, b: Int) = primitive(_.setInt(a, b))
    override def setLargeMaxRows(a: Long) = primitive(_.setLargeMaxRows(a))
    override def setLong(a: Int, b: Long) = primitive(_.setLong(a, b))
    override def setMaxFieldSize(a: Int) = primitive(_.setMaxFieldSize(a))
    override def setMaxRows(a: Int) = primitive(_.setMaxRows(a))
    override def setNCharacterStream(a: Int, b: Reader) = primitive(_.setNCharacterStream(a, b))
    override def setNCharacterStream(a: Int, b: Reader, c: Long) = primitive(_.setNCharacterStream(a, b, c))
    override def setNClob(a: Int, b: NClob) = primitive(_.setNClob(a, b))
    override def setNClob(a: Int, b: Reader) = primitive(_.setNClob(a, b))
    override def setNClob(a: Int, b: Reader, c: Long) = primitive(_.setNClob(a, b, c))
    override def setNString(a: Int, b: String) = primitive(_.setNString(a, b))
    override def setNull(a: Int, b: Int) = primitive(_.setNull(a, b))
    override def setNull(a: Int, b: Int, c: String) = primitive(_.setNull(a, b, c))
    override def setObject(a: Int, b: AnyRef) = primitive(_.setObject(a, b))
    override def setObject(a: Int, b: AnyRef, c: Int) = primitive(_.setObject(a, b, c))
    override def setObject(a: Int, b: AnyRef, c: Int, d: Int) = primitive(_.setObject(a, b, c, d))
    override def setObject(a: Int, b: AnyRef, c: SQLType) = primitive(_.setObject(a, b, c))
    override def setObject(a: Int, b: AnyRef, c: SQLType, d: Int) = primitive(_.setObject(a, b, c, d))
    override def setPoolable(a: Boolean) = primitive(_.setPoolable(a))
    override def setQueryTimeout(a: Int) = primitive(_.setQueryTimeout(a))
    override def setRef(a: Int, b: Ref) = primitive(_.setRef(a, b))
    override def setRowId(a: Int, b: RowId) = primitive(_.setRowId(a, b))
    override def setSQLXML(a: Int, b: SQLXML) = primitive(_.setSQLXML(a, b))
    override def setShort(a: Int, b: Short) = primitive(_.setShort(a, b))
    override def setString(a: Int, b: String) = primitive(_.setString(a, b))
    override def setTime(a: Int, b: Time) = primitive(_.setTime(a, b))
    override def setTime(a: Int, b: Time, c: Calendar) = primitive(_.setTime(a, b, c))
    override def setTimestamp(a: Int, b: Timestamp) = primitive(_.setTimestamp(a, b))
    override def setTimestamp(a: Int, b: Timestamp, c: Calendar) = primitive(_.setTimestamp(a, b, c))
    override def setURL(a: Int, b: URL) = primitive(_.setURL(a, b))
    override def unwrap[T](a: Class[T]) = primitive(_.unwrap(a))

  }

  trait CallableStatementInterpreter extends CallableStatementOp.Visitor[Kleisli[M, CallableStatement, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](f: CallableStatement => A): Kleisli[M, CallableStatement, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, CallableStatement, A] = outer.embed(e)
    override def raiseError[A](e: Throwable): Kleisli[M, CallableStatement, A] = outer.raiseError(e)
    override def monotonic: Kleisli[M, CallableStatement, FiniteDuration] = outer.monotonic[CallableStatement]
    override def realTime: Kleisli[M, CallableStatement, FiniteDuration] = outer.realTime[CallableStatement]
    override def delay[A](thunk: => A): Kleisli[M, CallableStatement, A] = outer.delay(thunk)
    override def suspend[A](hint: Sync.Type)(thunk: => A): Kleisli[M, CallableStatement, A] = outer.suspend(hint)(thunk)
    override def canceled: Kleisli[M, CallableStatement, Unit] = outer.canceled[CallableStatement]

    override def performLogging(event: LogEvent): Kleisli[M, CallableStatement, Unit] = Kleisli(_ => logHandler.run(event))

    // for operations using CallableStatementIO we must call ourself recursively
    override def handleErrorWith[A](fa: CallableStatementIO[A])(f: Throwable => CallableStatementIO[A]): Kleisli[M, CallableStatement, A] = outer.handleErrorWith(this)(fa)(f)
    override def forceR[A, B](fa: CallableStatementIO[A])(fb: CallableStatementIO[B]): Kleisli[M, CallableStatement, B] = outer.forceR(this)(fa)(fb)
    override def uncancelable[A](body: Poll[CallableStatementIO] => CallableStatementIO[A]): Kleisli[M, CallableStatement, A] = outer.uncancelable(this, doobie.free.callablestatement.capturePoll)(body)
    override def poll[A](poll: Any, fa: CallableStatementIO[A]): Kleisli[M, CallableStatement, A] = outer.poll(this)(poll, fa)
    override def onCancel[A](fa: CallableStatementIO[A], fin: CallableStatementIO[Unit]): Kleisli[M, CallableStatement, A] = outer.onCancel(this)(fa, fin)
    override def fromFuture[A](fut: CallableStatementIO[Future[A]]): Kleisli[M, CallableStatement, A] = outer.fromFuture(this)(fut)
    override def fromFutureCancelable[A](fut: CallableStatementIO[(Future[A], CallableStatementIO[Unit])]): Kleisli[M, CallableStatement, A] = outer.fromFutureCancelable(this)(fut)
    override def cancelable[A](fa: CallableStatementIO[A], fin: CallableStatementIO[Unit]): Kleisli[M, CallableStatement, A] = outer.cancelable(this)(fa, fin)


    // domain-specific operations are implemented in terms of `primitive`
    override def addBatch: Kleisli[M, CallableStatement, Unit] = primitive(_.addBatch)
    override def addBatch(a: String) = primitive(_.addBatch(a))
    override def cancel: Kleisli[M, CallableStatement, Unit] = primitive(_.cancel)
    override def clearBatch: Kleisli[M, CallableStatement, Unit] = primitive(_.clearBatch)
    override def clearParameters: Kleisli[M, CallableStatement, Unit] = primitive(_.clearParameters)
    override def clearWarnings: Kleisli[M, CallableStatement, Unit] = primitive(_.clearWarnings)
    override def close: Kleisli[M, CallableStatement, Unit] = primitive(_.close)
    override def closeOnCompletion: Kleisli[M, CallableStatement, Unit] = primitive(_.closeOnCompletion)
    override def enquoteIdentifier(a: String, b: Boolean) = primitive(_.enquoteIdentifier(a, b))
    override def enquoteLiteral(a: String) = primitive(_.enquoteLiteral(a))
    override def enquoteNCharLiteral(a: String) = primitive(_.enquoteNCharLiteral(a))
    override def execute: Kleisli[M, CallableStatement, Boolean] = primitive(_.execute)
    override def execute(a: String) = primitive(_.execute(a))
    override def execute(a: String, b: Array[Int]) = primitive(_.execute(a, b))
    override def execute(a: String, b: Array[String]) = primitive(_.execute(a, b))
    override def execute(a: String, b: Int) = primitive(_.execute(a, b))
    override def executeBatch: Kleisli[M, CallableStatement, Array[Int]] = primitive(_.executeBatch)
    override def executeLargeBatch: Kleisli[M, CallableStatement, Array[Long]] = primitive(_.executeLargeBatch)
    override def executeLargeUpdate: Kleisli[M, CallableStatement, Long] = primitive(_.executeLargeUpdate)
    override def executeLargeUpdate(a: String) = primitive(_.executeLargeUpdate(a))
    override def executeLargeUpdate(a: String, b: Array[Int]) = primitive(_.executeLargeUpdate(a, b))
    override def executeLargeUpdate(a: String, b: Array[String]) = primitive(_.executeLargeUpdate(a, b))
    override def executeLargeUpdate(a: String, b: Int) = primitive(_.executeLargeUpdate(a, b))
    override def executeQuery: Kleisli[M, CallableStatement, ResultSet] = primitive(_.executeQuery)
    override def executeQuery(a: String) = primitive(_.executeQuery(a))
    override def executeUpdate: Kleisli[M, CallableStatement, Int] = primitive(_.executeUpdate)
    override def executeUpdate(a: String) = primitive(_.executeUpdate(a))
    override def executeUpdate(a: String, b: Array[Int]) = primitive(_.executeUpdate(a, b))
    override def executeUpdate(a: String, b: Array[String]) = primitive(_.executeUpdate(a, b))
    override def executeUpdate(a: String, b: Int) = primitive(_.executeUpdate(a, b))
    override def getArray(a: Int) = primitive(_.getArray(a))
    override def getArray(a: String) = primitive(_.getArray(a))
    override def getBigDecimal(a: Int) = primitive(_.getBigDecimal(a))
    override def getBigDecimal(a: String) = primitive(_.getBigDecimal(a))
    override def getBlob(a: Int) = primitive(_.getBlob(a))
    override def getBlob(a: String) = primitive(_.getBlob(a))
    override def getBoolean(a: Int) = primitive(_.getBoolean(a))
    override def getBoolean(a: String) = primitive(_.getBoolean(a))
    override def getByte(a: Int) = primitive(_.getByte(a))
    override def getByte(a: String) = primitive(_.getByte(a))
    override def getBytes(a: Int) = primitive(_.getBytes(a))
    override def getBytes(a: String) = primitive(_.getBytes(a))
    override def getCharacterStream(a: Int) = primitive(_.getCharacterStream(a))
    override def getCharacterStream(a: String) = primitive(_.getCharacterStream(a))
    override def getClob(a: Int) = primitive(_.getClob(a))
    override def getClob(a: String) = primitive(_.getClob(a))
    override def getConnection: Kleisli[M, CallableStatement, Connection] = primitive(_.getConnection)
    override def getDate(a: Int) = primitive(_.getDate(a))
    override def getDate(a: Int, b: Calendar) = primitive(_.getDate(a, b))
    override def getDate(a: String) = primitive(_.getDate(a))
    override def getDate(a: String, b: Calendar) = primitive(_.getDate(a, b))
    override def getDouble(a: Int) = primitive(_.getDouble(a))
    override def getDouble(a: String) = primitive(_.getDouble(a))
    override def getFetchDirection: Kleisli[M, CallableStatement, Int] = primitive(_.getFetchDirection)
    override def getFetchSize: Kleisli[M, CallableStatement, Int] = primitive(_.getFetchSize)
    override def getFloat(a: Int) = primitive(_.getFloat(a))
    override def getFloat(a: String) = primitive(_.getFloat(a))
    override def getGeneratedKeys: Kleisli[M, CallableStatement, ResultSet] = primitive(_.getGeneratedKeys)
    override def getInt(a: Int) = primitive(_.getInt(a))
    override def getInt(a: String) = primitive(_.getInt(a))
    override def getLargeMaxRows: Kleisli[M, CallableStatement, Long] = primitive(_.getLargeMaxRows)
    override def getLargeUpdateCount: Kleisli[M, CallableStatement, Long] = primitive(_.getLargeUpdateCount)
    override def getLong(a: Int) = primitive(_.getLong(a))
    override def getLong(a: String) = primitive(_.getLong(a))
    override def getMaxFieldSize: Kleisli[M, CallableStatement, Int] = primitive(_.getMaxFieldSize)
    override def getMaxRows: Kleisli[M, CallableStatement, Int] = primitive(_.getMaxRows)
    override def getMetaData: Kleisli[M, CallableStatement, ResultSetMetaData] = primitive(_.getMetaData)
    override def getMoreResults: Kleisli[M, CallableStatement, Boolean] = primitive(_.getMoreResults)
    override def getMoreResults(a: Int) = primitive(_.getMoreResults(a))
    override def getNCharacterStream(a: Int) = primitive(_.getNCharacterStream(a))
    override def getNCharacterStream(a: String) = primitive(_.getNCharacterStream(a))
    override def getNClob(a: Int) = primitive(_.getNClob(a))
    override def getNClob(a: String) = primitive(_.getNClob(a))
    override def getNString(a: Int) = primitive(_.getNString(a))
    override def getNString(a: String) = primitive(_.getNString(a))
    override def getObject(a: Int) = primitive(_.getObject(a))
    override def getObject[T](a: Int, b: Class[T]) = primitive(_.getObject(a, b))
    override def getObject(a: Int, b: java.util.Map[String, Class[?]]) = primitive(_.getObject(a, b))
    override def getObject(a: String) = primitive(_.getObject(a))
    override def getObject[T](a: String, b: Class[T]) = primitive(_.getObject(a, b))
    override def getObject(a: String, b: java.util.Map[String, Class[?]]) = primitive(_.getObject(a, b))
    override def getParameterMetaData: Kleisli[M, CallableStatement, ParameterMetaData] = primitive(_.getParameterMetaData)
    override def getQueryTimeout: Kleisli[M, CallableStatement, Int] = primitive(_.getQueryTimeout)
    override def getRef(a: Int) = primitive(_.getRef(a))
    override def getRef(a: String) = primitive(_.getRef(a))
    override def getResultSet: Kleisli[M, CallableStatement, ResultSet] = primitive(_.getResultSet)
    override def getResultSetConcurrency: Kleisli[M, CallableStatement, Int] = primitive(_.getResultSetConcurrency)
    override def getResultSetHoldability: Kleisli[M, CallableStatement, Int] = primitive(_.getResultSetHoldability)
    override def getResultSetType: Kleisli[M, CallableStatement, Int] = primitive(_.getResultSetType)
    override def getRowId(a: Int) = primitive(_.getRowId(a))
    override def getRowId(a: String) = primitive(_.getRowId(a))
    override def getSQLXML(a: Int) = primitive(_.getSQLXML(a))
    override def getSQLXML(a: String) = primitive(_.getSQLXML(a))
    override def getShort(a: Int) = primitive(_.getShort(a))
    override def getShort(a: String) = primitive(_.getShort(a))
    override def getString(a: Int) = primitive(_.getString(a))
    override def getString(a: String) = primitive(_.getString(a))
    override def getTime(a: Int) = primitive(_.getTime(a))
    override def getTime(a: Int, b: Calendar) = primitive(_.getTime(a, b))
    override def getTime(a: String) = primitive(_.getTime(a))
    override def getTime(a: String, b: Calendar) = primitive(_.getTime(a, b))
    override def getTimestamp(a: Int) = primitive(_.getTimestamp(a))
    override def getTimestamp(a: Int, b: Calendar) = primitive(_.getTimestamp(a, b))
    override def getTimestamp(a: String) = primitive(_.getTimestamp(a))
    override def getTimestamp(a: String, b: Calendar) = primitive(_.getTimestamp(a, b))
    override def getURL(a: Int) = primitive(_.getURL(a))
    override def getURL(a: String) = primitive(_.getURL(a))
    override def getUpdateCount: Kleisli[M, CallableStatement, Int] = primitive(_.getUpdateCount)
    override def getWarnings: Kleisli[M, CallableStatement, SQLWarning] = primitive(_.getWarnings)
    override def isCloseOnCompletion: Kleisli[M, CallableStatement, Boolean] = primitive(_.isCloseOnCompletion)
    override def isClosed: Kleisli[M, CallableStatement, Boolean] = primitive(_.isClosed)
    override def isPoolable: Kleisli[M, CallableStatement, Boolean] = primitive(_.isPoolable)
    override def isSimpleIdentifier(a: String) = primitive(_.isSimpleIdentifier(a))
    override def isWrapperFor(a: Class[?]) = primitive(_.isWrapperFor(a))
    override def registerOutParameter(a: Int, b: Int) = primitive(_.registerOutParameter(a, b))
    override def registerOutParameter(a: Int, b: Int, c: Int) = primitive(_.registerOutParameter(a, b, c))
    override def registerOutParameter(a: Int, b: Int, c: String) = primitive(_.registerOutParameter(a, b, c))
    override def registerOutParameter(a: Int, b: SQLType) = primitive(_.registerOutParameter(a, b))
    override def registerOutParameter(a: Int, b: SQLType, c: Int) = primitive(_.registerOutParameter(a, b, c))
    override def registerOutParameter(a: Int, b: SQLType, c: String) = primitive(_.registerOutParameter(a, b, c))
    override def registerOutParameter(a: String, b: Int) = primitive(_.registerOutParameter(a, b))
    override def registerOutParameter(a: String, b: Int, c: Int) = primitive(_.registerOutParameter(a, b, c))
    override def registerOutParameter(a: String, b: Int, c: String) = primitive(_.registerOutParameter(a, b, c))
    override def registerOutParameter(a: String, b: SQLType) = primitive(_.registerOutParameter(a, b))
    override def registerOutParameter(a: String, b: SQLType, c: Int) = primitive(_.registerOutParameter(a, b, c))
    override def registerOutParameter(a: String, b: SQLType, c: String) = primitive(_.registerOutParameter(a, b, c))
    override def setArray(a: Int, b: SqlArray) = primitive(_.setArray(a, b))
    override def setAsciiStream(a: Int, b: InputStream) = primitive(_.setAsciiStream(a, b))
    override def setAsciiStream(a: Int, b: InputStream, c: Int) = primitive(_.setAsciiStream(a, b, c))
    override def setAsciiStream(a: Int, b: InputStream, c: Long) = primitive(_.setAsciiStream(a, b, c))
    override def setAsciiStream(a: String, b: InputStream) = primitive(_.setAsciiStream(a, b))
    override def setAsciiStream(a: String, b: InputStream, c: Int) = primitive(_.setAsciiStream(a, b, c))
    override def setAsciiStream(a: String, b: InputStream, c: Long) = primitive(_.setAsciiStream(a, b, c))
    override def setBigDecimal(a: Int, b: BigDecimal) = primitive(_.setBigDecimal(a, b))
    override def setBigDecimal(a: String, b: BigDecimal) = primitive(_.setBigDecimal(a, b))
    override def setBinaryStream(a: Int, b: InputStream) = primitive(_.setBinaryStream(a, b))
    override def setBinaryStream(a: Int, b: InputStream, c: Int) = primitive(_.setBinaryStream(a, b, c))
    override def setBinaryStream(a: Int, b: InputStream, c: Long) = primitive(_.setBinaryStream(a, b, c))
    override def setBinaryStream(a: String, b: InputStream) = primitive(_.setBinaryStream(a, b))
    override def setBinaryStream(a: String, b: InputStream, c: Int) = primitive(_.setBinaryStream(a, b, c))
    override def setBinaryStream(a: String, b: InputStream, c: Long) = primitive(_.setBinaryStream(a, b, c))
    override def setBlob(a: Int, b: Blob) = primitive(_.setBlob(a, b))
    override def setBlob(a: Int, b: InputStream) = primitive(_.setBlob(a, b))
    override def setBlob(a: Int, b: InputStream, c: Long) = primitive(_.setBlob(a, b, c))
    override def setBlob(a: String, b: Blob) = primitive(_.setBlob(a, b))
    override def setBlob(a: String, b: InputStream) = primitive(_.setBlob(a, b))
    override def setBlob(a: String, b: InputStream, c: Long) = primitive(_.setBlob(a, b, c))
    override def setBoolean(a: Int, b: Boolean) = primitive(_.setBoolean(a, b))
    override def setBoolean(a: String, b: Boolean) = primitive(_.setBoolean(a, b))
    override def setByte(a: Int, b: Byte) = primitive(_.setByte(a, b))
    override def setByte(a: String, b: Byte) = primitive(_.setByte(a, b))
    override def setBytes(a: Int, b: Array[Byte]) = primitive(_.setBytes(a, b))
    override def setBytes(a: String, b: Array[Byte]) = primitive(_.setBytes(a, b))
    override def setCharacterStream(a: Int, b: Reader) = primitive(_.setCharacterStream(a, b))
    override def setCharacterStream(a: Int, b: Reader, c: Int) = primitive(_.setCharacterStream(a, b, c))
    override def setCharacterStream(a: Int, b: Reader, c: Long) = primitive(_.setCharacterStream(a, b, c))
    override def setCharacterStream(a: String, b: Reader) = primitive(_.setCharacterStream(a, b))
    override def setCharacterStream(a: String, b: Reader, c: Int) = primitive(_.setCharacterStream(a, b, c))
    override def setCharacterStream(a: String, b: Reader, c: Long) = primitive(_.setCharacterStream(a, b, c))
    override def setClob(a: Int, b: Clob) = primitive(_.setClob(a, b))
    override def setClob(a: Int, b: Reader) = primitive(_.setClob(a, b))
    override def setClob(a: Int, b: Reader, c: Long) = primitive(_.setClob(a, b, c))
    override def setClob(a: String, b: Clob) = primitive(_.setClob(a, b))
    override def setClob(a: String, b: Reader) = primitive(_.setClob(a, b))
    override def setClob(a: String, b: Reader, c: Long) = primitive(_.setClob(a, b, c))
    override def setCursorName(a: String) = primitive(_.setCursorName(a))
    override def setDate(a: Int, b: Date) = primitive(_.setDate(a, b))
    override def setDate(a: Int, b: Date, c: Calendar) = primitive(_.setDate(a, b, c))
    override def setDate(a: String, b: Date) = primitive(_.setDate(a, b))
    override def setDate(a: String, b: Date, c: Calendar) = primitive(_.setDate(a, b, c))
    override def setDouble(a: Int, b: Double) = primitive(_.setDouble(a, b))
    override def setDouble(a: String, b: Double) = primitive(_.setDouble(a, b))
    override def setEscapeProcessing(a: Boolean) = primitive(_.setEscapeProcessing(a))
    override def setFetchDirection(a: Int) = primitive(_.setFetchDirection(a))
    override def setFetchSize(a: Int) = primitive(_.setFetchSize(a))
    override def setFloat(a: Int, b: Float) = primitive(_.setFloat(a, b))
    override def setFloat(a: String, b: Float) = primitive(_.setFloat(a, b))
    override def setInt(a: Int, b: Int) = primitive(_.setInt(a, b))
    override def setInt(a: String, b: Int) = primitive(_.setInt(a, b))
    override def setLargeMaxRows(a: Long) = primitive(_.setLargeMaxRows(a))
    override def setLong(a: Int, b: Long) = primitive(_.setLong(a, b))
    override def setLong(a: String, b: Long) = primitive(_.setLong(a, b))
    override def setMaxFieldSize(a: Int) = primitive(_.setMaxFieldSize(a))
    override def setMaxRows(a: Int) = primitive(_.setMaxRows(a))
    override def setNCharacterStream(a: Int, b: Reader) = primitive(_.setNCharacterStream(a, b))
    override def setNCharacterStream(a: Int, b: Reader, c: Long) = primitive(_.setNCharacterStream(a, b, c))
    override def setNCharacterStream(a: String, b: Reader) = primitive(_.setNCharacterStream(a, b))
    override def setNCharacterStream(a: String, b: Reader, c: Long) = primitive(_.setNCharacterStream(a, b, c))
    override def setNClob(a: Int, b: NClob) = primitive(_.setNClob(a, b))
    override def setNClob(a: Int, b: Reader) = primitive(_.setNClob(a, b))
    override def setNClob(a: Int, b: Reader, c: Long) = primitive(_.setNClob(a, b, c))
    override def setNClob(a: String, b: NClob) = primitive(_.setNClob(a, b))
    override def setNClob(a: String, b: Reader) = primitive(_.setNClob(a, b))
    override def setNClob(a: String, b: Reader, c: Long) = primitive(_.setNClob(a, b, c))
    override def setNString(a: Int, b: String) = primitive(_.setNString(a, b))
    override def setNString(a: String, b: String) = primitive(_.setNString(a, b))
    override def setNull(a: Int, b: Int) = primitive(_.setNull(a, b))
    override def setNull(a: Int, b: Int, c: String) = primitive(_.setNull(a, b, c))
    override def setNull(a: String, b: Int) = primitive(_.setNull(a, b))
    override def setNull(a: String, b: Int, c: String) = primitive(_.setNull(a, b, c))
    override def setObject(a: Int, b: AnyRef) = primitive(_.setObject(a, b))
    override def setObject(a: Int, b: AnyRef, c: Int) = primitive(_.setObject(a, b, c))
    override def setObject(a: Int, b: AnyRef, c: Int, d: Int) = primitive(_.setObject(a, b, c, d))
    override def setObject(a: Int, b: AnyRef, c: SQLType) = primitive(_.setObject(a, b, c))
    override def setObject(a: Int, b: AnyRef, c: SQLType, d: Int) = primitive(_.setObject(a, b, c, d))
    override def setObject(a: String, b: AnyRef) = primitive(_.setObject(a, b))
    override def setObject(a: String, b: AnyRef, c: Int) = primitive(_.setObject(a, b, c))
    override def setObject(a: String, b: AnyRef, c: Int, d: Int) = primitive(_.setObject(a, b, c, d))
    override def setObject(a: String, b: AnyRef, c: SQLType) = primitive(_.setObject(a, b, c))
    override def setObject(a: String, b: AnyRef, c: SQLType, d: Int) = primitive(_.setObject(a, b, c, d))
    override def setPoolable(a: Boolean) = primitive(_.setPoolable(a))
    override def setQueryTimeout(a: Int) = primitive(_.setQueryTimeout(a))
    override def setRef(a: Int, b: Ref) = primitive(_.setRef(a, b))
    override def setRowId(a: Int, b: RowId) = primitive(_.setRowId(a, b))
    override def setRowId(a: String, b: RowId) = primitive(_.setRowId(a, b))
    override def setSQLXML(a: Int, b: SQLXML) = primitive(_.setSQLXML(a, b))
    override def setSQLXML(a: String, b: SQLXML) = primitive(_.setSQLXML(a, b))
    override def setShort(a: Int, b: Short) = primitive(_.setShort(a, b))
    override def setShort(a: String, b: Short) = primitive(_.setShort(a, b))
    override def setString(a: Int, b: String) = primitive(_.setString(a, b))
    override def setString(a: String, b: String) = primitive(_.setString(a, b))
    override def setTime(a: Int, b: Time) = primitive(_.setTime(a, b))
    override def setTime(a: Int, b: Time, c: Calendar) = primitive(_.setTime(a, b, c))
    override def setTime(a: String, b: Time) = primitive(_.setTime(a, b))
    override def setTime(a: String, b: Time, c: Calendar) = primitive(_.setTime(a, b, c))
    override def setTimestamp(a: Int, b: Timestamp) = primitive(_.setTimestamp(a, b))
    override def setTimestamp(a: Int, b: Timestamp, c: Calendar) = primitive(_.setTimestamp(a, b, c))
    override def setTimestamp(a: String, b: Timestamp) = primitive(_.setTimestamp(a, b))
    override def setTimestamp(a: String, b: Timestamp, c: Calendar) = primitive(_.setTimestamp(a, b, c))
    override def setURL(a: Int, b: URL) = primitive(_.setURL(a, b))
    override def setURL(a: String, b: URL) = primitive(_.setURL(a, b))
    override def unwrap[T](a: Class[T]) = primitive(_.unwrap(a))
    override def wasNull: Kleisli[M, CallableStatement, Boolean] = primitive(_.wasNull)

  }

  trait ResultSetInterpreter extends ResultSetOp.Visitor[Kleisli[M, ResultSet, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](f: ResultSet => A): Kleisli[M, ResultSet, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, ResultSet, A] = outer.embed(e)
    override def raiseError[A](e: Throwable): Kleisli[M, ResultSet, A] = outer.raiseError(e)
    override def monotonic: Kleisli[M, ResultSet, FiniteDuration] = outer.monotonic[ResultSet]
    override def realTime: Kleisli[M, ResultSet, FiniteDuration] = outer.realTime[ResultSet]
    override def delay[A](thunk: => A): Kleisli[M, ResultSet, A] = outer.delay(thunk)
    override def suspend[A](hint: Sync.Type)(thunk: => A): Kleisli[M, ResultSet, A] = outer.suspend(hint)(thunk)
    override def canceled: Kleisli[M, ResultSet, Unit] = outer.canceled[ResultSet]

    override def performLogging(event: LogEvent): Kleisli[M, ResultSet, Unit] = Kleisli(_ => logHandler.run(event))

    // for operations using ResultSetIO we must call ourself recursively
    override def handleErrorWith[A](fa: ResultSetIO[A])(f: Throwable => ResultSetIO[A]): Kleisli[M, ResultSet, A] = outer.handleErrorWith(this)(fa)(f)
    override def forceR[A, B](fa: ResultSetIO[A])(fb: ResultSetIO[B]): Kleisli[M, ResultSet, B] = outer.forceR(this)(fa)(fb)
    override def uncancelable[A](body: Poll[ResultSetIO] => ResultSetIO[A]): Kleisli[M, ResultSet, A] = outer.uncancelable(this, doobie.free.resultset.capturePoll)(body)
    override def poll[A](poll: Any, fa: ResultSetIO[A]): Kleisli[M, ResultSet, A] = outer.poll(this)(poll, fa)
    override def onCancel[A](fa: ResultSetIO[A], fin: ResultSetIO[Unit]): Kleisli[M, ResultSet, A] = outer.onCancel(this)(fa, fin)
    override def fromFuture[A](fut: ResultSetIO[Future[A]]): Kleisli[M, ResultSet, A] = outer.fromFuture(this)(fut)
    override def fromFutureCancelable[A](fut: ResultSetIO[(Future[A], ResultSetIO[Unit])]): Kleisli[M, ResultSet, A] = outer.fromFutureCancelable(this)(fut)
    override def cancelable[A](fa: ResultSetIO[A], fin: ResultSetIO[Unit]): Kleisli[M, ResultSet, A] = outer.cancelable(this)(fa, fin)


    // domain-specific operations are implemented in terms of `primitive`
    override def absolute(a: Int) = primitive(_.absolute(a))
    override def afterLast: Kleisli[M, ResultSet, Unit] = primitive(_.afterLast)
    override def beforeFirst: Kleisli[M, ResultSet, Unit] = primitive(_.beforeFirst)
    override def cancelRowUpdates: Kleisli[M, ResultSet, Unit] = primitive(_.cancelRowUpdates)
    override def clearWarnings: Kleisli[M, ResultSet, Unit] = primitive(_.clearWarnings)
    override def close: Kleisli[M, ResultSet, Unit] = primitive(_.close)
    override def deleteRow: Kleisli[M, ResultSet, Unit] = primitive(_.deleteRow)
    override def findColumn(a: String) = primitive(_.findColumn(a))
    override def first: Kleisli[M, ResultSet, Boolean] = primitive(_.first)
    override def getArray(a: Int) = primitive(_.getArray(a))
    override def getArray(a: String) = primitive(_.getArray(a))
    override def getAsciiStream(a: Int) = primitive(_.getAsciiStream(a))
    override def getAsciiStream(a: String) = primitive(_.getAsciiStream(a))
    override def getBigDecimal(a: Int) = primitive(_.getBigDecimal(a))
    override def getBigDecimal(a: String) = primitive(_.getBigDecimal(a))
    override def getBinaryStream(a: Int) = primitive(_.getBinaryStream(a))
    override def getBinaryStream(a: String) = primitive(_.getBinaryStream(a))
    override def getBlob(a: Int) = primitive(_.getBlob(a))
    override def getBlob(a: String) = primitive(_.getBlob(a))
    override def getBoolean(a: Int) = primitive(_.getBoolean(a))
    override def getBoolean(a: String) = primitive(_.getBoolean(a))
    override def getByte(a: Int) = primitive(_.getByte(a))
    override def getByte(a: String) = primitive(_.getByte(a))
    override def getBytes(a: Int) = primitive(_.getBytes(a))
    override def getBytes(a: String) = primitive(_.getBytes(a))
    override def getCharacterStream(a: Int) = primitive(_.getCharacterStream(a))
    override def getCharacterStream(a: String) = primitive(_.getCharacterStream(a))
    override def getClob(a: Int) = primitive(_.getClob(a))
    override def getClob(a: String) = primitive(_.getClob(a))
    override def getConcurrency: Kleisli[M, ResultSet, Int] = primitive(_.getConcurrency)
    override def getCursorName: Kleisli[M, ResultSet, String] = primitive(_.getCursorName)
    override def getDate(a: Int) = primitive(_.getDate(a))
    override def getDate(a: Int, b: Calendar) = primitive(_.getDate(a, b))
    override def getDate(a: String) = primitive(_.getDate(a))
    override def getDate(a: String, b: Calendar) = primitive(_.getDate(a, b))
    override def getDouble(a: Int) = primitive(_.getDouble(a))
    override def getDouble(a: String) = primitive(_.getDouble(a))
    override def getFetchDirection: Kleisli[M, ResultSet, Int] = primitive(_.getFetchDirection)
    override def getFetchSize: Kleisli[M, ResultSet, Int] = primitive(_.getFetchSize)
    override def getFloat(a: Int) = primitive(_.getFloat(a))
    override def getFloat(a: String) = primitive(_.getFloat(a))
    override def getHoldability: Kleisli[M, ResultSet, Int] = primitive(_.getHoldability)
    override def getInt(a: Int) = primitive(_.getInt(a))
    override def getInt(a: String) = primitive(_.getInt(a))
    override def getLong(a: Int) = primitive(_.getLong(a))
    override def getLong(a: String) = primitive(_.getLong(a))
    override def getMetaData: Kleisli[M, ResultSet, ResultSetMetaData] = primitive(_.getMetaData)
    override def getNCharacterStream(a: Int) = primitive(_.getNCharacterStream(a))
    override def getNCharacterStream(a: String) = primitive(_.getNCharacterStream(a))
    override def getNClob(a: Int) = primitive(_.getNClob(a))
    override def getNClob(a: String) = primitive(_.getNClob(a))
    override def getNString(a: Int) = primitive(_.getNString(a))
    override def getNString(a: String) = primitive(_.getNString(a))
    override def getObject(a: Int) = primitive(_.getObject(a))
    override def getObject[T](a: Int, b: Class[T]) = primitive(_.getObject(a, b))
    override def getObject(a: Int, b: java.util.Map[String, Class[?]]) = primitive(_.getObject(a, b))
    override def getObject(a: String) = primitive(_.getObject(a))
    override def getObject[T](a: String, b: Class[T]) = primitive(_.getObject(a, b))
    override def getObject(a: String, b: java.util.Map[String, Class[?]]) = primitive(_.getObject(a, b))
    override def getRef(a: Int) = primitive(_.getRef(a))
    override def getRef(a: String) = primitive(_.getRef(a))
    override def getRow: Kleisli[M, ResultSet, Int] = primitive(_.getRow)
    override def getRowId(a: Int) = primitive(_.getRowId(a))
    override def getRowId(a: String) = primitive(_.getRowId(a))
    override def getSQLXML(a: Int) = primitive(_.getSQLXML(a))
    override def getSQLXML(a: String) = primitive(_.getSQLXML(a))
    override def getShort(a: Int) = primitive(_.getShort(a))
    override def getShort(a: String) = primitive(_.getShort(a))
    override def getStatement: Kleisli[M, ResultSet, Statement] = primitive(_.getStatement)
    override def getString(a: Int) = primitive(_.getString(a))
    override def getString(a: String) = primitive(_.getString(a))
    override def getTime(a: Int) = primitive(_.getTime(a))
    override def getTime(a: Int, b: Calendar) = primitive(_.getTime(a, b))
    override def getTime(a: String) = primitive(_.getTime(a))
    override def getTime(a: String, b: Calendar) = primitive(_.getTime(a, b))
    override def getTimestamp(a: Int) = primitive(_.getTimestamp(a))
    override def getTimestamp(a: Int, b: Calendar) = primitive(_.getTimestamp(a, b))
    override def getTimestamp(a: String) = primitive(_.getTimestamp(a))
    override def getTimestamp(a: String, b: Calendar) = primitive(_.getTimestamp(a, b))
    override def getType: Kleisli[M, ResultSet, Int] = primitive(_.getType)
    override def getURL(a: Int) = primitive(_.getURL(a))
    override def getURL(a: String) = primitive(_.getURL(a))
    override def getWarnings: Kleisli[M, ResultSet, SQLWarning] = primitive(_.getWarnings)
    override def insertRow: Kleisli[M, ResultSet, Unit] = primitive(_.insertRow)
    override def isAfterLast: Kleisli[M, ResultSet, Boolean] = primitive(_.isAfterLast)
    override def isBeforeFirst: Kleisli[M, ResultSet, Boolean] = primitive(_.isBeforeFirst)
    override def isClosed: Kleisli[M, ResultSet, Boolean] = primitive(_.isClosed)
    override def isFirst: Kleisli[M, ResultSet, Boolean] = primitive(_.isFirst)
    override def isLast: Kleisli[M, ResultSet, Boolean] = primitive(_.isLast)
    override def isWrapperFor(a: Class[?]) = primitive(_.isWrapperFor(a))
    override def last: Kleisli[M, ResultSet, Boolean] = primitive(_.last)
    override def moveToCurrentRow: Kleisli[M, ResultSet, Unit] = primitive(_.moveToCurrentRow)
    override def moveToInsertRow: Kleisli[M, ResultSet, Unit] = primitive(_.moveToInsertRow)
    override def next: Kleisli[M, ResultSet, Boolean] = primitive(_.next)
    override def previous: Kleisli[M, ResultSet, Boolean] = primitive(_.previous)
    override def refreshRow: Kleisli[M, ResultSet, Unit] = primitive(_.refreshRow)
    override def relative(a: Int) = primitive(_.relative(a))
    override def rowDeleted: Kleisli[M, ResultSet, Boolean] = primitive(_.rowDeleted)
    override def rowInserted: Kleisli[M, ResultSet, Boolean] = primitive(_.rowInserted)
    override def rowUpdated: Kleisli[M, ResultSet, Boolean] = primitive(_.rowUpdated)
    override def setFetchDirection(a: Int) = primitive(_.setFetchDirection(a))
    override def setFetchSize(a: Int) = primitive(_.setFetchSize(a))
    override def unwrap[T](a: Class[T]) = primitive(_.unwrap(a))
    override def updateArray(a: Int, b: SqlArray) = primitive(_.updateArray(a, b))
    override def updateArray(a: String, b: SqlArray) = primitive(_.updateArray(a, b))
    override def updateAsciiStream(a: Int, b: InputStream) = primitive(_.updateAsciiStream(a, b))
    override def updateAsciiStream(a: Int, b: InputStream, c: Int) = primitive(_.updateAsciiStream(a, b, c))
    override def updateAsciiStream(a: Int, b: InputStream, c: Long) = primitive(_.updateAsciiStream(a, b, c))
    override def updateAsciiStream(a: String, b: InputStream) = primitive(_.updateAsciiStream(a, b))
    override def updateAsciiStream(a: String, b: InputStream, c: Int) = primitive(_.updateAsciiStream(a, b, c))
    override def updateAsciiStream(a: String, b: InputStream, c: Long) = primitive(_.updateAsciiStream(a, b, c))
    override def updateBigDecimal(a: Int, b: BigDecimal) = primitive(_.updateBigDecimal(a, b))
    override def updateBigDecimal(a: String, b: BigDecimal) = primitive(_.updateBigDecimal(a, b))
    override def updateBinaryStream(a: Int, b: InputStream) = primitive(_.updateBinaryStream(a, b))
    override def updateBinaryStream(a: Int, b: InputStream, c: Int) = primitive(_.updateBinaryStream(a, b, c))
    override def updateBinaryStream(a: Int, b: InputStream, c: Long) = primitive(_.updateBinaryStream(a, b, c))
    override def updateBinaryStream(a: String, b: InputStream) = primitive(_.updateBinaryStream(a, b))
    override def updateBinaryStream(a: String, b: InputStream, c: Int) = primitive(_.updateBinaryStream(a, b, c))
    override def updateBinaryStream(a: String, b: InputStream, c: Long) = primitive(_.updateBinaryStream(a, b, c))
    override def updateBlob(a: Int, b: Blob) = primitive(_.updateBlob(a, b))
    override def updateBlob(a: Int, b: InputStream) = primitive(_.updateBlob(a, b))
    override def updateBlob(a: Int, b: InputStream, c: Long) = primitive(_.updateBlob(a, b, c))
    override def updateBlob(a: String, b: Blob) = primitive(_.updateBlob(a, b))
    override def updateBlob(a: String, b: InputStream) = primitive(_.updateBlob(a, b))
    override def updateBlob(a: String, b: InputStream, c: Long) = primitive(_.updateBlob(a, b, c))
    override def updateBoolean(a: Int, b: Boolean) = primitive(_.updateBoolean(a, b))
    override def updateBoolean(a: String, b: Boolean) = primitive(_.updateBoolean(a, b))
    override def updateByte(a: Int, b: Byte) = primitive(_.updateByte(a, b))
    override def updateByte(a: String, b: Byte) = primitive(_.updateByte(a, b))
    override def updateBytes(a: Int, b: Array[Byte]) = primitive(_.updateBytes(a, b))
    override def updateBytes(a: String, b: Array[Byte]) = primitive(_.updateBytes(a, b))
    override def updateCharacterStream(a: Int, b: Reader) = primitive(_.updateCharacterStream(a, b))
    override def updateCharacterStream(a: Int, b: Reader, c: Int) = primitive(_.updateCharacterStream(a, b, c))
    override def updateCharacterStream(a: Int, b: Reader, c: Long) = primitive(_.updateCharacterStream(a, b, c))
    override def updateCharacterStream(a: String, b: Reader) = primitive(_.updateCharacterStream(a, b))
    override def updateCharacterStream(a: String, b: Reader, c: Int) = primitive(_.updateCharacterStream(a, b, c))
    override def updateCharacterStream(a: String, b: Reader, c: Long) = primitive(_.updateCharacterStream(a, b, c))
    override def updateClob(a: Int, b: Clob) = primitive(_.updateClob(a, b))
    override def updateClob(a: Int, b: Reader) = primitive(_.updateClob(a, b))
    override def updateClob(a: Int, b: Reader, c: Long) = primitive(_.updateClob(a, b, c))
    override def updateClob(a: String, b: Clob) = primitive(_.updateClob(a, b))
    override def updateClob(a: String, b: Reader) = primitive(_.updateClob(a, b))
    override def updateClob(a: String, b: Reader, c: Long) = primitive(_.updateClob(a, b, c))
    override def updateDate(a: Int, b: Date) = primitive(_.updateDate(a, b))
    override def updateDate(a: String, b: Date) = primitive(_.updateDate(a, b))
    override def updateDouble(a: Int, b: Double) = primitive(_.updateDouble(a, b))
    override def updateDouble(a: String, b: Double) = primitive(_.updateDouble(a, b))
    override def updateFloat(a: Int, b: Float) = primitive(_.updateFloat(a, b))
    override def updateFloat(a: String, b: Float) = primitive(_.updateFloat(a, b))
    override def updateInt(a: Int, b: Int) = primitive(_.updateInt(a, b))
    override def updateInt(a: String, b: Int) = primitive(_.updateInt(a, b))
    override def updateLong(a: Int, b: Long) = primitive(_.updateLong(a, b))
    override def updateLong(a: String, b: Long) = primitive(_.updateLong(a, b))
    override def updateNCharacterStream(a: Int, b: Reader) = primitive(_.updateNCharacterStream(a, b))
    override def updateNCharacterStream(a: Int, b: Reader, c: Long) = primitive(_.updateNCharacterStream(a, b, c))
    override def updateNCharacterStream(a: String, b: Reader) = primitive(_.updateNCharacterStream(a, b))
    override def updateNCharacterStream(a: String, b: Reader, c: Long) = primitive(_.updateNCharacterStream(a, b, c))
    override def updateNClob(a: Int, b: NClob) = primitive(_.updateNClob(a, b))
    override def updateNClob(a: Int, b: Reader) = primitive(_.updateNClob(a, b))
    override def updateNClob(a: Int, b: Reader, c: Long) = primitive(_.updateNClob(a, b, c))
    override def updateNClob(a: String, b: NClob) = primitive(_.updateNClob(a, b))
    override def updateNClob(a: String, b: Reader) = primitive(_.updateNClob(a, b))
    override def updateNClob(a: String, b: Reader, c: Long) = primitive(_.updateNClob(a, b, c))
    override def updateNString(a: Int, b: String) = primitive(_.updateNString(a, b))
    override def updateNString(a: String, b: String) = primitive(_.updateNString(a, b))
    override def updateNull(a: Int) = primitive(_.updateNull(a))
    override def updateNull(a: String) = primitive(_.updateNull(a))
    override def updateObject(a: Int, b: AnyRef) = primitive(_.updateObject(a, b))
    override def updateObject(a: Int, b: AnyRef, c: Int) = primitive(_.updateObject(a, b, c))
    override def updateObject(a: Int, b: AnyRef, c: SQLType) = primitive(_.updateObject(a, b, c))
    override def updateObject(a: Int, b: AnyRef, c: SQLType, d: Int) = primitive(_.updateObject(a, b, c, d))
    override def updateObject(a: String, b: AnyRef) = primitive(_.updateObject(a, b))
    override def updateObject(a: String, b: AnyRef, c: Int) = primitive(_.updateObject(a, b, c))
    override def updateObject(a: String, b: AnyRef, c: SQLType) = primitive(_.updateObject(a, b, c))
    override def updateObject(a: String, b: AnyRef, c: SQLType, d: Int) = primitive(_.updateObject(a, b, c, d))
    override def updateRef(a: Int, b: Ref) = primitive(_.updateRef(a, b))
    override def updateRef(a: String, b: Ref) = primitive(_.updateRef(a, b))
    override def updateRow: Kleisli[M, ResultSet, Unit] = primitive(_.updateRow)
    override def updateRowId(a: Int, b: RowId) = primitive(_.updateRowId(a, b))
    override def updateRowId(a: String, b: RowId) = primitive(_.updateRowId(a, b))
    override def updateSQLXML(a: Int, b: SQLXML) = primitive(_.updateSQLXML(a, b))
    override def updateSQLXML(a: String, b: SQLXML) = primitive(_.updateSQLXML(a, b))
    override def updateShort(a: Int, b: Short) = primitive(_.updateShort(a, b))
    override def updateShort(a: String, b: Short) = primitive(_.updateShort(a, b))
    override def updateString(a: Int, b: String) = primitive(_.updateString(a, b))
    override def updateString(a: String, b: String) = primitive(_.updateString(a, b))
    override def updateTime(a: Int, b: Time) = primitive(_.updateTime(a, b))
    override def updateTime(a: String, b: Time) = primitive(_.updateTime(a, b))
    override def updateTimestamp(a: Int, b: Timestamp) = primitive(_.updateTimestamp(a, b))
    override def updateTimestamp(a: String, b: Timestamp) = primitive(_.updateTimestamp(a, b))
    override def wasNull: Kleisli[M, ResultSet, Boolean] = primitive(_.wasNull)

  }


}

