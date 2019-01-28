// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.free

// Library imports
import cats.~>
import cats.data.Kleisli
import cats.effect.{ Async, ContextShift, ExitCase }
import scala.concurrent.ExecutionContext

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
import java.sql.PreparedStatement
import java.sql.Ref
import java.sql.ResultSet
import java.sql.RowId
import java.sql.SQLData
import java.sql.SQLInput
import java.sql.SQLOutput
import java.sql.SQLType
import java.sql.SQLXML
import java.sql.Savepoint
import java.sql.Statement
import java.sql.Struct
import java.sql.Time
import java.sql.Timestamp
import java.sql.{ Array => SqlArray }
import java.util.Calendar
import java.util.Map
import java.util.Properties
import java.util.concurrent.Executor

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

  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  def apply[M[_]](
    implicit am: Async[M],
             cs: ContextShift[M]
  ): KleisliInterpreter[M] =
    new KleisliInterpreter[M] {
      val asyncM = am
      val contextShiftM = cs
    }

}

// Family of interpreters into Kleisli arrows for some monad M.
@com.github.ghik.silencer.silent // deprecations, unused variables, etc.
trait KleisliInterpreter[M[_]] { outer =>

  implicit val asyncM: Async[M]

  // We need these things in order to provide ContextShift[ConnectionIO] and so on, and also
  // to support shifting blocking operations to another pool.
  val contextShiftM: ContextShift[M]

  // The 14 interpreters, with definitions below. These can be overridden to customize behavior.
  lazy val NClobInterpreter: NClobOp ~> Kleisli[M, Env[NClob], ?] = new NClobInterpreter { }
  lazy val BlobInterpreter: BlobOp ~> Kleisli[M, Env[Blob], ?] = new BlobInterpreter { }
  lazy val ClobInterpreter: ClobOp ~> Kleisli[M, Env[Clob], ?] = new ClobInterpreter { }
  lazy val DatabaseMetaDataInterpreter: DatabaseMetaDataOp ~> Kleisli[M, Env[DatabaseMetaData], ?] = new DatabaseMetaDataInterpreter { }
  lazy val DriverInterpreter: DriverOp ~> Kleisli[M, Env[Driver], ?] = new DriverInterpreter { }
  lazy val RefInterpreter: RefOp ~> Kleisli[M, Env[Ref], ?] = new RefInterpreter { }
  lazy val SQLDataInterpreter: SQLDataOp ~> Kleisli[M, Env[SQLData], ?] = new SQLDataInterpreter { }
  lazy val SQLInputInterpreter: SQLInputOp ~> Kleisli[M, Env[SQLInput], ?] = new SQLInputInterpreter { }
  lazy val SQLOutputInterpreter: SQLOutputOp ~> Kleisli[M, Env[SQLOutput], ?] = new SQLOutputInterpreter { }
  lazy val ConnectionInterpreter: ConnectionOp ~> Kleisli[M, Env[Connection], ?] = new ConnectionInterpreter { }
  lazy val StatementInterpreter: StatementOp ~> Kleisli[M, Env[Statement], ?] = new StatementInterpreter { }
  lazy val PreparedStatementInterpreter: PreparedStatementOp ~> Kleisli[M, Env[PreparedStatement], ?] = new PreparedStatementInterpreter { }
  lazy val CallableStatementInterpreter: CallableStatementOp ~> Kleisli[M, Env[CallableStatement], ?] = new CallableStatementInterpreter { }
  lazy val ResultSetInterpreter: ResultSetOp ~> Kleisli[M, Env[ResultSet], ?] = new ResultSetInterpreter { }

  // Some methods are common to all interpreters and can be overridden to change behavior globally.
  def primitive[J, A](f: J => A, name: String, args: Any*): Kleisli[M, Env[J], A] =
    raw(_.unsafeTrace(s"$name(${args.mkString(", ")})")(f))

  def delay[J, A](a: () => A): Kleisli[M, Env[J], A] =
    Kleisli(_ => asyncM.delay(a()))

  def raw[J, A](f: Env[J] => A): Kleisli[M, Env[J], A] =
    Kleisli(e => contextShiftM.evalOn(e.blockingContext)(asyncM.delay(f(e))))

  def async[J, A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, Env[J], A] =
    Kleisli(_ => asyncM.async(k))

  def embed[J, A](e: Embedded[A]): Kleisli[M, Env[J], A] =
    e match {
      case Embedded.NClob(j, fa) => Kleisli(e => fa.foldMap(NClobInterpreter).run(e.copy(jdbc = j)))
      case Embedded.Blob(j, fa) => Kleisli(e => fa.foldMap(BlobInterpreter).run(e.copy(jdbc = j)))
      case Embedded.Clob(j, fa) => Kleisli(e => fa.foldMap(ClobInterpreter).run(e.copy(jdbc = j)))
      case Embedded.DatabaseMetaData(j, fa) => Kleisli(e => fa.foldMap(DatabaseMetaDataInterpreter).run(e.copy(jdbc = j)))
      case Embedded.Driver(j, fa) => Kleisli(e => fa.foldMap(DriverInterpreter).run(e.copy(jdbc = j)))
      case Embedded.Ref(j, fa) => Kleisli(e => fa.foldMap(RefInterpreter).run(e.copy(jdbc = j)))
      case Embedded.SQLData(j, fa) => Kleisli(e => fa.foldMap(SQLDataInterpreter).run(e.copy(jdbc = j)))
      case Embedded.SQLInput(j, fa) => Kleisli(e => fa.foldMap(SQLInputInterpreter).run(e.copy(jdbc = j)))
      case Embedded.SQLOutput(j, fa) => Kleisli(e => fa.foldMap(SQLOutputInterpreter).run(e.copy(jdbc = j)))
      case Embedded.Connection(j, fa) => Kleisli(e => fa.foldMap(ConnectionInterpreter).run(e.copy(jdbc = j)))
      case Embedded.Statement(j, fa) => Kleisli(e => fa.foldMap(StatementInterpreter).run(e.copy(jdbc = j)))
      case Embedded.PreparedStatement(j, fa) => Kleisli(e => fa.foldMap(PreparedStatementInterpreter).run(e.copy(jdbc = j)))
      case Embedded.CallableStatement(j, fa) => Kleisli(e => fa.foldMap(CallableStatementInterpreter).run(e.copy(jdbc = j)))
      case Embedded.ResultSet(j, fa) => Kleisli(e => fa.foldMap(ResultSetInterpreter).run(e.copy(jdbc = j)))
    }

  // Interpreters
  trait NClobInterpreter extends NClobOp.Visitor[Kleisli[M, Env[NClob], ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: Env[NClob] => A): Kleisli[M, Env[NClob], A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Env[NClob], A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, Env[NClob], A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, Env[NClob], A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => NClobIO[Unit]): Kleisli[M, Env[NClob], A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: NClobIO[A], f: Throwable => NClobIO[A]): Kleisli[M, Env[NClob], A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: NClobIO[A])(use: A => NClobIO[B])(release: (A, ExitCase[Throwable]) => NClobIO[Unit]): Kleisli[M, Env[NClob], B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, Env[NClob], Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: NClobIO[A]): Kleisli[M, Env[NClob], A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    def liftE[G[_]](env: Env[NClob] => G ~> NClobIO): Kleisli[M, Env[NClob], G ~> NClobIO] =
      Kleisli(e => asyncM.pure(env(e)))

    // domain-specific operations are implemented in terms of `primitive`
    override def free = primitive(_.free, "free")
    override def getAsciiStream = primitive(_.getAsciiStream, "getAsciiStream")
    override def getCharacterStream = primitive(_.getCharacterStream, "getCharacterStream")
    override def getCharacterStream(a: Long, b: Long) = primitive(_.getCharacterStream(a, b), "getCharacterStream", a, b)
    override def getSubString(a: Long, b: Int) = primitive(_.getSubString(a, b), "getSubString", a, b)
    override def length = primitive(_.length, "length")
    override def position(a: Clob, b: Long) = primitive(_.position(a, b), "position", a, b)
    override def position(a: String, b: Long) = primitive(_.position(a, b), "position", a, b)
    override def setAsciiStream(a: Long) = primitive(_.setAsciiStream(a), "setAsciiStream", a)
    override def setCharacterStream(a: Long) = primitive(_.setCharacterStream(a), "setCharacterStream", a)
    override def setString(a: Long, b: String) = primitive(_.setString(a, b), "setString", a, b)
    override def setString(a: Long, b: String, c: Int, d: Int) = primitive(_.setString(a, b, c, d), "setString", a, b, c, d)
    override def truncate(a: Long) = primitive(_.truncate(a), "truncate", a)

  }

  trait BlobInterpreter extends BlobOp.Visitor[Kleisli[M, Env[Blob], ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: Env[Blob] => A): Kleisli[M, Env[Blob], A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Env[Blob], A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, Env[Blob], A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, Env[Blob], A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => BlobIO[Unit]): Kleisli[M, Env[Blob], A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: BlobIO[A], f: Throwable => BlobIO[A]): Kleisli[M, Env[Blob], A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: BlobIO[A])(use: A => BlobIO[B])(release: (A, ExitCase[Throwable]) => BlobIO[Unit]): Kleisli[M, Env[Blob], B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, Env[Blob], Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: BlobIO[A]): Kleisli[M, Env[Blob], A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    def liftE[G[_]](env: Env[Blob] => G ~> BlobIO): Kleisli[M, Env[Blob], G ~> BlobIO] =
      Kleisli(e => asyncM.pure(env(e)))

    // domain-specific operations are implemented in terms of `primitive`
    override def free = primitive(_.free, "free")
    override def getBinaryStream = primitive(_.getBinaryStream, "getBinaryStream")
    override def getBinaryStream(a: Long, b: Long) = primitive(_.getBinaryStream(a, b), "getBinaryStream", a, b)
    override def getBytes(a: Long, b: Int) = primitive(_.getBytes(a, b), "getBytes", a, b)
    override def length = primitive(_.length, "length")
    override def position(a: Array[Byte], b: Long) = primitive(_.position(a, b), "position", a, b)
    override def position(a: Blob, b: Long) = primitive(_.position(a, b), "position", a, b)
    override def setBinaryStream(a: Long) = primitive(_.setBinaryStream(a), "setBinaryStream", a)
    override def setBytes(a: Long, b: Array[Byte]) = primitive(_.setBytes(a, b), "setBytes", a, b)
    override def setBytes(a: Long, b: Array[Byte], c: Int, d: Int) = primitive(_.setBytes(a, b, c, d), "setBytes", a, b, c, d)
    override def truncate(a: Long) = primitive(_.truncate(a), "truncate", a)

  }

  trait ClobInterpreter extends ClobOp.Visitor[Kleisli[M, Env[Clob], ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: Env[Clob] => A): Kleisli[M, Env[Clob], A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Env[Clob], A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, Env[Clob], A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, Env[Clob], A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => ClobIO[Unit]): Kleisli[M, Env[Clob], A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: ClobIO[A], f: Throwable => ClobIO[A]): Kleisli[M, Env[Clob], A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: ClobIO[A])(use: A => ClobIO[B])(release: (A, ExitCase[Throwable]) => ClobIO[Unit]): Kleisli[M, Env[Clob], B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, Env[Clob], Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: ClobIO[A]): Kleisli[M, Env[Clob], A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    def liftE[G[_]](env: Env[Clob] => G ~> ClobIO): Kleisli[M, Env[Clob], G ~> ClobIO] =
      Kleisli(e => asyncM.pure(env(e)))

    // domain-specific operations are implemented in terms of `primitive`
    override def free = primitive(_.free, "free")
    override def getAsciiStream = primitive(_.getAsciiStream, "getAsciiStream")
    override def getCharacterStream = primitive(_.getCharacterStream, "getCharacterStream")
    override def getCharacterStream(a: Long, b: Long) = primitive(_.getCharacterStream(a, b), "getCharacterStream", a, b)
    override def getSubString(a: Long, b: Int) = primitive(_.getSubString(a, b), "getSubString", a, b)
    override def length = primitive(_.length, "length")
    override def position(a: Clob, b: Long) = primitive(_.position(a, b), "position", a, b)
    override def position(a: String, b: Long) = primitive(_.position(a, b), "position", a, b)
    override def setAsciiStream(a: Long) = primitive(_.setAsciiStream(a), "setAsciiStream", a)
    override def setCharacterStream(a: Long) = primitive(_.setCharacterStream(a), "setCharacterStream", a)
    override def setString(a: Long, b: String) = primitive(_.setString(a, b), "setString", a, b)
    override def setString(a: Long, b: String, c: Int, d: Int) = primitive(_.setString(a, b, c, d), "setString", a, b, c, d)
    override def truncate(a: Long) = primitive(_.truncate(a), "truncate", a)

  }

  trait DatabaseMetaDataInterpreter extends DatabaseMetaDataOp.Visitor[Kleisli[M, Env[DatabaseMetaData], ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: Env[DatabaseMetaData] => A): Kleisli[M, Env[DatabaseMetaData], A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Env[DatabaseMetaData], A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, Env[DatabaseMetaData], A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, Env[DatabaseMetaData], A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => DatabaseMetaDataIO[Unit]): Kleisli[M, Env[DatabaseMetaData], A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: DatabaseMetaDataIO[A], f: Throwable => DatabaseMetaDataIO[A]): Kleisli[M, Env[DatabaseMetaData], A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: DatabaseMetaDataIO[A])(use: A => DatabaseMetaDataIO[B])(release: (A, ExitCase[Throwable]) => DatabaseMetaDataIO[Unit]): Kleisli[M, Env[DatabaseMetaData], B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, Env[DatabaseMetaData], Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: DatabaseMetaDataIO[A]): Kleisli[M, Env[DatabaseMetaData], A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    def liftE[G[_]](env: Env[DatabaseMetaData] => G ~> DatabaseMetaDataIO): Kleisli[M, Env[DatabaseMetaData], G ~> DatabaseMetaDataIO] =
      Kleisli(e => asyncM.pure(env(e)))

    // domain-specific operations are implemented in terms of `primitive`
    override def allProceduresAreCallable = primitive(_.allProceduresAreCallable, "allProceduresAreCallable")
    override def allTablesAreSelectable = primitive(_.allTablesAreSelectable, "allTablesAreSelectable")
    override def autoCommitFailureClosesAllResultSets = primitive(_.autoCommitFailureClosesAllResultSets, "autoCommitFailureClosesAllResultSets")
    override def dataDefinitionCausesTransactionCommit = primitive(_.dataDefinitionCausesTransactionCommit, "dataDefinitionCausesTransactionCommit")
    override def dataDefinitionIgnoredInTransactions = primitive(_.dataDefinitionIgnoredInTransactions, "dataDefinitionIgnoredInTransactions")
    override def deletesAreDetected(a: Int) = primitive(_.deletesAreDetected(a), "deletesAreDetected", a)
    override def doesMaxRowSizeIncludeBlobs = primitive(_.doesMaxRowSizeIncludeBlobs, "doesMaxRowSizeIncludeBlobs")
    override def generatedKeyAlwaysReturned = primitive(_.generatedKeyAlwaysReturned, "generatedKeyAlwaysReturned")
    override def getAttributes(a: String, b: String, c: String, d: String) = primitive(_.getAttributes(a, b, c, d), "getAttributes", a, b, c, d)
    override def getBestRowIdentifier(a: String, b: String, c: String, d: Int, e: Boolean) = primitive(_.getBestRowIdentifier(a, b, c, d, e), "getBestRowIdentifier", a, b, c, d, e)
    override def getCatalogSeparator = primitive(_.getCatalogSeparator, "getCatalogSeparator")
    override def getCatalogTerm = primitive(_.getCatalogTerm, "getCatalogTerm")
    override def getCatalogs = primitive(_.getCatalogs, "getCatalogs")
    override def getClientInfoProperties = primitive(_.getClientInfoProperties, "getClientInfoProperties")
    override def getColumnPrivileges(a: String, b: String, c: String, d: String) = primitive(_.getColumnPrivileges(a, b, c, d), "getColumnPrivileges", a, b, c, d)
    override def getColumns(a: String, b: String, c: String, d: String) = primitive(_.getColumns(a, b, c, d), "getColumns", a, b, c, d)
    override def getConnection = primitive(_.getConnection, "getConnection")
    override def getCrossReference(a: String, b: String, c: String, d: String, e: String, f: String) = primitive(_.getCrossReference(a, b, c, d, e, f), "getCrossReference", a, b, c, d, e, f)
    override def getDatabaseMajorVersion = primitive(_.getDatabaseMajorVersion, "getDatabaseMajorVersion")
    override def getDatabaseMinorVersion = primitive(_.getDatabaseMinorVersion, "getDatabaseMinorVersion")
    override def getDatabaseProductName = primitive(_.getDatabaseProductName, "getDatabaseProductName")
    override def getDatabaseProductVersion = primitive(_.getDatabaseProductVersion, "getDatabaseProductVersion")
    override def getDefaultTransactionIsolation = primitive(_.getDefaultTransactionIsolation, "getDefaultTransactionIsolation")
    override def getDriverMajorVersion = primitive(_.getDriverMajorVersion, "getDriverMajorVersion")
    override def getDriverMinorVersion = primitive(_.getDriverMinorVersion, "getDriverMinorVersion")
    override def getDriverName = primitive(_.getDriverName, "getDriverName")
    override def getDriverVersion = primitive(_.getDriverVersion, "getDriverVersion")
    override def getExportedKeys(a: String, b: String, c: String) = primitive(_.getExportedKeys(a, b, c), "getExportedKeys", a, b, c)
    override def getExtraNameCharacters = primitive(_.getExtraNameCharacters, "getExtraNameCharacters")
    override def getFunctionColumns(a: String, b: String, c: String, d: String) = primitive(_.getFunctionColumns(a, b, c, d), "getFunctionColumns", a, b, c, d)
    override def getFunctions(a: String, b: String, c: String) = primitive(_.getFunctions(a, b, c), "getFunctions", a, b, c)
    override def getIdentifierQuoteString = primitive(_.getIdentifierQuoteString, "getIdentifierQuoteString")
    override def getImportedKeys(a: String, b: String, c: String) = primitive(_.getImportedKeys(a, b, c), "getImportedKeys", a, b, c)
    override def getIndexInfo(a: String, b: String, c: String, d: Boolean, e: Boolean) = primitive(_.getIndexInfo(a, b, c, d, e), "getIndexInfo", a, b, c, d, e)
    override def getJDBCMajorVersion = primitive(_.getJDBCMajorVersion, "getJDBCMajorVersion")
    override def getJDBCMinorVersion = primitive(_.getJDBCMinorVersion, "getJDBCMinorVersion")
    override def getMaxBinaryLiteralLength = primitive(_.getMaxBinaryLiteralLength, "getMaxBinaryLiteralLength")
    override def getMaxCatalogNameLength = primitive(_.getMaxCatalogNameLength, "getMaxCatalogNameLength")
    override def getMaxCharLiteralLength = primitive(_.getMaxCharLiteralLength, "getMaxCharLiteralLength")
    override def getMaxColumnNameLength = primitive(_.getMaxColumnNameLength, "getMaxColumnNameLength")
    override def getMaxColumnsInGroupBy = primitive(_.getMaxColumnsInGroupBy, "getMaxColumnsInGroupBy")
    override def getMaxColumnsInIndex = primitive(_.getMaxColumnsInIndex, "getMaxColumnsInIndex")
    override def getMaxColumnsInOrderBy = primitive(_.getMaxColumnsInOrderBy, "getMaxColumnsInOrderBy")
    override def getMaxColumnsInSelect = primitive(_.getMaxColumnsInSelect, "getMaxColumnsInSelect")
    override def getMaxColumnsInTable = primitive(_.getMaxColumnsInTable, "getMaxColumnsInTable")
    override def getMaxConnections = primitive(_.getMaxConnections, "getMaxConnections")
    override def getMaxCursorNameLength = primitive(_.getMaxCursorNameLength, "getMaxCursorNameLength")
    override def getMaxIndexLength = primitive(_.getMaxIndexLength, "getMaxIndexLength")
    override def getMaxLogicalLobSize = primitive(_.getMaxLogicalLobSize, "getMaxLogicalLobSize")
    override def getMaxProcedureNameLength = primitive(_.getMaxProcedureNameLength, "getMaxProcedureNameLength")
    override def getMaxRowSize = primitive(_.getMaxRowSize, "getMaxRowSize")
    override def getMaxSchemaNameLength = primitive(_.getMaxSchemaNameLength, "getMaxSchemaNameLength")
    override def getMaxStatementLength = primitive(_.getMaxStatementLength, "getMaxStatementLength")
    override def getMaxStatements = primitive(_.getMaxStatements, "getMaxStatements")
    override def getMaxTableNameLength = primitive(_.getMaxTableNameLength, "getMaxTableNameLength")
    override def getMaxTablesInSelect = primitive(_.getMaxTablesInSelect, "getMaxTablesInSelect")
    override def getMaxUserNameLength = primitive(_.getMaxUserNameLength, "getMaxUserNameLength")
    override def getNumericFunctions = primitive(_.getNumericFunctions, "getNumericFunctions")
    override def getPrimaryKeys(a: String, b: String, c: String) = primitive(_.getPrimaryKeys(a, b, c), "getPrimaryKeys", a, b, c)
    override def getProcedureColumns(a: String, b: String, c: String, d: String) = primitive(_.getProcedureColumns(a, b, c, d), "getProcedureColumns", a, b, c, d)
    override def getProcedureTerm = primitive(_.getProcedureTerm, "getProcedureTerm")
    override def getProcedures(a: String, b: String, c: String) = primitive(_.getProcedures(a, b, c), "getProcedures", a, b, c)
    override def getPseudoColumns(a: String, b: String, c: String, d: String) = primitive(_.getPseudoColumns(a, b, c, d), "getPseudoColumns", a, b, c, d)
    override def getResultSetHoldability = primitive(_.getResultSetHoldability, "getResultSetHoldability")
    override def getRowIdLifetime = primitive(_.getRowIdLifetime, "getRowIdLifetime")
    override def getSQLKeywords = primitive(_.getSQLKeywords, "getSQLKeywords")
    override def getSQLStateType = primitive(_.getSQLStateType, "getSQLStateType")
    override def getSchemaTerm = primitive(_.getSchemaTerm, "getSchemaTerm")
    override def getSchemas = primitive(_.getSchemas, "getSchemas")
    override def getSchemas(a: String, b: String) = primitive(_.getSchemas(a, b), "getSchemas", a, b)
    override def getSearchStringEscape = primitive(_.getSearchStringEscape, "getSearchStringEscape")
    override def getStringFunctions = primitive(_.getStringFunctions, "getStringFunctions")
    override def getSuperTables(a: String, b: String, c: String) = primitive(_.getSuperTables(a, b, c), "getSuperTables", a, b, c)
    override def getSuperTypes(a: String, b: String, c: String) = primitive(_.getSuperTypes(a, b, c), "getSuperTypes", a, b, c)
    override def getSystemFunctions = primitive(_.getSystemFunctions, "getSystemFunctions")
    override def getTablePrivileges(a: String, b: String, c: String) = primitive(_.getTablePrivileges(a, b, c), "getTablePrivileges", a, b, c)
    override def getTableTypes = primitive(_.getTableTypes, "getTableTypes")
    override def getTables(a: String, b: String, c: String, d: Array[String]) = primitive(_.getTables(a, b, c, d), "getTables", a, b, c, d)
    override def getTimeDateFunctions = primitive(_.getTimeDateFunctions, "getTimeDateFunctions")
    override def getTypeInfo = primitive(_.getTypeInfo, "getTypeInfo")
    override def getUDTs(a: String, b: String, c: String, d: Array[Int]) = primitive(_.getUDTs(a, b, c, d), "getUDTs", a, b, c, d)
    override def getURL = primitive(_.getURL, "getURL")
    override def getUserName = primitive(_.getUserName, "getUserName")
    override def getVersionColumns(a: String, b: String, c: String) = primitive(_.getVersionColumns(a, b, c), "getVersionColumns", a, b, c)
    override def insertsAreDetected(a: Int) = primitive(_.insertsAreDetected(a), "insertsAreDetected", a)
    override def isCatalogAtStart = primitive(_.isCatalogAtStart, "isCatalogAtStart")
    override def isReadOnly = primitive(_.isReadOnly, "isReadOnly")
    override def isWrapperFor(a: Class[_]) = primitive(_.isWrapperFor(a), "isWrapperFor", a)
    override def locatorsUpdateCopy = primitive(_.locatorsUpdateCopy, "locatorsUpdateCopy")
    override def nullPlusNonNullIsNull = primitive(_.nullPlusNonNullIsNull, "nullPlusNonNullIsNull")
    override def nullsAreSortedAtEnd = primitive(_.nullsAreSortedAtEnd, "nullsAreSortedAtEnd")
    override def nullsAreSortedAtStart = primitive(_.nullsAreSortedAtStart, "nullsAreSortedAtStart")
    override def nullsAreSortedHigh = primitive(_.nullsAreSortedHigh, "nullsAreSortedHigh")
    override def nullsAreSortedLow = primitive(_.nullsAreSortedLow, "nullsAreSortedLow")
    override def othersDeletesAreVisible(a: Int) = primitive(_.othersDeletesAreVisible(a), "othersDeletesAreVisible", a)
    override def othersInsertsAreVisible(a: Int) = primitive(_.othersInsertsAreVisible(a), "othersInsertsAreVisible", a)
    override def othersUpdatesAreVisible(a: Int) = primitive(_.othersUpdatesAreVisible(a), "othersUpdatesAreVisible", a)
    override def ownDeletesAreVisible(a: Int) = primitive(_.ownDeletesAreVisible(a), "ownDeletesAreVisible", a)
    override def ownInsertsAreVisible(a: Int) = primitive(_.ownInsertsAreVisible(a), "ownInsertsAreVisible", a)
    override def ownUpdatesAreVisible(a: Int) = primitive(_.ownUpdatesAreVisible(a), "ownUpdatesAreVisible", a)
    override def storesLowerCaseIdentifiers = primitive(_.storesLowerCaseIdentifiers, "storesLowerCaseIdentifiers")
    override def storesLowerCaseQuotedIdentifiers = primitive(_.storesLowerCaseQuotedIdentifiers, "storesLowerCaseQuotedIdentifiers")
    override def storesMixedCaseIdentifiers = primitive(_.storesMixedCaseIdentifiers, "storesMixedCaseIdentifiers")
    override def storesMixedCaseQuotedIdentifiers = primitive(_.storesMixedCaseQuotedIdentifiers, "storesMixedCaseQuotedIdentifiers")
    override def storesUpperCaseIdentifiers = primitive(_.storesUpperCaseIdentifiers, "storesUpperCaseIdentifiers")
    override def storesUpperCaseQuotedIdentifiers = primitive(_.storesUpperCaseQuotedIdentifiers, "storesUpperCaseQuotedIdentifiers")
    override def supportsANSI92EntryLevelSQL = primitive(_.supportsANSI92EntryLevelSQL, "supportsANSI92EntryLevelSQL")
    override def supportsANSI92FullSQL = primitive(_.supportsANSI92FullSQL, "supportsANSI92FullSQL")
    override def supportsANSI92IntermediateSQL = primitive(_.supportsANSI92IntermediateSQL, "supportsANSI92IntermediateSQL")
    override def supportsAlterTableWithAddColumn = primitive(_.supportsAlterTableWithAddColumn, "supportsAlterTableWithAddColumn")
    override def supportsAlterTableWithDropColumn = primitive(_.supportsAlterTableWithDropColumn, "supportsAlterTableWithDropColumn")
    override def supportsBatchUpdates = primitive(_.supportsBatchUpdates, "supportsBatchUpdates")
    override def supportsCatalogsInDataManipulation = primitive(_.supportsCatalogsInDataManipulation, "supportsCatalogsInDataManipulation")
    override def supportsCatalogsInIndexDefinitions = primitive(_.supportsCatalogsInIndexDefinitions, "supportsCatalogsInIndexDefinitions")
    override def supportsCatalogsInPrivilegeDefinitions = primitive(_.supportsCatalogsInPrivilegeDefinitions, "supportsCatalogsInPrivilegeDefinitions")
    override def supportsCatalogsInProcedureCalls = primitive(_.supportsCatalogsInProcedureCalls, "supportsCatalogsInProcedureCalls")
    override def supportsCatalogsInTableDefinitions = primitive(_.supportsCatalogsInTableDefinitions, "supportsCatalogsInTableDefinitions")
    override def supportsColumnAliasing = primitive(_.supportsColumnAliasing, "supportsColumnAliasing")
    override def supportsConvert = primitive(_.supportsConvert, "supportsConvert")
    override def supportsConvert(a: Int, b: Int) = primitive(_.supportsConvert(a, b), "supportsConvert", a, b)
    override def supportsCoreSQLGrammar = primitive(_.supportsCoreSQLGrammar, "supportsCoreSQLGrammar")
    override def supportsCorrelatedSubqueries = primitive(_.supportsCorrelatedSubqueries, "supportsCorrelatedSubqueries")
    override def supportsDataDefinitionAndDataManipulationTransactions = primitive(_.supportsDataDefinitionAndDataManipulationTransactions, "supportsDataDefinitionAndDataManipulationTransactions")
    override def supportsDataManipulationTransactionsOnly = primitive(_.supportsDataManipulationTransactionsOnly, "supportsDataManipulationTransactionsOnly")
    override def supportsDifferentTableCorrelationNames = primitive(_.supportsDifferentTableCorrelationNames, "supportsDifferentTableCorrelationNames")
    override def supportsExpressionsInOrderBy = primitive(_.supportsExpressionsInOrderBy, "supportsExpressionsInOrderBy")
    override def supportsExtendedSQLGrammar = primitive(_.supportsExtendedSQLGrammar, "supportsExtendedSQLGrammar")
    override def supportsFullOuterJoins = primitive(_.supportsFullOuterJoins, "supportsFullOuterJoins")
    override def supportsGetGeneratedKeys = primitive(_.supportsGetGeneratedKeys, "supportsGetGeneratedKeys")
    override def supportsGroupBy = primitive(_.supportsGroupBy, "supportsGroupBy")
    override def supportsGroupByBeyondSelect = primitive(_.supportsGroupByBeyondSelect, "supportsGroupByBeyondSelect")
    override def supportsGroupByUnrelated = primitive(_.supportsGroupByUnrelated, "supportsGroupByUnrelated")
    override def supportsIntegrityEnhancementFacility = primitive(_.supportsIntegrityEnhancementFacility, "supportsIntegrityEnhancementFacility")
    override def supportsLikeEscapeClause = primitive(_.supportsLikeEscapeClause, "supportsLikeEscapeClause")
    override def supportsLimitedOuterJoins = primitive(_.supportsLimitedOuterJoins, "supportsLimitedOuterJoins")
    override def supportsMinimumSQLGrammar = primitive(_.supportsMinimumSQLGrammar, "supportsMinimumSQLGrammar")
    override def supportsMixedCaseIdentifiers = primitive(_.supportsMixedCaseIdentifiers, "supportsMixedCaseIdentifiers")
    override def supportsMixedCaseQuotedIdentifiers = primitive(_.supportsMixedCaseQuotedIdentifiers, "supportsMixedCaseQuotedIdentifiers")
    override def supportsMultipleOpenResults = primitive(_.supportsMultipleOpenResults, "supportsMultipleOpenResults")
    override def supportsMultipleResultSets = primitive(_.supportsMultipleResultSets, "supportsMultipleResultSets")
    override def supportsMultipleTransactions = primitive(_.supportsMultipleTransactions, "supportsMultipleTransactions")
    override def supportsNamedParameters = primitive(_.supportsNamedParameters, "supportsNamedParameters")
    override def supportsNonNullableColumns = primitive(_.supportsNonNullableColumns, "supportsNonNullableColumns")
    override def supportsOpenCursorsAcrossCommit = primitive(_.supportsOpenCursorsAcrossCommit, "supportsOpenCursorsAcrossCommit")
    override def supportsOpenCursorsAcrossRollback = primitive(_.supportsOpenCursorsAcrossRollback, "supportsOpenCursorsAcrossRollback")
    override def supportsOpenStatementsAcrossCommit = primitive(_.supportsOpenStatementsAcrossCommit, "supportsOpenStatementsAcrossCommit")
    override def supportsOpenStatementsAcrossRollback = primitive(_.supportsOpenStatementsAcrossRollback, "supportsOpenStatementsAcrossRollback")
    override def supportsOrderByUnrelated = primitive(_.supportsOrderByUnrelated, "supportsOrderByUnrelated")
    override def supportsOuterJoins = primitive(_.supportsOuterJoins, "supportsOuterJoins")
    override def supportsPositionedDelete = primitive(_.supportsPositionedDelete, "supportsPositionedDelete")
    override def supportsPositionedUpdate = primitive(_.supportsPositionedUpdate, "supportsPositionedUpdate")
    override def supportsRefCursors = primitive(_.supportsRefCursors, "supportsRefCursors")
    override def supportsResultSetConcurrency(a: Int, b: Int) = primitive(_.supportsResultSetConcurrency(a, b), "supportsResultSetConcurrency", a, b)
    override def supportsResultSetHoldability(a: Int) = primitive(_.supportsResultSetHoldability(a), "supportsResultSetHoldability", a)
    override def supportsResultSetType(a: Int) = primitive(_.supportsResultSetType(a), "supportsResultSetType", a)
    override def supportsSavepoints = primitive(_.supportsSavepoints, "supportsSavepoints")
    override def supportsSchemasInDataManipulation = primitive(_.supportsSchemasInDataManipulation, "supportsSchemasInDataManipulation")
    override def supportsSchemasInIndexDefinitions = primitive(_.supportsSchemasInIndexDefinitions, "supportsSchemasInIndexDefinitions")
    override def supportsSchemasInPrivilegeDefinitions = primitive(_.supportsSchemasInPrivilegeDefinitions, "supportsSchemasInPrivilegeDefinitions")
    override def supportsSchemasInProcedureCalls = primitive(_.supportsSchemasInProcedureCalls, "supportsSchemasInProcedureCalls")
    override def supportsSchemasInTableDefinitions = primitive(_.supportsSchemasInTableDefinitions, "supportsSchemasInTableDefinitions")
    override def supportsSelectForUpdate = primitive(_.supportsSelectForUpdate, "supportsSelectForUpdate")
    override def supportsStatementPooling = primitive(_.supportsStatementPooling, "supportsStatementPooling")
    override def supportsStoredFunctionsUsingCallSyntax = primitive(_.supportsStoredFunctionsUsingCallSyntax, "supportsStoredFunctionsUsingCallSyntax")
    override def supportsStoredProcedures = primitive(_.supportsStoredProcedures, "supportsStoredProcedures")
    override def supportsSubqueriesInComparisons = primitive(_.supportsSubqueriesInComparisons, "supportsSubqueriesInComparisons")
    override def supportsSubqueriesInExists = primitive(_.supportsSubqueriesInExists, "supportsSubqueriesInExists")
    override def supportsSubqueriesInIns = primitive(_.supportsSubqueriesInIns, "supportsSubqueriesInIns")
    override def supportsSubqueriesInQuantifieds = primitive(_.supportsSubqueriesInQuantifieds, "supportsSubqueriesInQuantifieds")
    override def supportsTableCorrelationNames = primitive(_.supportsTableCorrelationNames, "supportsTableCorrelationNames")
    override def supportsTransactionIsolationLevel(a: Int) = primitive(_.supportsTransactionIsolationLevel(a), "supportsTransactionIsolationLevel", a)
    override def supportsTransactions = primitive(_.supportsTransactions, "supportsTransactions")
    override def supportsUnion = primitive(_.supportsUnion, "supportsUnion")
    override def supportsUnionAll = primitive(_.supportsUnionAll, "supportsUnionAll")
    override def unwrap[T](a: Class[T]) = primitive(_.unwrap(a), "unwrap", a)
    override def updatesAreDetected(a: Int) = primitive(_.updatesAreDetected(a), "updatesAreDetected", a)
    override def usesLocalFilePerTable = primitive(_.usesLocalFilePerTable, "usesLocalFilePerTable")
    override def usesLocalFiles = primitive(_.usesLocalFiles, "usesLocalFiles")

  }

  trait DriverInterpreter extends DriverOp.Visitor[Kleisli[M, Env[Driver], ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: Env[Driver] => A): Kleisli[M, Env[Driver], A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Env[Driver], A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, Env[Driver], A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, Env[Driver], A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => DriverIO[Unit]): Kleisli[M, Env[Driver], A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: DriverIO[A], f: Throwable => DriverIO[A]): Kleisli[M, Env[Driver], A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: DriverIO[A])(use: A => DriverIO[B])(release: (A, ExitCase[Throwable]) => DriverIO[Unit]): Kleisli[M, Env[Driver], B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, Env[Driver], Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: DriverIO[A]): Kleisli[M, Env[Driver], A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    def liftE[G[_]](env: Env[Driver] => G ~> DriverIO): Kleisli[M, Env[Driver], G ~> DriverIO] =
      Kleisli(e => asyncM.pure(env(e)))

    // domain-specific operations are implemented in terms of `primitive`
    override def acceptsURL(a: String) = primitive(_.acceptsURL(a), "acceptsURL", a)
    override def connect(a: String, b: Properties) = primitive(_.connect(a, b), "connect", a, b)
    override def getMajorVersion = primitive(_.getMajorVersion, "getMajorVersion")
    override def getMinorVersion = primitive(_.getMinorVersion, "getMinorVersion")
    override def getParentLogger = primitive(_.getParentLogger, "getParentLogger")
    override def getPropertyInfo(a: String, b: Properties) = primitive(_.getPropertyInfo(a, b), "getPropertyInfo", a, b)
    override def jdbcCompliant = primitive(_.jdbcCompliant, "jdbcCompliant")

  }

  trait RefInterpreter extends RefOp.Visitor[Kleisli[M, Env[Ref], ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: Env[Ref] => A): Kleisli[M, Env[Ref], A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Env[Ref], A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, Env[Ref], A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, Env[Ref], A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => RefIO[Unit]): Kleisli[M, Env[Ref], A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: RefIO[A], f: Throwable => RefIO[A]): Kleisli[M, Env[Ref], A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: RefIO[A])(use: A => RefIO[B])(release: (A, ExitCase[Throwable]) => RefIO[Unit]): Kleisli[M, Env[Ref], B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, Env[Ref], Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: RefIO[A]): Kleisli[M, Env[Ref], A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    def liftE[G[_]](env: Env[Ref] => G ~> RefIO): Kleisli[M, Env[Ref], G ~> RefIO] =
      Kleisli(e => asyncM.pure(env(e)))

    // domain-specific operations are implemented in terms of `primitive`
    override def getBaseTypeName = primitive(_.getBaseTypeName, "getBaseTypeName")
    override def getObject = primitive(_.getObject, "getObject")
    override def getObject(a: Map[String, Class[_]]) = primitive(_.getObject(a), "getObject", a)
    override def setObject(a: AnyRef) = primitive(_.setObject(a), "setObject", a)

  }

  trait SQLDataInterpreter extends SQLDataOp.Visitor[Kleisli[M, Env[SQLData], ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: Env[SQLData] => A): Kleisli[M, Env[SQLData], A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Env[SQLData], A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, Env[SQLData], A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, Env[SQLData], A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => SQLDataIO[Unit]): Kleisli[M, Env[SQLData], A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: SQLDataIO[A], f: Throwable => SQLDataIO[A]): Kleisli[M, Env[SQLData], A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: SQLDataIO[A])(use: A => SQLDataIO[B])(release: (A, ExitCase[Throwable]) => SQLDataIO[Unit]): Kleisli[M, Env[SQLData], B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, Env[SQLData], Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: SQLDataIO[A]): Kleisli[M, Env[SQLData], A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    def liftE[G[_]](env: Env[SQLData] => G ~> SQLDataIO): Kleisli[M, Env[SQLData], G ~> SQLDataIO] =
      Kleisli(e => asyncM.pure(env(e)))

    // domain-specific operations are implemented in terms of `primitive`
    override def getSQLTypeName = primitive(_.getSQLTypeName, "getSQLTypeName")
    override def readSQL(a: SQLInput, b: String) = primitive(_.readSQL(a, b), "readSQL", a, b)
    override def writeSQL(a: SQLOutput) = primitive(_.writeSQL(a), "writeSQL", a)

  }

  trait SQLInputInterpreter extends SQLInputOp.Visitor[Kleisli[M, Env[SQLInput], ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: Env[SQLInput] => A): Kleisli[M, Env[SQLInput], A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Env[SQLInput], A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, Env[SQLInput], A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, Env[SQLInput], A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => SQLInputIO[Unit]): Kleisli[M, Env[SQLInput], A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: SQLInputIO[A], f: Throwable => SQLInputIO[A]): Kleisli[M, Env[SQLInput], A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: SQLInputIO[A])(use: A => SQLInputIO[B])(release: (A, ExitCase[Throwable]) => SQLInputIO[Unit]): Kleisli[M, Env[SQLInput], B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, Env[SQLInput], Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: SQLInputIO[A]): Kleisli[M, Env[SQLInput], A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    def liftE[G[_]](env: Env[SQLInput] => G ~> SQLInputIO): Kleisli[M, Env[SQLInput], G ~> SQLInputIO] =
      Kleisli(e => asyncM.pure(env(e)))

    // domain-specific operations are implemented in terms of `primitive`
    override def readArray = primitive(_.readArray, "readArray")
    override def readAsciiStream = primitive(_.readAsciiStream, "readAsciiStream")
    override def readBigDecimal = primitive(_.readBigDecimal, "readBigDecimal")
    override def readBinaryStream = primitive(_.readBinaryStream, "readBinaryStream")
    override def readBlob = primitive(_.readBlob, "readBlob")
    override def readBoolean = primitive(_.readBoolean, "readBoolean")
    override def readByte = primitive(_.readByte, "readByte")
    override def readBytes = primitive(_.readBytes, "readBytes")
    override def readCharacterStream = primitive(_.readCharacterStream, "readCharacterStream")
    override def readClob = primitive(_.readClob, "readClob")
    override def readDate = primitive(_.readDate, "readDate")
    override def readDouble = primitive(_.readDouble, "readDouble")
    override def readFloat = primitive(_.readFloat, "readFloat")
    override def readInt = primitive(_.readInt, "readInt")
    override def readLong = primitive(_.readLong, "readLong")
    override def readNClob = primitive(_.readNClob, "readNClob")
    override def readNString = primitive(_.readNString, "readNString")
    override def readObject = primitive(_.readObject, "readObject")
    override def readObject[T](a: Class[T]) = primitive(_.readObject(a), "readObject", a)
    override def readRef = primitive(_.readRef, "readRef")
    override def readRowId = primitive(_.readRowId, "readRowId")
    override def readSQLXML = primitive(_.readSQLXML, "readSQLXML")
    override def readShort = primitive(_.readShort, "readShort")
    override def readString = primitive(_.readString, "readString")
    override def readTime = primitive(_.readTime, "readTime")
    override def readTimestamp = primitive(_.readTimestamp, "readTimestamp")
    override def readURL = primitive(_.readURL, "readURL")
    override def wasNull = primitive(_.wasNull, "wasNull")

  }

  trait SQLOutputInterpreter extends SQLOutputOp.Visitor[Kleisli[M, Env[SQLOutput], ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: Env[SQLOutput] => A): Kleisli[M, Env[SQLOutput], A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Env[SQLOutput], A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, Env[SQLOutput], A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, Env[SQLOutput], A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => SQLOutputIO[Unit]): Kleisli[M, Env[SQLOutput], A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: SQLOutputIO[A], f: Throwable => SQLOutputIO[A]): Kleisli[M, Env[SQLOutput], A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: SQLOutputIO[A])(use: A => SQLOutputIO[B])(release: (A, ExitCase[Throwable]) => SQLOutputIO[Unit]): Kleisli[M, Env[SQLOutput], B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, Env[SQLOutput], Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: SQLOutputIO[A]): Kleisli[M, Env[SQLOutput], A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    def liftE[G[_]](env: Env[SQLOutput] => G ~> SQLOutputIO): Kleisli[M, Env[SQLOutput], G ~> SQLOutputIO] =
      Kleisli(e => asyncM.pure(env(e)))

    // domain-specific operations are implemented in terms of `primitive`
    override def writeArray(a: SqlArray) = primitive(_.writeArray(a), "writeArray", a)
    override def writeAsciiStream(a: InputStream) = primitive(_.writeAsciiStream(a), "writeAsciiStream", a)
    override def writeBigDecimal(a: BigDecimal) = primitive(_.writeBigDecimal(a), "writeBigDecimal", a)
    override def writeBinaryStream(a: InputStream) = primitive(_.writeBinaryStream(a), "writeBinaryStream", a)
    override def writeBlob(a: Blob) = primitive(_.writeBlob(a), "writeBlob", a)
    override def writeBoolean(a: Boolean) = primitive(_.writeBoolean(a), "writeBoolean", a)
    override def writeByte(a: Byte) = primitive(_.writeByte(a), "writeByte", a)
    override def writeBytes(a: Array[Byte]) = primitive(_.writeBytes(a), "writeBytes", a)
    override def writeCharacterStream(a: Reader) = primitive(_.writeCharacterStream(a), "writeCharacterStream", a)
    override def writeClob(a: Clob) = primitive(_.writeClob(a), "writeClob", a)
    override def writeDate(a: Date) = primitive(_.writeDate(a), "writeDate", a)
    override def writeDouble(a: Double) = primitive(_.writeDouble(a), "writeDouble", a)
    override def writeFloat(a: Float) = primitive(_.writeFloat(a), "writeFloat", a)
    override def writeInt(a: Int) = primitive(_.writeInt(a), "writeInt", a)
    override def writeLong(a: Long) = primitive(_.writeLong(a), "writeLong", a)
    override def writeNClob(a: NClob) = primitive(_.writeNClob(a), "writeNClob", a)
    override def writeNString(a: String) = primitive(_.writeNString(a), "writeNString", a)
    override def writeObject(a: AnyRef, b: SQLType) = primitive(_.writeObject(a, b), "writeObject", a, b)
    override def writeObject(a: SQLData) = primitive(_.writeObject(a), "writeObject", a)
    override def writeRef(a: Ref) = primitive(_.writeRef(a), "writeRef", a)
    override def writeRowId(a: RowId) = primitive(_.writeRowId(a), "writeRowId", a)
    override def writeSQLXML(a: SQLXML) = primitive(_.writeSQLXML(a), "writeSQLXML", a)
    override def writeShort(a: Short) = primitive(_.writeShort(a), "writeShort", a)
    override def writeString(a: String) = primitive(_.writeString(a), "writeString", a)
    override def writeStruct(a: Struct) = primitive(_.writeStruct(a), "writeStruct", a)
    override def writeTime(a: Time) = primitive(_.writeTime(a), "writeTime", a)
    override def writeTimestamp(a: Timestamp) = primitive(_.writeTimestamp(a), "writeTimestamp", a)
    override def writeURL(a: URL) = primitive(_.writeURL(a), "writeURL", a)

  }

  trait ConnectionInterpreter extends ConnectionOp.Visitor[Kleisli[M, Env[Connection], ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: Env[Connection] => A): Kleisli[M, Env[Connection], A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Env[Connection], A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, Env[Connection], A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, Env[Connection], A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => ConnectionIO[Unit]): Kleisli[M, Env[Connection], A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: ConnectionIO[A], f: Throwable => ConnectionIO[A]): Kleisli[M, Env[Connection], A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: ConnectionIO[A])(use: A => ConnectionIO[B])(release: (A, ExitCase[Throwable]) => ConnectionIO[Unit]): Kleisli[M, Env[Connection], B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, Env[Connection], Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: ConnectionIO[A]): Kleisli[M, Env[Connection], A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    def liftE[G[_]](env: Env[Connection] => G ~> ConnectionIO): Kleisli[M, Env[Connection], G ~> ConnectionIO] =
      Kleisli(e => asyncM.pure(env(e)))

    // domain-specific operations are implemented in terms of `primitive`
    override def abort(a: Executor) = primitive(_.abort(a), "abort", a)
    override def clearWarnings = primitive(_.clearWarnings, "clearWarnings")
    override def close = primitive(_.close, "close")
    override def commit = primitive(_.commit, "commit")
    override def createArrayOf(a: String, b: Array[AnyRef]) = primitive(_.createArrayOf(a, b), "createArrayOf", a, b)
    override def createBlob = primitive(_.createBlob, "createBlob")
    override def createClob = primitive(_.createClob, "createClob")
    override def createNClob = primitive(_.createNClob, "createNClob")
    override def createSQLXML = primitive(_.createSQLXML, "createSQLXML")
    override def createStatement = primitive(_.createStatement, "createStatement")
    override def createStatement(a: Int, b: Int) = primitive(_.createStatement(a, b), "createStatement", a, b)
    override def createStatement(a: Int, b: Int, c: Int) = primitive(_.createStatement(a, b, c), "createStatement", a, b, c)
    override def createStruct(a: String, b: Array[AnyRef]) = primitive(_.createStruct(a, b), "createStruct", a, b)
    override def getAutoCommit = primitive(_.getAutoCommit, "getAutoCommit")
    override def getCatalog = primitive(_.getCatalog, "getCatalog")
    override def getClientInfo = primitive(_.getClientInfo, "getClientInfo")
    override def getClientInfo(a: String) = primitive(_.getClientInfo(a), "getClientInfo", a)
    override def getHoldability = primitive(_.getHoldability, "getHoldability")
    override def getMetaData = primitive(_.getMetaData, "getMetaData")
    override def getNetworkTimeout = primitive(_.getNetworkTimeout, "getNetworkTimeout")
    override def getSchema = primitive(_.getSchema, "getSchema")
    override def getTransactionIsolation = primitive(_.getTransactionIsolation, "getTransactionIsolation")
    override def getTypeMap = primitive(_.getTypeMap, "getTypeMap")
    override def getWarnings = primitive(_.getWarnings, "getWarnings")
    override def isClosed = primitive(_.isClosed, "isClosed")
    override def isReadOnly = primitive(_.isReadOnly, "isReadOnly")
    override def isValid(a: Int) = primitive(_.isValid(a), "isValid", a)
    override def isWrapperFor(a: Class[_]) = primitive(_.isWrapperFor(a), "isWrapperFor", a)
    override def nativeSQL(a: String) = primitive(_.nativeSQL(a), "nativeSQL", a)
    override def prepareCall(a: String) = primitive(_.prepareCall(a), "prepareCall", a)
    override def prepareCall(a: String, b: Int, c: Int) = primitive(_.prepareCall(a, b, c), "prepareCall", a, b, c)
    override def prepareCall(a: String, b: Int, c: Int, d: Int) = primitive(_.prepareCall(a, b, c, d), "prepareCall", a, b, c, d)
    override def prepareStatement(a: String) = primitive(_.prepareStatement(a), "prepareStatement", a)
    override def prepareStatement(a: String, b: Array[Int]) = primitive(_.prepareStatement(a, b), "prepareStatement", a, b)
    override def prepareStatement(a: String, b: Array[String]) = primitive(_.prepareStatement(a, b), "prepareStatement", a, b)
    override def prepareStatement(a: String, b: Int) = primitive(_.prepareStatement(a, b), "prepareStatement", a, b)
    override def prepareStatement(a: String, b: Int, c: Int) = primitive(_.prepareStatement(a, b, c), "prepareStatement", a, b, c)
    override def prepareStatement(a: String, b: Int, c: Int, d: Int) = primitive(_.prepareStatement(a, b, c, d), "prepareStatement", a, b, c, d)
    override def releaseSavepoint(a: Savepoint) = primitive(_.releaseSavepoint(a), "releaseSavepoint", a)
    override def rollback = primitive(_.rollback, "rollback")
    override def rollback(a: Savepoint) = primitive(_.rollback(a), "rollback", a)
    override def setAutoCommit(a: Boolean) = primitive(_.setAutoCommit(a), "setAutoCommit", a)
    override def setCatalog(a: String) = primitive(_.setCatalog(a), "setCatalog", a)
    override def setClientInfo(a: Properties) = primitive(_.setClientInfo(a), "setClientInfo", a)
    override def setClientInfo(a: String, b: String) = primitive(_.setClientInfo(a, b), "setClientInfo", a, b)
    override def setHoldability(a: Int) = primitive(_.setHoldability(a), "setHoldability", a)
    override def setNetworkTimeout(a: Executor, b: Int) = primitive(_.setNetworkTimeout(a, b), "setNetworkTimeout", a, b)
    override def setReadOnly(a: Boolean) = primitive(_.setReadOnly(a), "setReadOnly", a)
    override def setSavepoint = primitive(_.setSavepoint, "setSavepoint")
    override def setSavepoint(a: String) = primitive(_.setSavepoint(a), "setSavepoint", a)
    override def setSchema(a: String) = primitive(_.setSchema(a), "setSchema", a)
    override def setTransactionIsolation(a: Int) = primitive(_.setTransactionIsolation(a), "setTransactionIsolation", a)
    override def setTypeMap(a: Map[String, Class[_]]) = primitive(_.setTypeMap(a), "setTypeMap", a)
    override def unwrap[T](a: Class[T]) = primitive(_.unwrap(a), "unwrap", a)

  }

  trait StatementInterpreter extends StatementOp.Visitor[Kleisli[M, Env[Statement], ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: Env[Statement] => A): Kleisli[M, Env[Statement], A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Env[Statement], A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, Env[Statement], A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, Env[Statement], A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => StatementIO[Unit]): Kleisli[M, Env[Statement], A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: StatementIO[A], f: Throwable => StatementIO[A]): Kleisli[M, Env[Statement], A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: StatementIO[A])(use: A => StatementIO[B])(release: (A, ExitCase[Throwable]) => StatementIO[Unit]): Kleisli[M, Env[Statement], B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, Env[Statement], Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: StatementIO[A]): Kleisli[M, Env[Statement], A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    def liftE[G[_]](env: Env[Statement] => G ~> StatementIO): Kleisli[M, Env[Statement], G ~> StatementIO] =
      Kleisli(e => asyncM.pure(env(e)))

    // domain-specific operations are implemented in terms of `primitive`
    override def addBatch(a: String) = primitive(_.addBatch(a), "addBatch", a)
    override def cancel = primitive(_.cancel, "cancel")
    override def clearBatch = primitive(_.clearBatch, "clearBatch")
    override def clearWarnings = primitive(_.clearWarnings, "clearWarnings")
    override def close = primitive(_.close, "close")
    override def closeOnCompletion = primitive(_.closeOnCompletion, "closeOnCompletion")
    override def execute(a: String) = primitive(_.execute(a), "execute", a)
    override def execute(a: String, b: Array[Int]) = primitive(_.execute(a, b), "execute", a, b)
    override def execute(a: String, b: Array[String]) = primitive(_.execute(a, b), "execute", a, b)
    override def execute(a: String, b: Int) = primitive(_.execute(a, b), "execute", a, b)
    override def executeBatch = primitive(_.executeBatch, "executeBatch")
    override def executeLargeBatch = primitive(_.executeLargeBatch, "executeLargeBatch")
    override def executeLargeUpdate(a: String) = primitive(_.executeLargeUpdate(a), "executeLargeUpdate", a)
    override def executeLargeUpdate(a: String, b: Array[Int]) = primitive(_.executeLargeUpdate(a, b), "executeLargeUpdate", a, b)
    override def executeLargeUpdate(a: String, b: Array[String]) = primitive(_.executeLargeUpdate(a, b), "executeLargeUpdate", a, b)
    override def executeLargeUpdate(a: String, b: Int) = primitive(_.executeLargeUpdate(a, b), "executeLargeUpdate", a, b)
    override def executeQuery(a: String) = primitive(_.executeQuery(a), "executeQuery", a)
    override def executeUpdate(a: String) = primitive(_.executeUpdate(a), "executeUpdate", a)
    override def executeUpdate(a: String, b: Array[Int]) = primitive(_.executeUpdate(a, b), "executeUpdate", a, b)
    override def executeUpdate(a: String, b: Array[String]) = primitive(_.executeUpdate(a, b), "executeUpdate", a, b)
    override def executeUpdate(a: String, b: Int) = primitive(_.executeUpdate(a, b), "executeUpdate", a, b)
    override def getConnection = primitive(_.getConnection, "getConnection")
    override def getFetchDirection = primitive(_.getFetchDirection, "getFetchDirection")
    override def getFetchSize = primitive(_.getFetchSize, "getFetchSize")
    override def getGeneratedKeys = primitive(_.getGeneratedKeys, "getGeneratedKeys")
    override def getLargeMaxRows = primitive(_.getLargeMaxRows, "getLargeMaxRows")
    override def getLargeUpdateCount = primitive(_.getLargeUpdateCount, "getLargeUpdateCount")
    override def getMaxFieldSize = primitive(_.getMaxFieldSize, "getMaxFieldSize")
    override def getMaxRows = primitive(_.getMaxRows, "getMaxRows")
    override def getMoreResults = primitive(_.getMoreResults, "getMoreResults")
    override def getMoreResults(a: Int) = primitive(_.getMoreResults(a), "getMoreResults", a)
    override def getQueryTimeout = primitive(_.getQueryTimeout, "getQueryTimeout")
    override def getResultSet = primitive(_.getResultSet, "getResultSet")
    override def getResultSetConcurrency = primitive(_.getResultSetConcurrency, "getResultSetConcurrency")
    override def getResultSetHoldability = primitive(_.getResultSetHoldability, "getResultSetHoldability")
    override def getResultSetType = primitive(_.getResultSetType, "getResultSetType")
    override def getUpdateCount = primitive(_.getUpdateCount, "getUpdateCount")
    override def getWarnings = primitive(_.getWarnings, "getWarnings")
    override def isCloseOnCompletion = primitive(_.isCloseOnCompletion, "isCloseOnCompletion")
    override def isClosed = primitive(_.isClosed, "isClosed")
    override def isPoolable = primitive(_.isPoolable, "isPoolable")
    override def isWrapperFor(a: Class[_]) = primitive(_.isWrapperFor(a), "isWrapperFor", a)
    override def setCursorName(a: String) = primitive(_.setCursorName(a), "setCursorName", a)
    override def setEscapeProcessing(a: Boolean) = primitive(_.setEscapeProcessing(a), "setEscapeProcessing", a)
    override def setFetchDirection(a: Int) = primitive(_.setFetchDirection(a), "setFetchDirection", a)
    override def setFetchSize(a: Int) = primitive(_.setFetchSize(a), "setFetchSize", a)
    override def setLargeMaxRows(a: Long) = primitive(_.setLargeMaxRows(a), "setLargeMaxRows", a)
    override def setMaxFieldSize(a: Int) = primitive(_.setMaxFieldSize(a), "setMaxFieldSize", a)
    override def setMaxRows(a: Int) = primitive(_.setMaxRows(a), "setMaxRows", a)
    override def setPoolable(a: Boolean) = primitive(_.setPoolable(a), "setPoolable", a)
    override def setQueryTimeout(a: Int) = primitive(_.setQueryTimeout(a), "setQueryTimeout", a)
    override def unwrap[T](a: Class[T]) = primitive(_.unwrap(a), "unwrap", a)

  }

  trait PreparedStatementInterpreter extends PreparedStatementOp.Visitor[Kleisli[M, Env[PreparedStatement], ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: Env[PreparedStatement] => A): Kleisli[M, Env[PreparedStatement], A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Env[PreparedStatement], A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, Env[PreparedStatement], A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, Env[PreparedStatement], A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => PreparedStatementIO[Unit]): Kleisli[M, Env[PreparedStatement], A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: PreparedStatementIO[A], f: Throwable => PreparedStatementIO[A]): Kleisli[M, Env[PreparedStatement], A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: PreparedStatementIO[A])(use: A => PreparedStatementIO[B])(release: (A, ExitCase[Throwable]) => PreparedStatementIO[Unit]): Kleisli[M, Env[PreparedStatement], B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, Env[PreparedStatement], Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: PreparedStatementIO[A]): Kleisli[M, Env[PreparedStatement], A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    def liftE[G[_]](env: Env[PreparedStatement] => G ~> PreparedStatementIO): Kleisli[M, Env[PreparedStatement], G ~> PreparedStatementIO] =
      Kleisli(e => asyncM.pure(env(e)))

    // domain-specific operations are implemented in terms of `primitive`
    override def addBatch = primitive(_.addBatch, "addBatch")
    override def addBatch(a: String) = primitive(_.addBatch(a), "addBatch", a)
    override def cancel = primitive(_.cancel, "cancel")
    override def clearBatch = primitive(_.clearBatch, "clearBatch")
    override def clearParameters = primitive(_.clearParameters, "clearParameters")
    override def clearWarnings = primitive(_.clearWarnings, "clearWarnings")
    override def close = primitive(_.close, "close")
    override def closeOnCompletion = primitive(_.closeOnCompletion, "closeOnCompletion")
    override def execute = primitive(_.execute, "execute")
    override def execute(a: String) = primitive(_.execute(a), "execute", a)
    override def execute(a: String, b: Array[Int]) = primitive(_.execute(a, b), "execute", a, b)
    override def execute(a: String, b: Array[String]) = primitive(_.execute(a, b), "execute", a, b)
    override def execute(a: String, b: Int) = primitive(_.execute(a, b), "execute", a, b)
    override def executeBatch = primitive(_.executeBatch, "executeBatch")
    override def executeLargeBatch = primitive(_.executeLargeBatch, "executeLargeBatch")
    override def executeLargeUpdate = primitive(_.executeLargeUpdate, "executeLargeUpdate")
    override def executeLargeUpdate(a: String) = primitive(_.executeLargeUpdate(a), "executeLargeUpdate", a)
    override def executeLargeUpdate(a: String, b: Array[Int]) = primitive(_.executeLargeUpdate(a, b), "executeLargeUpdate", a, b)
    override def executeLargeUpdate(a: String, b: Array[String]) = primitive(_.executeLargeUpdate(a, b), "executeLargeUpdate", a, b)
    override def executeLargeUpdate(a: String, b: Int) = primitive(_.executeLargeUpdate(a, b), "executeLargeUpdate", a, b)
    override def executeQuery = primitive(_.executeQuery, "executeQuery")
    override def executeQuery(a: String) = primitive(_.executeQuery(a), "executeQuery", a)
    override def executeUpdate = primitive(_.executeUpdate, "executeUpdate")
    override def executeUpdate(a: String) = primitive(_.executeUpdate(a), "executeUpdate", a)
    override def executeUpdate(a: String, b: Array[Int]) = primitive(_.executeUpdate(a, b), "executeUpdate", a, b)
    override def executeUpdate(a: String, b: Array[String]) = primitive(_.executeUpdate(a, b), "executeUpdate", a, b)
    override def executeUpdate(a: String, b: Int) = primitive(_.executeUpdate(a, b), "executeUpdate", a, b)
    override def getConnection = primitive(_.getConnection, "getConnection")
    override def getFetchDirection = primitive(_.getFetchDirection, "getFetchDirection")
    override def getFetchSize = primitive(_.getFetchSize, "getFetchSize")
    override def getGeneratedKeys = primitive(_.getGeneratedKeys, "getGeneratedKeys")
    override def getLargeMaxRows = primitive(_.getLargeMaxRows, "getLargeMaxRows")
    override def getLargeUpdateCount = primitive(_.getLargeUpdateCount, "getLargeUpdateCount")
    override def getMaxFieldSize = primitive(_.getMaxFieldSize, "getMaxFieldSize")
    override def getMaxRows = primitive(_.getMaxRows, "getMaxRows")
    override def getMetaData = primitive(_.getMetaData, "getMetaData")
    override def getMoreResults = primitive(_.getMoreResults, "getMoreResults")
    override def getMoreResults(a: Int) = primitive(_.getMoreResults(a), "getMoreResults", a)
    override def getParameterMetaData = primitive(_.getParameterMetaData, "getParameterMetaData")
    override def getQueryTimeout = primitive(_.getQueryTimeout, "getQueryTimeout")
    override def getResultSet = primitive(_.getResultSet, "getResultSet")
    override def getResultSetConcurrency = primitive(_.getResultSetConcurrency, "getResultSetConcurrency")
    override def getResultSetHoldability = primitive(_.getResultSetHoldability, "getResultSetHoldability")
    override def getResultSetType = primitive(_.getResultSetType, "getResultSetType")
    override def getUpdateCount = primitive(_.getUpdateCount, "getUpdateCount")
    override def getWarnings = primitive(_.getWarnings, "getWarnings")
    override def isCloseOnCompletion = primitive(_.isCloseOnCompletion, "isCloseOnCompletion")
    override def isClosed = primitive(_.isClosed, "isClosed")
    override def isPoolable = primitive(_.isPoolable, "isPoolable")
    override def isWrapperFor(a: Class[_]) = primitive(_.isWrapperFor(a), "isWrapperFor", a)
    override def setArray(a: Int, b: SqlArray) = primitive(_.setArray(a, b), "setArray", a, b)
    override def setAsciiStream(a: Int, b: InputStream) = primitive(_.setAsciiStream(a, b), "setAsciiStream", a, b)
    override def setAsciiStream(a: Int, b: InputStream, c: Int) = primitive(_.setAsciiStream(a, b, c), "setAsciiStream", a, b, c)
    override def setAsciiStream(a: Int, b: InputStream, c: Long) = primitive(_.setAsciiStream(a, b, c), "setAsciiStream", a, b, c)
    override def setBigDecimal(a: Int, b: BigDecimal) = primitive(_.setBigDecimal(a, b), "setBigDecimal", a, b)
    override def setBinaryStream(a: Int, b: InputStream) = primitive(_.setBinaryStream(a, b), "setBinaryStream", a, b)
    override def setBinaryStream(a: Int, b: InputStream, c: Int) = primitive(_.setBinaryStream(a, b, c), "setBinaryStream", a, b, c)
    override def setBinaryStream(a: Int, b: InputStream, c: Long) = primitive(_.setBinaryStream(a, b, c), "setBinaryStream", a, b, c)
    override def setBlob(a: Int, b: Blob) = primitive(_.setBlob(a, b), "setBlob", a, b)
    override def setBlob(a: Int, b: InputStream) = primitive(_.setBlob(a, b), "setBlob", a, b)
    override def setBlob(a: Int, b: InputStream, c: Long) = primitive(_.setBlob(a, b, c), "setBlob", a, b, c)
    override def setBoolean(a: Int, b: Boolean) = primitive(_.setBoolean(a, b), "setBoolean", a, b)
    override def setByte(a: Int, b: Byte) = primitive(_.setByte(a, b), "setByte", a, b)
    override def setBytes(a: Int, b: Array[Byte]) = primitive(_.setBytes(a, b), "setBytes", a, b)
    override def setCharacterStream(a: Int, b: Reader) = primitive(_.setCharacterStream(a, b), "setCharacterStream", a, b)
    override def setCharacterStream(a: Int, b: Reader, c: Int) = primitive(_.setCharacterStream(a, b, c), "setCharacterStream", a, b, c)
    override def setCharacterStream(a: Int, b: Reader, c: Long) = primitive(_.setCharacterStream(a, b, c), "setCharacterStream", a, b, c)
    override def setClob(a: Int, b: Clob) = primitive(_.setClob(a, b), "setClob", a, b)
    override def setClob(a: Int, b: Reader) = primitive(_.setClob(a, b), "setClob", a, b)
    override def setClob(a: Int, b: Reader, c: Long) = primitive(_.setClob(a, b, c), "setClob", a, b, c)
    override def setCursorName(a: String) = primitive(_.setCursorName(a), "setCursorName", a)
    override def setDate(a: Int, b: Date) = primitive(_.setDate(a, b), "setDate", a, b)
    override def setDate(a: Int, b: Date, c: Calendar) = primitive(_.setDate(a, b, c), "setDate", a, b, c)
    override def setDouble(a: Int, b: Double) = primitive(_.setDouble(a, b), "setDouble", a, b)
    override def setEscapeProcessing(a: Boolean) = primitive(_.setEscapeProcessing(a), "setEscapeProcessing", a)
    override def setFetchDirection(a: Int) = primitive(_.setFetchDirection(a), "setFetchDirection", a)
    override def setFetchSize(a: Int) = primitive(_.setFetchSize(a), "setFetchSize", a)
    override def setFloat(a: Int, b: Float) = primitive(_.setFloat(a, b), "setFloat", a, b)
    override def setInt(a: Int, b: Int) = primitive(_.setInt(a, b), "setInt", a, b)
    override def setLargeMaxRows(a: Long) = primitive(_.setLargeMaxRows(a), "setLargeMaxRows", a)
    override def setLong(a: Int, b: Long) = primitive(_.setLong(a, b), "setLong", a, b)
    override def setMaxFieldSize(a: Int) = primitive(_.setMaxFieldSize(a), "setMaxFieldSize", a)
    override def setMaxRows(a: Int) = primitive(_.setMaxRows(a), "setMaxRows", a)
    override def setNCharacterStream(a: Int, b: Reader) = primitive(_.setNCharacterStream(a, b), "setNCharacterStream", a, b)
    override def setNCharacterStream(a: Int, b: Reader, c: Long) = primitive(_.setNCharacterStream(a, b, c), "setNCharacterStream", a, b, c)
    override def setNClob(a: Int, b: NClob) = primitive(_.setNClob(a, b), "setNClob", a, b)
    override def setNClob(a: Int, b: Reader) = primitive(_.setNClob(a, b), "setNClob", a, b)
    override def setNClob(a: Int, b: Reader, c: Long) = primitive(_.setNClob(a, b, c), "setNClob", a, b, c)
    override def setNString(a: Int, b: String) = primitive(_.setNString(a, b), "setNString", a, b)
    override def setNull(a: Int, b: Int) = primitive(_.setNull(a, b), "setNull", a, b)
    override def setNull(a: Int, b: Int, c: String) = primitive(_.setNull(a, b, c), "setNull", a, b, c)
    override def setObject(a: Int, b: AnyRef) = primitive(_.setObject(a, b), "setObject", a, b)
    override def setObject(a: Int, b: AnyRef, c: Int) = primitive(_.setObject(a, b, c), "setObject", a, b, c)
    override def setObject(a: Int, b: AnyRef, c: Int, d: Int) = primitive(_.setObject(a, b, c, d), "setObject", a, b, c, d)
    override def setObject(a: Int, b: AnyRef, c: SQLType) = primitive(_.setObject(a, b, c), "setObject", a, b, c)
    override def setObject(a: Int, b: AnyRef, c: SQLType, d: Int) = primitive(_.setObject(a, b, c, d), "setObject", a, b, c, d)
    override def setPoolable(a: Boolean) = primitive(_.setPoolable(a), "setPoolable", a)
    override def setQueryTimeout(a: Int) = primitive(_.setQueryTimeout(a), "setQueryTimeout", a)
    override def setRef(a: Int, b: Ref) = primitive(_.setRef(a, b), "setRef", a, b)
    override def setRowId(a: Int, b: RowId) = primitive(_.setRowId(a, b), "setRowId", a, b)
    override def setSQLXML(a: Int, b: SQLXML) = primitive(_.setSQLXML(a, b), "setSQLXML", a, b)
    override def setShort(a: Int, b: Short) = primitive(_.setShort(a, b), "setShort", a, b)
    override def setString(a: Int, b: String) = primitive(_.setString(a, b), "setString", a, b)
    override def setTime(a: Int, b: Time) = primitive(_.setTime(a, b), "setTime", a, b)
    override def setTime(a: Int, b: Time, c: Calendar) = primitive(_.setTime(a, b, c), "setTime", a, b, c)
    override def setTimestamp(a: Int, b: Timestamp) = primitive(_.setTimestamp(a, b), "setTimestamp", a, b)
    override def setTimestamp(a: Int, b: Timestamp, c: Calendar) = primitive(_.setTimestamp(a, b, c), "setTimestamp", a, b, c)
    override def setURL(a: Int, b: URL) = primitive(_.setURL(a, b), "setURL", a, b)
    override def setUnicodeStream(a: Int, b: InputStream, c: Int) = primitive(_.setUnicodeStream(a, b, c), "setUnicodeStream", a, b, c)
    override def unwrap[T](a: Class[T]) = primitive(_.unwrap(a), "unwrap", a)

  }

  trait CallableStatementInterpreter extends CallableStatementOp.Visitor[Kleisli[M, Env[CallableStatement], ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: Env[CallableStatement] => A): Kleisli[M, Env[CallableStatement], A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Env[CallableStatement], A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, Env[CallableStatement], A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, Env[CallableStatement], A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => CallableStatementIO[Unit]): Kleisli[M, Env[CallableStatement], A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: CallableStatementIO[A], f: Throwable => CallableStatementIO[A]): Kleisli[M, Env[CallableStatement], A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: CallableStatementIO[A])(use: A => CallableStatementIO[B])(release: (A, ExitCase[Throwable]) => CallableStatementIO[Unit]): Kleisli[M, Env[CallableStatement], B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, Env[CallableStatement], Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: CallableStatementIO[A]): Kleisli[M, Env[CallableStatement], A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    def liftE[G[_]](env: Env[CallableStatement] => G ~> CallableStatementIO): Kleisli[M, Env[CallableStatement], G ~> CallableStatementIO] =
      Kleisli(e => asyncM.pure(env(e)))

    // domain-specific operations are implemented in terms of `primitive`
    override def addBatch = primitive(_.addBatch, "addBatch")
    override def addBatch(a: String) = primitive(_.addBatch(a), "addBatch", a)
    override def cancel = primitive(_.cancel, "cancel")
    override def clearBatch = primitive(_.clearBatch, "clearBatch")
    override def clearParameters = primitive(_.clearParameters, "clearParameters")
    override def clearWarnings = primitive(_.clearWarnings, "clearWarnings")
    override def close = primitive(_.close, "close")
    override def closeOnCompletion = primitive(_.closeOnCompletion, "closeOnCompletion")
    override def execute = primitive(_.execute, "execute")
    override def execute(a: String) = primitive(_.execute(a), "execute", a)
    override def execute(a: String, b: Array[Int]) = primitive(_.execute(a, b), "execute", a, b)
    override def execute(a: String, b: Array[String]) = primitive(_.execute(a, b), "execute", a, b)
    override def execute(a: String, b: Int) = primitive(_.execute(a, b), "execute", a, b)
    override def executeBatch = primitive(_.executeBatch, "executeBatch")
    override def executeLargeBatch = primitive(_.executeLargeBatch, "executeLargeBatch")
    override def executeLargeUpdate = primitive(_.executeLargeUpdate, "executeLargeUpdate")
    override def executeLargeUpdate(a: String) = primitive(_.executeLargeUpdate(a), "executeLargeUpdate", a)
    override def executeLargeUpdate(a: String, b: Array[Int]) = primitive(_.executeLargeUpdate(a, b), "executeLargeUpdate", a, b)
    override def executeLargeUpdate(a: String, b: Array[String]) = primitive(_.executeLargeUpdate(a, b), "executeLargeUpdate", a, b)
    override def executeLargeUpdate(a: String, b: Int) = primitive(_.executeLargeUpdate(a, b), "executeLargeUpdate", a, b)
    override def executeQuery = primitive(_.executeQuery, "executeQuery")
    override def executeQuery(a: String) = primitive(_.executeQuery(a), "executeQuery", a)
    override def executeUpdate = primitive(_.executeUpdate, "executeUpdate")
    override def executeUpdate(a: String) = primitive(_.executeUpdate(a), "executeUpdate", a)
    override def executeUpdate(a: String, b: Array[Int]) = primitive(_.executeUpdate(a, b), "executeUpdate", a, b)
    override def executeUpdate(a: String, b: Array[String]) = primitive(_.executeUpdate(a, b), "executeUpdate", a, b)
    override def executeUpdate(a: String, b: Int) = primitive(_.executeUpdate(a, b), "executeUpdate", a, b)
    override def getArray(a: Int) = primitive(_.getArray(a), "getArray", a)
    override def getArray(a: String) = primitive(_.getArray(a), "getArray", a)
    override def getBigDecimal(a: Int) = primitive(_.getBigDecimal(a), "getBigDecimal", a)
    override def getBigDecimal(a: Int, b: Int) = primitive(_.getBigDecimal(a, b), "getBigDecimal", a, b)
    override def getBigDecimal(a: String) = primitive(_.getBigDecimal(a), "getBigDecimal", a)
    override def getBlob(a: Int) = primitive(_.getBlob(a), "getBlob", a)
    override def getBlob(a: String) = primitive(_.getBlob(a), "getBlob", a)
    override def getBoolean(a: Int) = primitive(_.getBoolean(a), "getBoolean", a)
    override def getBoolean(a: String) = primitive(_.getBoolean(a), "getBoolean", a)
    override def getByte(a: Int) = primitive(_.getByte(a), "getByte", a)
    override def getByte(a: String) = primitive(_.getByte(a), "getByte", a)
    override def getBytes(a: Int) = primitive(_.getBytes(a), "getBytes", a)
    override def getBytes(a: String) = primitive(_.getBytes(a), "getBytes", a)
    override def getCharacterStream(a: Int) = primitive(_.getCharacterStream(a), "getCharacterStream", a)
    override def getCharacterStream(a: String) = primitive(_.getCharacterStream(a), "getCharacterStream", a)
    override def getClob(a: Int) = primitive(_.getClob(a), "getClob", a)
    override def getClob(a: String) = primitive(_.getClob(a), "getClob", a)
    override def getConnection = primitive(_.getConnection, "getConnection")
    override def getDate(a: Int) = primitive(_.getDate(a), "getDate", a)
    override def getDate(a: Int, b: Calendar) = primitive(_.getDate(a, b), "getDate", a, b)
    override def getDate(a: String) = primitive(_.getDate(a), "getDate", a)
    override def getDate(a: String, b: Calendar) = primitive(_.getDate(a, b), "getDate", a, b)
    override def getDouble(a: Int) = primitive(_.getDouble(a), "getDouble", a)
    override def getDouble(a: String) = primitive(_.getDouble(a), "getDouble", a)
    override def getFetchDirection = primitive(_.getFetchDirection, "getFetchDirection")
    override def getFetchSize = primitive(_.getFetchSize, "getFetchSize")
    override def getFloat(a: Int) = primitive(_.getFloat(a), "getFloat", a)
    override def getFloat(a: String) = primitive(_.getFloat(a), "getFloat", a)
    override def getGeneratedKeys = primitive(_.getGeneratedKeys, "getGeneratedKeys")
    override def getInt(a: Int) = primitive(_.getInt(a), "getInt", a)
    override def getInt(a: String) = primitive(_.getInt(a), "getInt", a)
    override def getLargeMaxRows = primitive(_.getLargeMaxRows, "getLargeMaxRows")
    override def getLargeUpdateCount = primitive(_.getLargeUpdateCount, "getLargeUpdateCount")
    override def getLong(a: Int) = primitive(_.getLong(a), "getLong", a)
    override def getLong(a: String) = primitive(_.getLong(a), "getLong", a)
    override def getMaxFieldSize = primitive(_.getMaxFieldSize, "getMaxFieldSize")
    override def getMaxRows = primitive(_.getMaxRows, "getMaxRows")
    override def getMetaData = primitive(_.getMetaData, "getMetaData")
    override def getMoreResults = primitive(_.getMoreResults, "getMoreResults")
    override def getMoreResults(a: Int) = primitive(_.getMoreResults(a), "getMoreResults", a)
    override def getNCharacterStream(a: Int) = primitive(_.getNCharacterStream(a), "getNCharacterStream", a)
    override def getNCharacterStream(a: String) = primitive(_.getNCharacterStream(a), "getNCharacterStream", a)
    override def getNClob(a: Int) = primitive(_.getNClob(a), "getNClob", a)
    override def getNClob(a: String) = primitive(_.getNClob(a), "getNClob", a)
    override def getNString(a: Int) = primitive(_.getNString(a), "getNString", a)
    override def getNString(a: String) = primitive(_.getNString(a), "getNString", a)
    override def getObject(a: Int) = primitive(_.getObject(a), "getObject", a)
    override def getObject[T](a: Int, b: Class[T]) = primitive(_.getObject(a, b), "getObject", a, b)
    override def getObject(a: Int, b: Map[String, Class[_]]) = primitive(_.getObject(a, b), "getObject", a, b)
    override def getObject(a: String) = primitive(_.getObject(a), "getObject", a)
    override def getObject[T](a: String, b: Class[T]) = primitive(_.getObject(a, b), "getObject", a, b)
    override def getObject(a: String, b: Map[String, Class[_]]) = primitive(_.getObject(a, b), "getObject", a, b)
    override def getParameterMetaData = primitive(_.getParameterMetaData, "getParameterMetaData")
    override def getQueryTimeout = primitive(_.getQueryTimeout, "getQueryTimeout")
    override def getRef(a: Int) = primitive(_.getRef(a), "getRef", a)
    override def getRef(a: String) = primitive(_.getRef(a), "getRef", a)
    override def getResultSet = primitive(_.getResultSet, "getResultSet")
    override def getResultSetConcurrency = primitive(_.getResultSetConcurrency, "getResultSetConcurrency")
    override def getResultSetHoldability = primitive(_.getResultSetHoldability, "getResultSetHoldability")
    override def getResultSetType = primitive(_.getResultSetType, "getResultSetType")
    override def getRowId(a: Int) = primitive(_.getRowId(a), "getRowId", a)
    override def getRowId(a: String) = primitive(_.getRowId(a), "getRowId", a)
    override def getSQLXML(a: Int) = primitive(_.getSQLXML(a), "getSQLXML", a)
    override def getSQLXML(a: String) = primitive(_.getSQLXML(a), "getSQLXML", a)
    override def getShort(a: Int) = primitive(_.getShort(a), "getShort", a)
    override def getShort(a: String) = primitive(_.getShort(a), "getShort", a)
    override def getString(a: Int) = primitive(_.getString(a), "getString", a)
    override def getString(a: String) = primitive(_.getString(a), "getString", a)
    override def getTime(a: Int) = primitive(_.getTime(a), "getTime", a)
    override def getTime(a: Int, b: Calendar) = primitive(_.getTime(a, b), "getTime", a, b)
    override def getTime(a: String) = primitive(_.getTime(a), "getTime", a)
    override def getTime(a: String, b: Calendar) = primitive(_.getTime(a, b), "getTime", a, b)
    override def getTimestamp(a: Int) = primitive(_.getTimestamp(a), "getTimestamp", a)
    override def getTimestamp(a: Int, b: Calendar) = primitive(_.getTimestamp(a, b), "getTimestamp", a, b)
    override def getTimestamp(a: String) = primitive(_.getTimestamp(a), "getTimestamp", a)
    override def getTimestamp(a: String, b: Calendar) = primitive(_.getTimestamp(a, b), "getTimestamp", a, b)
    override def getURL(a: Int) = primitive(_.getURL(a), "getURL", a)
    override def getURL(a: String) = primitive(_.getURL(a), "getURL", a)
    override def getUpdateCount = primitive(_.getUpdateCount, "getUpdateCount")
    override def getWarnings = primitive(_.getWarnings, "getWarnings")
    override def isCloseOnCompletion = primitive(_.isCloseOnCompletion, "isCloseOnCompletion")
    override def isClosed = primitive(_.isClosed, "isClosed")
    override def isPoolable = primitive(_.isPoolable, "isPoolable")
    override def isWrapperFor(a: Class[_]) = primitive(_.isWrapperFor(a), "isWrapperFor", a)
    override def registerOutParameter(a: Int, b: Int) = primitive(_.registerOutParameter(a, b), "registerOutParameter", a, b)
    override def registerOutParameter(a: Int, b: Int, c: Int) = primitive(_.registerOutParameter(a, b, c), "registerOutParameter", a, b, c)
    override def registerOutParameter(a: Int, b: Int, c: String) = primitive(_.registerOutParameter(a, b, c), "registerOutParameter", a, b, c)
    override def registerOutParameter(a: Int, b: SQLType) = primitive(_.registerOutParameter(a, b), "registerOutParameter", a, b)
    override def registerOutParameter(a: Int, b: SQLType, c: Int) = primitive(_.registerOutParameter(a, b, c), "registerOutParameter", a, b, c)
    override def registerOutParameter(a: Int, b: SQLType, c: String) = primitive(_.registerOutParameter(a, b, c), "registerOutParameter", a, b, c)
    override def registerOutParameter(a: String, b: Int) = primitive(_.registerOutParameter(a, b), "registerOutParameter", a, b)
    override def registerOutParameter(a: String, b: Int, c: Int) = primitive(_.registerOutParameter(a, b, c), "registerOutParameter", a, b, c)
    override def registerOutParameter(a: String, b: Int, c: String) = primitive(_.registerOutParameter(a, b, c), "registerOutParameter", a, b, c)
    override def registerOutParameter(a: String, b: SQLType) = primitive(_.registerOutParameter(a, b), "registerOutParameter", a, b)
    override def registerOutParameter(a: String, b: SQLType, c: Int) = primitive(_.registerOutParameter(a, b, c), "registerOutParameter", a, b, c)
    override def registerOutParameter(a: String, b: SQLType, c: String) = primitive(_.registerOutParameter(a, b, c), "registerOutParameter", a, b, c)
    override def setArray(a: Int, b: SqlArray) = primitive(_.setArray(a, b), "setArray", a, b)
    override def setAsciiStream(a: Int, b: InputStream) = primitive(_.setAsciiStream(a, b), "setAsciiStream", a, b)
    override def setAsciiStream(a: Int, b: InputStream, c: Int) = primitive(_.setAsciiStream(a, b, c), "setAsciiStream", a, b, c)
    override def setAsciiStream(a: Int, b: InputStream, c: Long) = primitive(_.setAsciiStream(a, b, c), "setAsciiStream", a, b, c)
    override def setAsciiStream(a: String, b: InputStream) = primitive(_.setAsciiStream(a, b), "setAsciiStream", a, b)
    override def setAsciiStream(a: String, b: InputStream, c: Int) = primitive(_.setAsciiStream(a, b, c), "setAsciiStream", a, b, c)
    override def setAsciiStream(a: String, b: InputStream, c: Long) = primitive(_.setAsciiStream(a, b, c), "setAsciiStream", a, b, c)
    override def setBigDecimal(a: Int, b: BigDecimal) = primitive(_.setBigDecimal(a, b), "setBigDecimal", a, b)
    override def setBigDecimal(a: String, b: BigDecimal) = primitive(_.setBigDecimal(a, b), "setBigDecimal", a, b)
    override def setBinaryStream(a: Int, b: InputStream) = primitive(_.setBinaryStream(a, b), "setBinaryStream", a, b)
    override def setBinaryStream(a: Int, b: InputStream, c: Int) = primitive(_.setBinaryStream(a, b, c), "setBinaryStream", a, b, c)
    override def setBinaryStream(a: Int, b: InputStream, c: Long) = primitive(_.setBinaryStream(a, b, c), "setBinaryStream", a, b, c)
    override def setBinaryStream(a: String, b: InputStream) = primitive(_.setBinaryStream(a, b), "setBinaryStream", a, b)
    override def setBinaryStream(a: String, b: InputStream, c: Int) = primitive(_.setBinaryStream(a, b, c), "setBinaryStream", a, b, c)
    override def setBinaryStream(a: String, b: InputStream, c: Long) = primitive(_.setBinaryStream(a, b, c), "setBinaryStream", a, b, c)
    override def setBlob(a: Int, b: Blob) = primitive(_.setBlob(a, b), "setBlob", a, b)
    override def setBlob(a: Int, b: InputStream) = primitive(_.setBlob(a, b), "setBlob", a, b)
    override def setBlob(a: Int, b: InputStream, c: Long) = primitive(_.setBlob(a, b, c), "setBlob", a, b, c)
    override def setBlob(a: String, b: Blob) = primitive(_.setBlob(a, b), "setBlob", a, b)
    override def setBlob(a: String, b: InputStream) = primitive(_.setBlob(a, b), "setBlob", a, b)
    override def setBlob(a: String, b: InputStream, c: Long) = primitive(_.setBlob(a, b, c), "setBlob", a, b, c)
    override def setBoolean(a: Int, b: Boolean) = primitive(_.setBoolean(a, b), "setBoolean", a, b)
    override def setBoolean(a: String, b: Boolean) = primitive(_.setBoolean(a, b), "setBoolean", a, b)
    override def setByte(a: Int, b: Byte) = primitive(_.setByte(a, b), "setByte", a, b)
    override def setByte(a: String, b: Byte) = primitive(_.setByte(a, b), "setByte", a, b)
    override def setBytes(a: Int, b: Array[Byte]) = primitive(_.setBytes(a, b), "setBytes", a, b)
    override def setBytes(a: String, b: Array[Byte]) = primitive(_.setBytes(a, b), "setBytes", a, b)
    override def setCharacterStream(a: Int, b: Reader) = primitive(_.setCharacterStream(a, b), "setCharacterStream", a, b)
    override def setCharacterStream(a: Int, b: Reader, c: Int) = primitive(_.setCharacterStream(a, b, c), "setCharacterStream", a, b, c)
    override def setCharacterStream(a: Int, b: Reader, c: Long) = primitive(_.setCharacterStream(a, b, c), "setCharacterStream", a, b, c)
    override def setCharacterStream(a: String, b: Reader) = primitive(_.setCharacterStream(a, b), "setCharacterStream", a, b)
    override def setCharacterStream(a: String, b: Reader, c: Int) = primitive(_.setCharacterStream(a, b, c), "setCharacterStream", a, b, c)
    override def setCharacterStream(a: String, b: Reader, c: Long) = primitive(_.setCharacterStream(a, b, c), "setCharacterStream", a, b, c)
    override def setClob(a: Int, b: Clob) = primitive(_.setClob(a, b), "setClob", a, b)
    override def setClob(a: Int, b: Reader) = primitive(_.setClob(a, b), "setClob", a, b)
    override def setClob(a: Int, b: Reader, c: Long) = primitive(_.setClob(a, b, c), "setClob", a, b, c)
    override def setClob(a: String, b: Clob) = primitive(_.setClob(a, b), "setClob", a, b)
    override def setClob(a: String, b: Reader) = primitive(_.setClob(a, b), "setClob", a, b)
    override def setClob(a: String, b: Reader, c: Long) = primitive(_.setClob(a, b, c), "setClob", a, b, c)
    override def setCursorName(a: String) = primitive(_.setCursorName(a), "setCursorName", a)
    override def setDate(a: Int, b: Date) = primitive(_.setDate(a, b), "setDate", a, b)
    override def setDate(a: Int, b: Date, c: Calendar) = primitive(_.setDate(a, b, c), "setDate", a, b, c)
    override def setDate(a: String, b: Date) = primitive(_.setDate(a, b), "setDate", a, b)
    override def setDate(a: String, b: Date, c: Calendar) = primitive(_.setDate(a, b, c), "setDate", a, b, c)
    override def setDouble(a: Int, b: Double) = primitive(_.setDouble(a, b), "setDouble", a, b)
    override def setDouble(a: String, b: Double) = primitive(_.setDouble(a, b), "setDouble", a, b)
    override def setEscapeProcessing(a: Boolean) = primitive(_.setEscapeProcessing(a), "setEscapeProcessing", a)
    override def setFetchDirection(a: Int) = primitive(_.setFetchDirection(a), "setFetchDirection", a)
    override def setFetchSize(a: Int) = primitive(_.setFetchSize(a), "setFetchSize", a)
    override def setFloat(a: Int, b: Float) = primitive(_.setFloat(a, b), "setFloat", a, b)
    override def setFloat(a: String, b: Float) = primitive(_.setFloat(a, b), "setFloat", a, b)
    override def setInt(a: Int, b: Int) = primitive(_.setInt(a, b), "setInt", a, b)
    override def setInt(a: String, b: Int) = primitive(_.setInt(a, b), "setInt", a, b)
    override def setLargeMaxRows(a: Long) = primitive(_.setLargeMaxRows(a), "setLargeMaxRows", a)
    override def setLong(a: Int, b: Long) = primitive(_.setLong(a, b), "setLong", a, b)
    override def setLong(a: String, b: Long) = primitive(_.setLong(a, b), "setLong", a, b)
    override def setMaxFieldSize(a: Int) = primitive(_.setMaxFieldSize(a), "setMaxFieldSize", a)
    override def setMaxRows(a: Int) = primitive(_.setMaxRows(a), "setMaxRows", a)
    override def setNCharacterStream(a: Int, b: Reader) = primitive(_.setNCharacterStream(a, b), "setNCharacterStream", a, b)
    override def setNCharacterStream(a: Int, b: Reader, c: Long) = primitive(_.setNCharacterStream(a, b, c), "setNCharacterStream", a, b, c)
    override def setNCharacterStream(a: String, b: Reader) = primitive(_.setNCharacterStream(a, b), "setNCharacterStream", a, b)
    override def setNCharacterStream(a: String, b: Reader, c: Long) = primitive(_.setNCharacterStream(a, b, c), "setNCharacterStream", a, b, c)
    override def setNClob(a: Int, b: NClob) = primitive(_.setNClob(a, b), "setNClob", a, b)
    override def setNClob(a: Int, b: Reader) = primitive(_.setNClob(a, b), "setNClob", a, b)
    override def setNClob(a: Int, b: Reader, c: Long) = primitive(_.setNClob(a, b, c), "setNClob", a, b, c)
    override def setNClob(a: String, b: NClob) = primitive(_.setNClob(a, b), "setNClob", a, b)
    override def setNClob(a: String, b: Reader) = primitive(_.setNClob(a, b), "setNClob", a, b)
    override def setNClob(a: String, b: Reader, c: Long) = primitive(_.setNClob(a, b, c), "setNClob", a, b, c)
    override def setNString(a: Int, b: String) = primitive(_.setNString(a, b), "setNString", a, b)
    override def setNString(a: String, b: String) = primitive(_.setNString(a, b), "setNString", a, b)
    override def setNull(a: Int, b: Int) = primitive(_.setNull(a, b), "setNull", a, b)
    override def setNull(a: Int, b: Int, c: String) = primitive(_.setNull(a, b, c), "setNull", a, b, c)
    override def setNull(a: String, b: Int) = primitive(_.setNull(a, b), "setNull", a, b)
    override def setNull(a: String, b: Int, c: String) = primitive(_.setNull(a, b, c), "setNull", a, b, c)
    override def setObject(a: Int, b: AnyRef) = primitive(_.setObject(a, b), "setObject", a, b)
    override def setObject(a: Int, b: AnyRef, c: Int) = primitive(_.setObject(a, b, c), "setObject", a, b, c)
    override def setObject(a: Int, b: AnyRef, c: Int, d: Int) = primitive(_.setObject(a, b, c, d), "setObject", a, b, c, d)
    override def setObject(a: Int, b: AnyRef, c: SQLType) = primitive(_.setObject(a, b, c), "setObject", a, b, c)
    override def setObject(a: Int, b: AnyRef, c: SQLType, d: Int) = primitive(_.setObject(a, b, c, d), "setObject", a, b, c, d)
    override def setObject(a: String, b: AnyRef) = primitive(_.setObject(a, b), "setObject", a, b)
    override def setObject(a: String, b: AnyRef, c: Int) = primitive(_.setObject(a, b, c), "setObject", a, b, c)
    override def setObject(a: String, b: AnyRef, c: Int, d: Int) = primitive(_.setObject(a, b, c, d), "setObject", a, b, c, d)
    override def setObject(a: String, b: AnyRef, c: SQLType) = primitive(_.setObject(a, b, c), "setObject", a, b, c)
    override def setObject(a: String, b: AnyRef, c: SQLType, d: Int) = primitive(_.setObject(a, b, c, d), "setObject", a, b, c, d)
    override def setPoolable(a: Boolean) = primitive(_.setPoolable(a), "setPoolable", a)
    override def setQueryTimeout(a: Int) = primitive(_.setQueryTimeout(a), "setQueryTimeout", a)
    override def setRef(a: Int, b: Ref) = primitive(_.setRef(a, b), "setRef", a, b)
    override def setRowId(a: Int, b: RowId) = primitive(_.setRowId(a, b), "setRowId", a, b)
    override def setRowId(a: String, b: RowId) = primitive(_.setRowId(a, b), "setRowId", a, b)
    override def setSQLXML(a: Int, b: SQLXML) = primitive(_.setSQLXML(a, b), "setSQLXML", a, b)
    override def setSQLXML(a: String, b: SQLXML) = primitive(_.setSQLXML(a, b), "setSQLXML", a, b)
    override def setShort(a: Int, b: Short) = primitive(_.setShort(a, b), "setShort", a, b)
    override def setShort(a: String, b: Short) = primitive(_.setShort(a, b), "setShort", a, b)
    override def setString(a: Int, b: String) = primitive(_.setString(a, b), "setString", a, b)
    override def setString(a: String, b: String) = primitive(_.setString(a, b), "setString", a, b)
    override def setTime(a: Int, b: Time) = primitive(_.setTime(a, b), "setTime", a, b)
    override def setTime(a: Int, b: Time, c: Calendar) = primitive(_.setTime(a, b, c), "setTime", a, b, c)
    override def setTime(a: String, b: Time) = primitive(_.setTime(a, b), "setTime", a, b)
    override def setTime(a: String, b: Time, c: Calendar) = primitive(_.setTime(a, b, c), "setTime", a, b, c)
    override def setTimestamp(a: Int, b: Timestamp) = primitive(_.setTimestamp(a, b), "setTimestamp", a, b)
    override def setTimestamp(a: Int, b: Timestamp, c: Calendar) = primitive(_.setTimestamp(a, b, c), "setTimestamp", a, b, c)
    override def setTimestamp(a: String, b: Timestamp) = primitive(_.setTimestamp(a, b), "setTimestamp", a, b)
    override def setTimestamp(a: String, b: Timestamp, c: Calendar) = primitive(_.setTimestamp(a, b, c), "setTimestamp", a, b, c)
    override def setURL(a: Int, b: URL) = primitive(_.setURL(a, b), "setURL", a, b)
    override def setURL(a: String, b: URL) = primitive(_.setURL(a, b), "setURL", a, b)
    override def setUnicodeStream(a: Int, b: InputStream, c: Int) = primitive(_.setUnicodeStream(a, b, c), "setUnicodeStream", a, b, c)
    override def unwrap[T](a: Class[T]) = primitive(_.unwrap(a), "unwrap", a)
    override def wasNull = primitive(_.wasNull, "wasNull")

  }

  trait ResultSetInterpreter extends ResultSetOp.Visitor[Kleisli[M, Env[ResultSet], ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: Env[ResultSet] => A): Kleisli[M, Env[ResultSet], A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Env[ResultSet], A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, Env[ResultSet], A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, Env[ResultSet], A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => ResultSetIO[Unit]): Kleisli[M, Env[ResultSet], A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: ResultSetIO[A], f: Throwable => ResultSetIO[A]): Kleisli[M, Env[ResultSet], A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: ResultSetIO[A])(use: A => ResultSetIO[B])(release: (A, ExitCase[Throwable]) => ResultSetIO[Unit]): Kleisli[M, Env[ResultSet], B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, Env[ResultSet], Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: ResultSetIO[A]): Kleisli[M, Env[ResultSet], A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    def liftE[G[_]](env: Env[ResultSet] => G ~> ResultSetIO): Kleisli[M, Env[ResultSet], G ~> ResultSetIO] =
      Kleisli(e => asyncM.pure(env(e)))

    // domain-specific operations are implemented in terms of `primitive`
    override def absolute(a: Int) = primitive(_.absolute(a), "absolute", a)
    override def afterLast = primitive(_.afterLast, "afterLast")
    override def beforeFirst = primitive(_.beforeFirst, "beforeFirst")
    override def cancelRowUpdates = primitive(_.cancelRowUpdates, "cancelRowUpdates")
    override def clearWarnings = primitive(_.clearWarnings, "clearWarnings")
    override def close = primitive(_.close, "close")
    override def deleteRow = primitive(_.deleteRow, "deleteRow")
    override def findColumn(a: String) = primitive(_.findColumn(a), "findColumn", a)
    override def first = primitive(_.first, "first")
    override def getArray(a: Int) = primitive(_.getArray(a), "getArray", a)
    override def getArray(a: String) = primitive(_.getArray(a), "getArray", a)
    override def getAsciiStream(a: Int) = primitive(_.getAsciiStream(a), "getAsciiStream", a)
    override def getAsciiStream(a: String) = primitive(_.getAsciiStream(a), "getAsciiStream", a)
    override def getBigDecimal(a: Int) = primitive(_.getBigDecimal(a), "getBigDecimal", a)
    override def getBigDecimal(a: Int, b: Int) = primitive(_.getBigDecimal(a, b), "getBigDecimal", a, b)
    override def getBigDecimal(a: String) = primitive(_.getBigDecimal(a), "getBigDecimal", a)
    override def getBigDecimal(a: String, b: Int) = primitive(_.getBigDecimal(a, b), "getBigDecimal", a, b)
    override def getBinaryStream(a: Int) = primitive(_.getBinaryStream(a), "getBinaryStream", a)
    override def getBinaryStream(a: String) = primitive(_.getBinaryStream(a), "getBinaryStream", a)
    override def getBlob(a: Int) = primitive(_.getBlob(a), "getBlob", a)
    override def getBlob(a: String) = primitive(_.getBlob(a), "getBlob", a)
    override def getBoolean(a: Int) = primitive(_.getBoolean(a), "getBoolean", a)
    override def getBoolean(a: String) = primitive(_.getBoolean(a), "getBoolean", a)
    override def getByte(a: Int) = primitive(_.getByte(a), "getByte", a)
    override def getByte(a: String) = primitive(_.getByte(a), "getByte", a)
    override def getBytes(a: Int) = primitive(_.getBytes(a), "getBytes", a)
    override def getBytes(a: String) = primitive(_.getBytes(a), "getBytes", a)
    override def getCharacterStream(a: Int) = primitive(_.getCharacterStream(a), "getCharacterStream", a)
    override def getCharacterStream(a: String) = primitive(_.getCharacterStream(a), "getCharacterStream", a)
    override def getClob(a: Int) = primitive(_.getClob(a), "getClob", a)
    override def getClob(a: String) = primitive(_.getClob(a), "getClob", a)
    override def getConcurrency = primitive(_.getConcurrency, "getConcurrency")
    override def getCursorName = primitive(_.getCursorName, "getCursorName")
    override def getDate(a: Int) = primitive(_.getDate(a), "getDate", a)
    override def getDate(a: Int, b: Calendar) = primitive(_.getDate(a, b), "getDate", a, b)
    override def getDate(a: String) = primitive(_.getDate(a), "getDate", a)
    override def getDate(a: String, b: Calendar) = primitive(_.getDate(a, b), "getDate", a, b)
    override def getDouble(a: Int) = primitive(_.getDouble(a), "getDouble", a)
    override def getDouble(a: String) = primitive(_.getDouble(a), "getDouble", a)
    override def getFetchDirection = primitive(_.getFetchDirection, "getFetchDirection")
    override def getFetchSize = primitive(_.getFetchSize, "getFetchSize")
    override def getFloat(a: Int) = primitive(_.getFloat(a), "getFloat", a)
    override def getFloat(a: String) = primitive(_.getFloat(a), "getFloat", a)
    override def getHoldability = primitive(_.getHoldability, "getHoldability")
    override def getInt(a: Int) = primitive(_.getInt(a), "getInt", a)
    override def getInt(a: String) = primitive(_.getInt(a), "getInt", a)
    override def getLong(a: Int) = primitive(_.getLong(a), "getLong", a)
    override def getLong(a: String) = primitive(_.getLong(a), "getLong", a)
    override def getMetaData = primitive(_.getMetaData, "getMetaData")
    override def getNCharacterStream(a: Int) = primitive(_.getNCharacterStream(a), "getNCharacterStream", a)
    override def getNCharacterStream(a: String) = primitive(_.getNCharacterStream(a), "getNCharacterStream", a)
    override def getNClob(a: Int) = primitive(_.getNClob(a), "getNClob", a)
    override def getNClob(a: String) = primitive(_.getNClob(a), "getNClob", a)
    override def getNString(a: Int) = primitive(_.getNString(a), "getNString", a)
    override def getNString(a: String) = primitive(_.getNString(a), "getNString", a)
    override def getObject(a: Int) = primitive(_.getObject(a), "getObject", a)
    override def getObject[T](a: Int, b: Class[T]) = primitive(_.getObject(a, b), "getObject", a, b)
    override def getObject(a: Int, b: Map[String, Class[_]]) = primitive(_.getObject(a, b), "getObject", a, b)
    override def getObject(a: String) = primitive(_.getObject(a), "getObject", a)
    override def getObject[T](a: String, b: Class[T]) = primitive(_.getObject(a, b), "getObject", a, b)
    override def getObject(a: String, b: Map[String, Class[_]]) = primitive(_.getObject(a, b), "getObject", a, b)
    override def getRef(a: Int) = primitive(_.getRef(a), "getRef", a)
    override def getRef(a: String) = primitive(_.getRef(a), "getRef", a)
    override def getRow = primitive(_.getRow, "getRow")
    override def getRowId(a: Int) = primitive(_.getRowId(a), "getRowId", a)
    override def getRowId(a: String) = primitive(_.getRowId(a), "getRowId", a)
    override def getSQLXML(a: Int) = primitive(_.getSQLXML(a), "getSQLXML", a)
    override def getSQLXML(a: String) = primitive(_.getSQLXML(a), "getSQLXML", a)
    override def getShort(a: Int) = primitive(_.getShort(a), "getShort", a)
    override def getShort(a: String) = primitive(_.getShort(a), "getShort", a)
    override def getStatement = primitive(_.getStatement, "getStatement")
    override def getString(a: Int) = primitive(_.getString(a), "getString", a)
    override def getString(a: String) = primitive(_.getString(a), "getString", a)
    override def getTime(a: Int) = primitive(_.getTime(a), "getTime", a)
    override def getTime(a: Int, b: Calendar) = primitive(_.getTime(a, b), "getTime", a, b)
    override def getTime(a: String) = primitive(_.getTime(a), "getTime", a)
    override def getTime(a: String, b: Calendar) = primitive(_.getTime(a, b), "getTime", a, b)
    override def getTimestamp(a: Int) = primitive(_.getTimestamp(a), "getTimestamp", a)
    override def getTimestamp(a: Int, b: Calendar) = primitive(_.getTimestamp(a, b), "getTimestamp", a, b)
    override def getTimestamp(a: String) = primitive(_.getTimestamp(a), "getTimestamp", a)
    override def getTimestamp(a: String, b: Calendar) = primitive(_.getTimestamp(a, b), "getTimestamp", a, b)
    override def getType = primitive(_.getType, "getType")
    override def getURL(a: Int) = primitive(_.getURL(a), "getURL", a)
    override def getURL(a: String) = primitive(_.getURL(a), "getURL", a)
    override def getUnicodeStream(a: Int) = primitive(_.getUnicodeStream(a), "getUnicodeStream", a)
    override def getUnicodeStream(a: String) = primitive(_.getUnicodeStream(a), "getUnicodeStream", a)
    override def getWarnings = primitive(_.getWarnings, "getWarnings")
    override def insertRow = primitive(_.insertRow, "insertRow")
    override def isAfterLast = primitive(_.isAfterLast, "isAfterLast")
    override def isBeforeFirst = primitive(_.isBeforeFirst, "isBeforeFirst")
    override def isClosed = primitive(_.isClosed, "isClosed")
    override def isFirst = primitive(_.isFirst, "isFirst")
    override def isLast = primitive(_.isLast, "isLast")
    override def isWrapperFor(a: Class[_]) = primitive(_.isWrapperFor(a), "isWrapperFor", a)
    override def last = primitive(_.last, "last")
    override def moveToCurrentRow = primitive(_.moveToCurrentRow, "moveToCurrentRow")
    override def moveToInsertRow = primitive(_.moveToInsertRow, "moveToInsertRow")
    override def next = primitive(_.next, "next")
    override def previous = primitive(_.previous, "previous")
    override def refreshRow = primitive(_.refreshRow, "refreshRow")
    override def relative(a: Int) = primitive(_.relative(a), "relative", a)
    override def rowDeleted = primitive(_.rowDeleted, "rowDeleted")
    override def rowInserted = primitive(_.rowInserted, "rowInserted")
    override def rowUpdated = primitive(_.rowUpdated, "rowUpdated")
    override def setFetchDirection(a: Int) = primitive(_.setFetchDirection(a), "setFetchDirection", a)
    override def setFetchSize(a: Int) = primitive(_.setFetchSize(a), "setFetchSize", a)
    override def unwrap[T](a: Class[T]) = primitive(_.unwrap(a), "unwrap", a)
    override def updateArray(a: Int, b: SqlArray) = primitive(_.updateArray(a, b), "updateArray", a, b)
    override def updateArray(a: String, b: SqlArray) = primitive(_.updateArray(a, b), "updateArray", a, b)
    override def updateAsciiStream(a: Int, b: InputStream) = primitive(_.updateAsciiStream(a, b), "updateAsciiStream", a, b)
    override def updateAsciiStream(a: Int, b: InputStream, c: Int) = primitive(_.updateAsciiStream(a, b, c), "updateAsciiStream", a, b, c)
    override def updateAsciiStream(a: Int, b: InputStream, c: Long) = primitive(_.updateAsciiStream(a, b, c), "updateAsciiStream", a, b, c)
    override def updateAsciiStream(a: String, b: InputStream) = primitive(_.updateAsciiStream(a, b), "updateAsciiStream", a, b)
    override def updateAsciiStream(a: String, b: InputStream, c: Int) = primitive(_.updateAsciiStream(a, b, c), "updateAsciiStream", a, b, c)
    override def updateAsciiStream(a: String, b: InputStream, c: Long) = primitive(_.updateAsciiStream(a, b, c), "updateAsciiStream", a, b, c)
    override def updateBigDecimal(a: Int, b: BigDecimal) = primitive(_.updateBigDecimal(a, b), "updateBigDecimal", a, b)
    override def updateBigDecimal(a: String, b: BigDecimal) = primitive(_.updateBigDecimal(a, b), "updateBigDecimal", a, b)
    override def updateBinaryStream(a: Int, b: InputStream) = primitive(_.updateBinaryStream(a, b), "updateBinaryStream", a, b)
    override def updateBinaryStream(a: Int, b: InputStream, c: Int) = primitive(_.updateBinaryStream(a, b, c), "updateBinaryStream", a, b, c)
    override def updateBinaryStream(a: Int, b: InputStream, c: Long) = primitive(_.updateBinaryStream(a, b, c), "updateBinaryStream", a, b, c)
    override def updateBinaryStream(a: String, b: InputStream) = primitive(_.updateBinaryStream(a, b), "updateBinaryStream", a, b)
    override def updateBinaryStream(a: String, b: InputStream, c: Int) = primitive(_.updateBinaryStream(a, b, c), "updateBinaryStream", a, b, c)
    override def updateBinaryStream(a: String, b: InputStream, c: Long) = primitive(_.updateBinaryStream(a, b, c), "updateBinaryStream", a, b, c)
    override def updateBlob(a: Int, b: Blob) = primitive(_.updateBlob(a, b), "updateBlob", a, b)
    override def updateBlob(a: Int, b: InputStream) = primitive(_.updateBlob(a, b), "updateBlob", a, b)
    override def updateBlob(a: Int, b: InputStream, c: Long) = primitive(_.updateBlob(a, b, c), "updateBlob", a, b, c)
    override def updateBlob(a: String, b: Blob) = primitive(_.updateBlob(a, b), "updateBlob", a, b)
    override def updateBlob(a: String, b: InputStream) = primitive(_.updateBlob(a, b), "updateBlob", a, b)
    override def updateBlob(a: String, b: InputStream, c: Long) = primitive(_.updateBlob(a, b, c), "updateBlob", a, b, c)
    override def updateBoolean(a: Int, b: Boolean) = primitive(_.updateBoolean(a, b), "updateBoolean", a, b)
    override def updateBoolean(a: String, b: Boolean) = primitive(_.updateBoolean(a, b), "updateBoolean", a, b)
    override def updateByte(a: Int, b: Byte) = primitive(_.updateByte(a, b), "updateByte", a, b)
    override def updateByte(a: String, b: Byte) = primitive(_.updateByte(a, b), "updateByte", a, b)
    override def updateBytes(a: Int, b: Array[Byte]) = primitive(_.updateBytes(a, b), "updateBytes", a, b)
    override def updateBytes(a: String, b: Array[Byte]) = primitive(_.updateBytes(a, b), "updateBytes", a, b)
    override def updateCharacterStream(a: Int, b: Reader) = primitive(_.updateCharacterStream(a, b), "updateCharacterStream", a, b)
    override def updateCharacterStream(a: Int, b: Reader, c: Int) = primitive(_.updateCharacterStream(a, b, c), "updateCharacterStream", a, b, c)
    override def updateCharacterStream(a: Int, b: Reader, c: Long) = primitive(_.updateCharacterStream(a, b, c), "updateCharacterStream", a, b, c)
    override def updateCharacterStream(a: String, b: Reader) = primitive(_.updateCharacterStream(a, b), "updateCharacterStream", a, b)
    override def updateCharacterStream(a: String, b: Reader, c: Int) = primitive(_.updateCharacterStream(a, b, c), "updateCharacterStream", a, b, c)
    override def updateCharacterStream(a: String, b: Reader, c: Long) = primitive(_.updateCharacterStream(a, b, c), "updateCharacterStream", a, b, c)
    override def updateClob(a: Int, b: Clob) = primitive(_.updateClob(a, b), "updateClob", a, b)
    override def updateClob(a: Int, b: Reader) = primitive(_.updateClob(a, b), "updateClob", a, b)
    override def updateClob(a: Int, b: Reader, c: Long) = primitive(_.updateClob(a, b, c), "updateClob", a, b, c)
    override def updateClob(a: String, b: Clob) = primitive(_.updateClob(a, b), "updateClob", a, b)
    override def updateClob(a: String, b: Reader) = primitive(_.updateClob(a, b), "updateClob", a, b)
    override def updateClob(a: String, b: Reader, c: Long) = primitive(_.updateClob(a, b, c), "updateClob", a, b, c)
    override def updateDate(a: Int, b: Date) = primitive(_.updateDate(a, b), "updateDate", a, b)
    override def updateDate(a: String, b: Date) = primitive(_.updateDate(a, b), "updateDate", a, b)
    override def updateDouble(a: Int, b: Double) = primitive(_.updateDouble(a, b), "updateDouble", a, b)
    override def updateDouble(a: String, b: Double) = primitive(_.updateDouble(a, b), "updateDouble", a, b)
    override def updateFloat(a: Int, b: Float) = primitive(_.updateFloat(a, b), "updateFloat", a, b)
    override def updateFloat(a: String, b: Float) = primitive(_.updateFloat(a, b), "updateFloat", a, b)
    override def updateInt(a: Int, b: Int) = primitive(_.updateInt(a, b), "updateInt", a, b)
    override def updateInt(a: String, b: Int) = primitive(_.updateInt(a, b), "updateInt", a, b)
    override def updateLong(a: Int, b: Long) = primitive(_.updateLong(a, b), "updateLong", a, b)
    override def updateLong(a: String, b: Long) = primitive(_.updateLong(a, b), "updateLong", a, b)
    override def updateNCharacterStream(a: Int, b: Reader) = primitive(_.updateNCharacterStream(a, b), "updateNCharacterStream", a, b)
    override def updateNCharacterStream(a: Int, b: Reader, c: Long) = primitive(_.updateNCharacterStream(a, b, c), "updateNCharacterStream", a, b, c)
    override def updateNCharacterStream(a: String, b: Reader) = primitive(_.updateNCharacterStream(a, b), "updateNCharacterStream", a, b)
    override def updateNCharacterStream(a: String, b: Reader, c: Long) = primitive(_.updateNCharacterStream(a, b, c), "updateNCharacterStream", a, b, c)
    override def updateNClob(a: Int, b: NClob) = primitive(_.updateNClob(a, b), "updateNClob", a, b)
    override def updateNClob(a: Int, b: Reader) = primitive(_.updateNClob(a, b), "updateNClob", a, b)
    override def updateNClob(a: Int, b: Reader, c: Long) = primitive(_.updateNClob(a, b, c), "updateNClob", a, b, c)
    override def updateNClob(a: String, b: NClob) = primitive(_.updateNClob(a, b), "updateNClob", a, b)
    override def updateNClob(a: String, b: Reader) = primitive(_.updateNClob(a, b), "updateNClob", a, b)
    override def updateNClob(a: String, b: Reader, c: Long) = primitive(_.updateNClob(a, b, c), "updateNClob", a, b, c)
    override def updateNString(a: Int, b: String) = primitive(_.updateNString(a, b), "updateNString", a, b)
    override def updateNString(a: String, b: String) = primitive(_.updateNString(a, b), "updateNString", a, b)
    override def updateNull(a: Int) = primitive(_.updateNull(a), "updateNull", a)
    override def updateNull(a: String) = primitive(_.updateNull(a), "updateNull", a)
    override def updateObject(a: Int, b: AnyRef) = primitive(_.updateObject(a, b), "updateObject", a, b)
    override def updateObject(a: Int, b: AnyRef, c: Int) = primitive(_.updateObject(a, b, c), "updateObject", a, b, c)
    override def updateObject(a: Int, b: AnyRef, c: SQLType) = primitive(_.updateObject(a, b, c), "updateObject", a, b, c)
    override def updateObject(a: Int, b: AnyRef, c: SQLType, d: Int) = primitive(_.updateObject(a, b, c, d), "updateObject", a, b, c, d)
    override def updateObject(a: String, b: AnyRef) = primitive(_.updateObject(a, b), "updateObject", a, b)
    override def updateObject(a: String, b: AnyRef, c: Int) = primitive(_.updateObject(a, b, c), "updateObject", a, b, c)
    override def updateObject(a: String, b: AnyRef, c: SQLType) = primitive(_.updateObject(a, b, c), "updateObject", a, b, c)
    override def updateObject(a: String, b: AnyRef, c: SQLType, d: Int) = primitive(_.updateObject(a, b, c, d), "updateObject", a, b, c, d)
    override def updateRef(a: Int, b: Ref) = primitive(_.updateRef(a, b), "updateRef", a, b)
    override def updateRef(a: String, b: Ref) = primitive(_.updateRef(a, b), "updateRef", a, b)
    override def updateRow = primitive(_.updateRow, "updateRow")
    override def updateRowId(a: Int, b: RowId) = primitive(_.updateRowId(a, b), "updateRowId", a, b)
    override def updateRowId(a: String, b: RowId) = primitive(_.updateRowId(a, b), "updateRowId", a, b)
    override def updateSQLXML(a: Int, b: SQLXML) = primitive(_.updateSQLXML(a, b), "updateSQLXML", a, b)
    override def updateSQLXML(a: String, b: SQLXML) = primitive(_.updateSQLXML(a, b), "updateSQLXML", a, b)
    override def updateShort(a: Int, b: Short) = primitive(_.updateShort(a, b), "updateShort", a, b)
    override def updateShort(a: String, b: Short) = primitive(_.updateShort(a, b), "updateShort", a, b)
    override def updateString(a: Int, b: String) = primitive(_.updateString(a, b), "updateString", a, b)
    override def updateString(a: String, b: String) = primitive(_.updateString(a, b), "updateString", a, b)
    override def updateTime(a: Int, b: Time) = primitive(_.updateTime(a, b), "updateTime", a, b)
    override def updateTime(a: String, b: Time) = primitive(_.updateTime(a, b), "updateTime", a, b)
    override def updateTimestamp(a: Int, b: Timestamp) = primitive(_.updateTimestamp(a, b), "updateTimestamp", a, b)
    override def updateTimestamp(a: String, b: Timestamp) = primitive(_.updateTimestamp(a, b), "updateTimestamp", a, b)
    override def wasNull = primitive(_.wasNull, "wasNull")

  }


}

