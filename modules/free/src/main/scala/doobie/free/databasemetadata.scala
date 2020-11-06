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

import java.lang.Class
import java.lang.String
import java.sql.Connection
import java.sql.DatabaseMetaData
import java.sql.ResultSet
import java.sql.RowIdLifetime

@silent("deprecated")
object databasemetadata { module =>

  // Algebra of operations for DatabaseMetaData. Each accepts a visitor as an alternative to pattern-matching.
  sealed trait DatabaseMetaDataOp[A] {
    def visit[F[_]](v: DatabaseMetaDataOp.Visitor[F]): F[A]
  }

  // Free monad over DatabaseMetaDataOp.
  type DatabaseMetaDataIO[A] = FF[DatabaseMetaDataOp, A]

  // Module of instances and constructors of DatabaseMetaDataOp.
  object DatabaseMetaDataOp {

    // Given a DatabaseMetaData we can embed a DatabaseMetaDataIO program in any algebra that understands embedding.
    implicit val DatabaseMetaDataOpEmbeddable: Embeddable[DatabaseMetaDataOp, DatabaseMetaData] =
      new Embeddable[DatabaseMetaDataOp, DatabaseMetaData] {
        def embed[A](j: DatabaseMetaData, fa: FF[DatabaseMetaDataOp, A]) = Embedded.DatabaseMetaData(j, fa)
      }

    // Interface for a natural transformation DatabaseMetaDataOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (DatabaseMetaDataOp ~> F) {
      final def apply[A](fa: DatabaseMetaDataOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](message: => String, f: DatabaseMetaData => A): F[A]
      def embed[A](e: Embedded[A]): F[A]

      // Async
      def delay[A](a: () => A): F[A]
      def handleErrorWith[A](fa: DatabaseMetaDataIO[A], f: Throwable => DatabaseMetaDataIO[A]): F[A]
      def raiseError[A](e: Throwable): F[A]
      def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]
      def asyncF[A](k: (Either[Throwable, A] => Unit) => DatabaseMetaDataIO[Unit]): F[A]
      def bracketCase[A, B](acquire: DatabaseMetaDataIO[A])(use: A => DatabaseMetaDataIO[B])(release: (A, ExitCase[Throwable]) => DatabaseMetaDataIO[Unit]): F[B]
      def shift: F[Unit]
      def evalOn[A](ec: ExecutionContext)(fa: DatabaseMetaDataIO[A]): F[A]

      // Logger
      def error(message: => String): F[Unit]
      def warn(message: => String): F[Unit]
      def info(message: => String): F[Unit]
      def debug(message: => String): F[Unit]
      def trace(message: => String): F[Unit]

      // DatabaseMetaData
      def allProceduresAreCallable: F[Boolean]
      def allTablesAreSelectable: F[Boolean]
      def autoCommitFailureClosesAllResultSets: F[Boolean]
      def dataDefinitionCausesTransactionCommit: F[Boolean]
      def dataDefinitionIgnoredInTransactions: F[Boolean]
      def deletesAreDetected(a: Int): F[Boolean]
      def doesMaxRowSizeIncludeBlobs: F[Boolean]
      def generatedKeyAlwaysReturned: F[Boolean]
      def getAttributes(a: String, b: String, c: String, d: String): F[ResultSet]
      def getBestRowIdentifier(a: String, b: String, c: String, d: Int, e: Boolean): F[ResultSet]
      def getCatalogSeparator: F[String]
      def getCatalogTerm: F[String]
      def getCatalogs: F[ResultSet]
      def getClientInfoProperties: F[ResultSet]
      def getColumnPrivileges(a: String, b: String, c: String, d: String): F[ResultSet]
      def getColumns(a: String, b: String, c: String, d: String): F[ResultSet]
      def getConnection: F[Connection]
      def getCrossReference(a: String, b: String, c: String, d: String, e: String, f: String): F[ResultSet]
      def getDatabaseMajorVersion: F[Int]
      def getDatabaseMinorVersion: F[Int]
      def getDatabaseProductName: F[String]
      def getDatabaseProductVersion: F[String]
      def getDefaultTransactionIsolation: F[Int]
      def getDriverMajorVersion: F[Int]
      def getDriverMinorVersion: F[Int]
      def getDriverName: F[String]
      def getDriverVersion: F[String]
      def getExportedKeys(a: String, b: String, c: String): F[ResultSet]
      def getExtraNameCharacters: F[String]
      def getFunctionColumns(a: String, b: String, c: String, d: String): F[ResultSet]
      def getFunctions(a: String, b: String, c: String): F[ResultSet]
      def getIdentifierQuoteString: F[String]
      def getImportedKeys(a: String, b: String, c: String): F[ResultSet]
      def getIndexInfo(a: String, b: String, c: String, d: Boolean, e: Boolean): F[ResultSet]
      def getJDBCMajorVersion: F[Int]
      def getJDBCMinorVersion: F[Int]
      def getMaxBinaryLiteralLength: F[Int]
      def getMaxCatalogNameLength: F[Int]
      def getMaxCharLiteralLength: F[Int]
      def getMaxColumnNameLength: F[Int]
      def getMaxColumnsInGroupBy: F[Int]
      def getMaxColumnsInIndex: F[Int]
      def getMaxColumnsInOrderBy: F[Int]
      def getMaxColumnsInSelect: F[Int]
      def getMaxColumnsInTable: F[Int]
      def getMaxConnections: F[Int]
      def getMaxCursorNameLength: F[Int]
      def getMaxIndexLength: F[Int]
      def getMaxLogicalLobSize: F[Long]
      def getMaxProcedureNameLength: F[Int]
      def getMaxRowSize: F[Int]
      def getMaxSchemaNameLength: F[Int]
      def getMaxStatementLength: F[Int]
      def getMaxStatements: F[Int]
      def getMaxTableNameLength: F[Int]
      def getMaxTablesInSelect: F[Int]
      def getMaxUserNameLength: F[Int]
      def getNumericFunctions: F[String]
      def getPrimaryKeys(a: String, b: String, c: String): F[ResultSet]
      def getProcedureColumns(a: String, b: String, c: String, d: String): F[ResultSet]
      def getProcedureTerm: F[String]
      def getProcedures(a: String, b: String, c: String): F[ResultSet]
      def getPseudoColumns(a: String, b: String, c: String, d: String): F[ResultSet]
      def getResultSetHoldability: F[Int]
      def getRowIdLifetime: F[RowIdLifetime]
      def getSQLKeywords: F[String]
      def getSQLStateType: F[Int]
      def getSchemaTerm: F[String]
      def getSchemas: F[ResultSet]
      def getSchemas(a: String, b: String): F[ResultSet]
      def getSearchStringEscape: F[String]
      def getStringFunctions: F[String]
      def getSuperTables(a: String, b: String, c: String): F[ResultSet]
      def getSuperTypes(a: String, b: String, c: String): F[ResultSet]
      def getSystemFunctions: F[String]
      def getTablePrivileges(a: String, b: String, c: String): F[ResultSet]
      def getTableTypes: F[ResultSet]
      def getTables(a: String, b: String, c: String, d: Array[String]): F[ResultSet]
      def getTimeDateFunctions: F[String]
      def getTypeInfo: F[ResultSet]
      def getUDTs(a: String, b: String, c: String, d: Array[Int]): F[ResultSet]
      def getURL: F[String]
      def getUserName: F[String]
      def getVersionColumns(a: String, b: String, c: String): F[ResultSet]
      def insertsAreDetected(a: Int): F[Boolean]
      def isCatalogAtStart: F[Boolean]
      def isReadOnly: F[Boolean]
      def isWrapperFor(a: Class[_]): F[Boolean]
      def locatorsUpdateCopy: F[Boolean]
      def nullPlusNonNullIsNull: F[Boolean]
      def nullsAreSortedAtEnd: F[Boolean]
      def nullsAreSortedAtStart: F[Boolean]
      def nullsAreSortedHigh: F[Boolean]
      def nullsAreSortedLow: F[Boolean]
      def othersDeletesAreVisible(a: Int): F[Boolean]
      def othersInsertsAreVisible(a: Int): F[Boolean]
      def othersUpdatesAreVisible(a: Int): F[Boolean]
      def ownDeletesAreVisible(a: Int): F[Boolean]
      def ownInsertsAreVisible(a: Int): F[Boolean]
      def ownUpdatesAreVisible(a: Int): F[Boolean]
      def storesLowerCaseIdentifiers: F[Boolean]
      def storesLowerCaseQuotedIdentifiers: F[Boolean]
      def storesMixedCaseIdentifiers: F[Boolean]
      def storesMixedCaseQuotedIdentifiers: F[Boolean]
      def storesUpperCaseIdentifiers: F[Boolean]
      def storesUpperCaseQuotedIdentifiers: F[Boolean]
      def supportsANSI92EntryLevelSQL: F[Boolean]
      def supportsANSI92FullSQL: F[Boolean]
      def supportsANSI92IntermediateSQL: F[Boolean]
      def supportsAlterTableWithAddColumn: F[Boolean]
      def supportsAlterTableWithDropColumn: F[Boolean]
      def supportsBatchUpdates: F[Boolean]
      def supportsCatalogsInDataManipulation: F[Boolean]
      def supportsCatalogsInIndexDefinitions: F[Boolean]
      def supportsCatalogsInPrivilegeDefinitions: F[Boolean]
      def supportsCatalogsInProcedureCalls: F[Boolean]
      def supportsCatalogsInTableDefinitions: F[Boolean]
      def supportsColumnAliasing: F[Boolean]
      def supportsConvert: F[Boolean]
      def supportsConvert(a: Int, b: Int): F[Boolean]
      def supportsCoreSQLGrammar: F[Boolean]
      def supportsCorrelatedSubqueries: F[Boolean]
      def supportsDataDefinitionAndDataManipulationTransactions: F[Boolean]
      def supportsDataManipulationTransactionsOnly: F[Boolean]
      def supportsDifferentTableCorrelationNames: F[Boolean]
      def supportsExpressionsInOrderBy: F[Boolean]
      def supportsExtendedSQLGrammar: F[Boolean]
      def supportsFullOuterJoins: F[Boolean]
      def supportsGetGeneratedKeys: F[Boolean]
      def supportsGroupBy: F[Boolean]
      def supportsGroupByBeyondSelect: F[Boolean]
      def supportsGroupByUnrelated: F[Boolean]
      def supportsIntegrityEnhancementFacility: F[Boolean]
      def supportsLikeEscapeClause: F[Boolean]
      def supportsLimitedOuterJoins: F[Boolean]
      def supportsMinimumSQLGrammar: F[Boolean]
      def supportsMixedCaseIdentifiers: F[Boolean]
      def supportsMixedCaseQuotedIdentifiers: F[Boolean]
      def supportsMultipleOpenResults: F[Boolean]
      def supportsMultipleResultSets: F[Boolean]
      def supportsMultipleTransactions: F[Boolean]
      def supportsNamedParameters: F[Boolean]
      def supportsNonNullableColumns: F[Boolean]
      def supportsOpenCursorsAcrossCommit: F[Boolean]
      def supportsOpenCursorsAcrossRollback: F[Boolean]
      def supportsOpenStatementsAcrossCommit: F[Boolean]
      def supportsOpenStatementsAcrossRollback: F[Boolean]
      def supportsOrderByUnrelated: F[Boolean]
      def supportsOuterJoins: F[Boolean]
      def supportsPositionedDelete: F[Boolean]
      def supportsPositionedUpdate: F[Boolean]
      def supportsRefCursors: F[Boolean]
      def supportsResultSetConcurrency(a: Int, b: Int): F[Boolean]
      def supportsResultSetHoldability(a: Int): F[Boolean]
      def supportsResultSetType(a: Int): F[Boolean]
      def supportsSavepoints: F[Boolean]
      def supportsSchemasInDataManipulation: F[Boolean]
      def supportsSchemasInIndexDefinitions: F[Boolean]
      def supportsSchemasInPrivilegeDefinitions: F[Boolean]
      def supportsSchemasInProcedureCalls: F[Boolean]
      def supportsSchemasInTableDefinitions: F[Boolean]
      def supportsSelectForUpdate: F[Boolean]
      def supportsSharding: F[Boolean]
      def supportsStatementPooling: F[Boolean]
      def supportsStoredFunctionsUsingCallSyntax: F[Boolean]
      def supportsStoredProcedures: F[Boolean]
      def supportsSubqueriesInComparisons: F[Boolean]
      def supportsSubqueriesInExists: F[Boolean]
      def supportsSubqueriesInIns: F[Boolean]
      def supportsSubqueriesInQuantifieds: F[Boolean]
      def supportsTableCorrelationNames: F[Boolean]
      def supportsTransactionIsolationLevel(a: Int): F[Boolean]
      def supportsTransactions: F[Boolean]
      def supportsUnion: F[Boolean]
      def supportsUnionAll: F[Boolean]
      def unwrap[T](a: Class[T]): F[T]
      def updatesAreDetected(a: Int): F[Boolean]
      def usesLocalFilePerTable: F[Boolean]
      def usesLocalFiles: F[Boolean]

    }

    // Common operations for all algebras.
    final case class Raw[A](message: () => String, f: DatabaseMetaData => A) extends DatabaseMetaDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(message(), f)
    }
    final case class Embed[A](e: Embedded[A]) extends DatabaseMetaDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class Delay[A](a: () => A) extends DatabaseMetaDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.delay(a)
    }
    final case class HandleErrorWith[A](fa: DatabaseMetaDataIO[A], f: Throwable => DatabaseMetaDataIO[A]) extends DatabaseMetaDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa, f)
    }
    final case class RaiseError[A](e: Throwable) extends DatabaseMetaDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raiseError(e)
    }
    final case class Async1[A](k: (Either[Throwable, A] => Unit) => Unit) extends DatabaseMetaDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.async(k)
    }
    final case class AsyncF[A](k: (Either[Throwable, A] => Unit) => DatabaseMetaDataIO[Unit]) extends DatabaseMetaDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.asyncF(k)
    }
    final case class BracketCase[A, B](acquire: DatabaseMetaDataIO[A], use: A => DatabaseMetaDataIO[B], release: (A, ExitCase[Throwable]) => DatabaseMetaDataIO[Unit]) extends DatabaseMetaDataOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.bracketCase(acquire)(use)(release)
    }
    final case object Shift extends DatabaseMetaDataOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.shift
    }
    final case class EvalOn[A](ec: ExecutionContext, fa: DatabaseMetaDataIO[A]) extends DatabaseMetaDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.evalOn(ec)(fa)
    }
    final case class LogError(message: () => String) extends DatabaseMetaDataOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.error(message()): F[Unit]
    }
    final case class LogWarn(message: () => String) extends DatabaseMetaDataOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.warn(message()): F[Unit]
    }
    final case class LogInfo(message: () => String) extends DatabaseMetaDataOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.info(message()): F[Unit]
    }
    final case class LogDebug(message: () => String) extends DatabaseMetaDataOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.debug(message()): F[Unit]
    }
    final case class LogTrace(message: () => String) extends DatabaseMetaDataOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.trace(message()): F[Unit]
    }

    // DatabaseMetaData-specific operations.
    final case object AllProceduresAreCallable extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.allProceduresAreCallable
    }
    final case object AllTablesAreSelectable extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.allTablesAreSelectable
    }
    final case object AutoCommitFailureClosesAllResultSets extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.autoCommitFailureClosesAllResultSets
    }
    final case object DataDefinitionCausesTransactionCommit extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.dataDefinitionCausesTransactionCommit
    }
    final case object DataDefinitionIgnoredInTransactions extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.dataDefinitionIgnoredInTransactions
    }
    final case class  DeletesAreDetected(a: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.deletesAreDetected(a)
    }
    final case object DoesMaxRowSizeIncludeBlobs extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.doesMaxRowSizeIncludeBlobs
    }
    final case object GeneratedKeyAlwaysReturned extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.generatedKeyAlwaysReturned
    }
    final case class  GetAttributes(a: String, b: String, c: String, d: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getAttributes(a, b, c, d)
    }
    final case class  GetBestRowIdentifier(a: String, b: String, c: String, d: Int, e: Boolean) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getBestRowIdentifier(a, b, c, d, e)
    }
    final case object GetCatalogSeparator extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getCatalogSeparator
    }
    final case object GetCatalogTerm extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getCatalogTerm
    }
    final case object GetCatalogs extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getCatalogs
    }
    final case object GetClientInfoProperties extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getClientInfoProperties
    }
    final case class  GetColumnPrivileges(a: String, b: String, c: String, d: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getColumnPrivileges(a, b, c, d)
    }
    final case class  GetColumns(a: String, b: String, c: String, d: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getColumns(a, b, c, d)
    }
    final case object GetConnection extends DatabaseMetaDataOp[Connection] {
      def visit[F[_]](v: Visitor[F]) = v.getConnection
    }
    final case class  GetCrossReference(a: String, b: String, c: String, d: String, e: String, f: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getCrossReference(a, b, c, d, e, f)
    }
    final case object GetDatabaseMajorVersion extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getDatabaseMajorVersion
    }
    final case object GetDatabaseMinorVersion extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getDatabaseMinorVersion
    }
    final case object GetDatabaseProductName extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getDatabaseProductName
    }
    final case object GetDatabaseProductVersion extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getDatabaseProductVersion
    }
    final case object GetDefaultTransactionIsolation extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getDefaultTransactionIsolation
    }
    final case object GetDriverMajorVersion extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getDriverMajorVersion
    }
    final case object GetDriverMinorVersion extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getDriverMinorVersion
    }
    final case object GetDriverName extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getDriverName
    }
    final case object GetDriverVersion extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getDriverVersion
    }
    final case class  GetExportedKeys(a: String, b: String, c: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getExportedKeys(a, b, c)
    }
    final case object GetExtraNameCharacters extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getExtraNameCharacters
    }
    final case class  GetFunctionColumns(a: String, b: String, c: String, d: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getFunctionColumns(a, b, c, d)
    }
    final case class  GetFunctions(a: String, b: String, c: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getFunctions(a, b, c)
    }
    final case object GetIdentifierQuoteString extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getIdentifierQuoteString
    }
    final case class  GetImportedKeys(a: String, b: String, c: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getImportedKeys(a, b, c)
    }
    final case class  GetIndexInfo(a: String, b: String, c: String, d: Boolean, e: Boolean) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getIndexInfo(a, b, c, d, e)
    }
    final case object GetJDBCMajorVersion extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getJDBCMajorVersion
    }
    final case object GetJDBCMinorVersion extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getJDBCMinorVersion
    }
    final case object GetMaxBinaryLiteralLength extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxBinaryLiteralLength
    }
    final case object GetMaxCatalogNameLength extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxCatalogNameLength
    }
    final case object GetMaxCharLiteralLength extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxCharLiteralLength
    }
    final case object GetMaxColumnNameLength extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxColumnNameLength
    }
    final case object GetMaxColumnsInGroupBy extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxColumnsInGroupBy
    }
    final case object GetMaxColumnsInIndex extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxColumnsInIndex
    }
    final case object GetMaxColumnsInOrderBy extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxColumnsInOrderBy
    }
    final case object GetMaxColumnsInSelect extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxColumnsInSelect
    }
    final case object GetMaxColumnsInTable extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxColumnsInTable
    }
    final case object GetMaxConnections extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxConnections
    }
    final case object GetMaxCursorNameLength extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxCursorNameLength
    }
    final case object GetMaxIndexLength extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxIndexLength
    }
    final case object GetMaxLogicalLobSize extends DatabaseMetaDataOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxLogicalLobSize
    }
    final case object GetMaxProcedureNameLength extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxProcedureNameLength
    }
    final case object GetMaxRowSize extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxRowSize
    }
    final case object GetMaxSchemaNameLength extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxSchemaNameLength
    }
    final case object GetMaxStatementLength extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxStatementLength
    }
    final case object GetMaxStatements extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxStatements
    }
    final case object GetMaxTableNameLength extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxTableNameLength
    }
    final case object GetMaxTablesInSelect extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxTablesInSelect
    }
    final case object GetMaxUserNameLength extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxUserNameLength
    }
    final case object GetNumericFunctions extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getNumericFunctions
    }
    final case class  GetPrimaryKeys(a: String, b: String, c: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getPrimaryKeys(a, b, c)
    }
    final case class  GetProcedureColumns(a: String, b: String, c: String, d: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getProcedureColumns(a, b, c, d)
    }
    final case object GetProcedureTerm extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getProcedureTerm
    }
    final case class  GetProcedures(a: String, b: String, c: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getProcedures(a, b, c)
    }
    final case class  GetPseudoColumns(a: String, b: String, c: String, d: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getPseudoColumns(a, b, c, d)
    }
    final case object GetResultSetHoldability extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getResultSetHoldability
    }
    final case object GetRowIdLifetime extends DatabaseMetaDataOp[RowIdLifetime] {
      def visit[F[_]](v: Visitor[F]) = v.getRowIdLifetime
    }
    final case object GetSQLKeywords extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getSQLKeywords
    }
    final case object GetSQLStateType extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getSQLStateType
    }
    final case object GetSchemaTerm extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getSchemaTerm
    }
    final case object GetSchemas extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getSchemas
    }
    final case class  GetSchemas1(a: String, b: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getSchemas(a, b)
    }
    final case object GetSearchStringEscape extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getSearchStringEscape
    }
    final case object GetStringFunctions extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getStringFunctions
    }
    final case class  GetSuperTables(a: String, b: String, c: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getSuperTables(a, b, c)
    }
    final case class  GetSuperTypes(a: String, b: String, c: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getSuperTypes(a, b, c)
    }
    final case object GetSystemFunctions extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getSystemFunctions
    }
    final case class  GetTablePrivileges(a: String, b: String, c: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getTablePrivileges(a, b, c)
    }
    final case object GetTableTypes extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getTableTypes
    }
    final case class  GetTables(a: String, b: String, c: String, d: Array[String]) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getTables(a, b, c, d)
    }
    final case object GetTimeDateFunctions extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getTimeDateFunctions
    }
    final case object GetTypeInfo extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getTypeInfo
    }
    final case class  GetUDTs(a: String, b: String, c: String, d: Array[Int]) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getUDTs(a, b, c, d)
    }
    final case object GetURL extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getURL
    }
    final case object GetUserName extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getUserName
    }
    final case class  GetVersionColumns(a: String, b: String, c: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getVersionColumns(a, b, c)
    }
    final case class  InsertsAreDetected(a: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.insertsAreDetected(a)
    }
    final case object IsCatalogAtStart extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isCatalogAtStart
    }
    final case object IsReadOnly extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isReadOnly
    }
    final case class  IsWrapperFor(a: Class[_]) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isWrapperFor(a)
    }
    final case object LocatorsUpdateCopy extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.locatorsUpdateCopy
    }
    final case object NullPlusNonNullIsNull extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.nullPlusNonNullIsNull
    }
    final case object NullsAreSortedAtEnd extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.nullsAreSortedAtEnd
    }
    final case object NullsAreSortedAtStart extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.nullsAreSortedAtStart
    }
    final case object NullsAreSortedHigh extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.nullsAreSortedHigh
    }
    final case object NullsAreSortedLow extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.nullsAreSortedLow
    }
    final case class  OthersDeletesAreVisible(a: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.othersDeletesAreVisible(a)
    }
    final case class  OthersInsertsAreVisible(a: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.othersInsertsAreVisible(a)
    }
    final case class  OthersUpdatesAreVisible(a: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.othersUpdatesAreVisible(a)
    }
    final case class  OwnDeletesAreVisible(a: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.ownDeletesAreVisible(a)
    }
    final case class  OwnInsertsAreVisible(a: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.ownInsertsAreVisible(a)
    }
    final case class  OwnUpdatesAreVisible(a: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.ownUpdatesAreVisible(a)
    }
    final case object StoresLowerCaseIdentifiers extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.storesLowerCaseIdentifiers
    }
    final case object StoresLowerCaseQuotedIdentifiers extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.storesLowerCaseQuotedIdentifiers
    }
    final case object StoresMixedCaseIdentifiers extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.storesMixedCaseIdentifiers
    }
    final case object StoresMixedCaseQuotedIdentifiers extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.storesMixedCaseQuotedIdentifiers
    }
    final case object StoresUpperCaseIdentifiers extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.storesUpperCaseIdentifiers
    }
    final case object StoresUpperCaseQuotedIdentifiers extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.storesUpperCaseQuotedIdentifiers
    }
    final case object SupportsANSI92EntryLevelSQL extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsANSI92EntryLevelSQL
    }
    final case object SupportsANSI92FullSQL extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsANSI92FullSQL
    }
    final case object SupportsANSI92IntermediateSQL extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsANSI92IntermediateSQL
    }
    final case object SupportsAlterTableWithAddColumn extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsAlterTableWithAddColumn
    }
    final case object SupportsAlterTableWithDropColumn extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsAlterTableWithDropColumn
    }
    final case object SupportsBatchUpdates extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsBatchUpdates
    }
    final case object SupportsCatalogsInDataManipulation extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsCatalogsInDataManipulation
    }
    final case object SupportsCatalogsInIndexDefinitions extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsCatalogsInIndexDefinitions
    }
    final case object SupportsCatalogsInPrivilegeDefinitions extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsCatalogsInPrivilegeDefinitions
    }
    final case object SupportsCatalogsInProcedureCalls extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsCatalogsInProcedureCalls
    }
    final case object SupportsCatalogsInTableDefinitions extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsCatalogsInTableDefinitions
    }
    final case object SupportsColumnAliasing extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsColumnAliasing
    }
    final case object SupportsConvert extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsConvert
    }
    final case class  SupportsConvert1(a: Int, b: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsConvert(a, b)
    }
    final case object SupportsCoreSQLGrammar extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsCoreSQLGrammar
    }
    final case object SupportsCorrelatedSubqueries extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsCorrelatedSubqueries
    }
    final case object SupportsDataDefinitionAndDataManipulationTransactions extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsDataDefinitionAndDataManipulationTransactions
    }
    final case object SupportsDataManipulationTransactionsOnly extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsDataManipulationTransactionsOnly
    }
    final case object SupportsDifferentTableCorrelationNames extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsDifferentTableCorrelationNames
    }
    final case object SupportsExpressionsInOrderBy extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsExpressionsInOrderBy
    }
    final case object SupportsExtendedSQLGrammar extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsExtendedSQLGrammar
    }
    final case object SupportsFullOuterJoins extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsFullOuterJoins
    }
    final case object SupportsGetGeneratedKeys extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsGetGeneratedKeys
    }
    final case object SupportsGroupBy extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsGroupBy
    }
    final case object SupportsGroupByBeyondSelect extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsGroupByBeyondSelect
    }
    final case object SupportsGroupByUnrelated extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsGroupByUnrelated
    }
    final case object SupportsIntegrityEnhancementFacility extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsIntegrityEnhancementFacility
    }
    final case object SupportsLikeEscapeClause extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsLikeEscapeClause
    }
    final case object SupportsLimitedOuterJoins extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsLimitedOuterJoins
    }
    final case object SupportsMinimumSQLGrammar extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsMinimumSQLGrammar
    }
    final case object SupportsMixedCaseIdentifiers extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsMixedCaseIdentifiers
    }
    final case object SupportsMixedCaseQuotedIdentifiers extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsMixedCaseQuotedIdentifiers
    }
    final case object SupportsMultipleOpenResults extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsMultipleOpenResults
    }
    final case object SupportsMultipleResultSets extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsMultipleResultSets
    }
    final case object SupportsMultipleTransactions extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsMultipleTransactions
    }
    final case object SupportsNamedParameters extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsNamedParameters
    }
    final case object SupportsNonNullableColumns extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsNonNullableColumns
    }
    final case object SupportsOpenCursorsAcrossCommit extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsOpenCursorsAcrossCommit
    }
    final case object SupportsOpenCursorsAcrossRollback extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsOpenCursorsAcrossRollback
    }
    final case object SupportsOpenStatementsAcrossCommit extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsOpenStatementsAcrossCommit
    }
    final case object SupportsOpenStatementsAcrossRollback extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsOpenStatementsAcrossRollback
    }
    final case object SupportsOrderByUnrelated extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsOrderByUnrelated
    }
    final case object SupportsOuterJoins extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsOuterJoins
    }
    final case object SupportsPositionedDelete extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsPositionedDelete
    }
    final case object SupportsPositionedUpdate extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsPositionedUpdate
    }
    final case object SupportsRefCursors extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsRefCursors
    }
    final case class  SupportsResultSetConcurrency(a: Int, b: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsResultSetConcurrency(a, b)
    }
    final case class  SupportsResultSetHoldability(a: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsResultSetHoldability(a)
    }
    final case class  SupportsResultSetType(a: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsResultSetType(a)
    }
    final case object SupportsSavepoints extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsSavepoints
    }
    final case object SupportsSchemasInDataManipulation extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsSchemasInDataManipulation
    }
    final case object SupportsSchemasInIndexDefinitions extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsSchemasInIndexDefinitions
    }
    final case object SupportsSchemasInPrivilegeDefinitions extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsSchemasInPrivilegeDefinitions
    }
    final case object SupportsSchemasInProcedureCalls extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsSchemasInProcedureCalls
    }
    final case object SupportsSchemasInTableDefinitions extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsSchemasInTableDefinitions
    }
    final case object SupportsSelectForUpdate extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsSelectForUpdate
    }
    final case object SupportsSharding extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsSharding
    }
    final case object SupportsStatementPooling extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsStatementPooling
    }
    final case object SupportsStoredFunctionsUsingCallSyntax extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsStoredFunctionsUsingCallSyntax
    }
    final case object SupportsStoredProcedures extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsStoredProcedures
    }
    final case object SupportsSubqueriesInComparisons extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsSubqueriesInComparisons
    }
    final case object SupportsSubqueriesInExists extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsSubqueriesInExists
    }
    final case object SupportsSubqueriesInIns extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsSubqueriesInIns
    }
    final case object SupportsSubqueriesInQuantifieds extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsSubqueriesInQuantifieds
    }
    final case object SupportsTableCorrelationNames extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsTableCorrelationNames
    }
    final case class  SupportsTransactionIsolationLevel(a: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsTransactionIsolationLevel(a)
    }
    final case object SupportsTransactions extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsTransactions
    }
    final case object SupportsUnion extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsUnion
    }
    final case object SupportsUnionAll extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsUnionAll
    }
    final case class  Unwrap[T](a: Class[T]) extends DatabaseMetaDataOp[T] {
      def visit[F[_]](v: Visitor[F]) = v.unwrap(a)
    }
    final case class  UpdatesAreDetected(a: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.updatesAreDetected(a)
    }
    final case object UsesLocalFilePerTable extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.usesLocalFilePerTable
    }
    final case object UsesLocalFiles extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.usesLocalFiles
    }

  }
  import DatabaseMetaDataOp._

  // Smart constructors for operations common to all algebras.
  val unit: DatabaseMetaDataIO[Unit] = FF.pure[DatabaseMetaDataOp, Unit](())
  def pure[A](a: A): DatabaseMetaDataIO[A] = FF.pure[DatabaseMetaDataOp, A](a)
  def raw[A](message: => String)(f: DatabaseMetaData => A): DatabaseMetaDataIO[A] = FF.liftF(Raw(() => message, f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[DatabaseMetaDataOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def delay[A](a: => A): DatabaseMetaDataIO[A] = FF.liftF(Delay(() => a))
  def handleErrorWith[A](fa: DatabaseMetaDataIO[A], f: Throwable => DatabaseMetaDataIO[A]): DatabaseMetaDataIO[A] = FF.liftF[DatabaseMetaDataOp, A](HandleErrorWith(fa, f))
  def raiseError[A](err: Throwable): DatabaseMetaDataIO[A] = FF.liftF[DatabaseMetaDataOp, A](RaiseError(err))
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): DatabaseMetaDataIO[A] = FF.liftF[DatabaseMetaDataOp, A](Async1(k))
  def asyncF[A](k: (Either[Throwable, A] => Unit) => DatabaseMetaDataIO[Unit]): DatabaseMetaDataIO[A] = FF.liftF[DatabaseMetaDataOp, A](AsyncF(k))
  def bracketCase[A, B](acquire: DatabaseMetaDataIO[A])(use: A => DatabaseMetaDataIO[B])(release: (A, ExitCase[Throwable]) => DatabaseMetaDataIO[Unit]): DatabaseMetaDataIO[B] = FF.liftF[DatabaseMetaDataOp, B](BracketCase(acquire, use, release))
  val shift: DatabaseMetaDataIO[Unit] = FF.liftF[DatabaseMetaDataOp, Unit](Shift)
  def evalOn[A](ec: ExecutionContext)(fa: DatabaseMetaDataIO[A]) = FF.liftF[DatabaseMetaDataOp, A](EvalOn(ec, fa))

  // Logger
  def error(message: => String): DatabaseMetaDataIO[Unit] = FF.liftF[DatabaseMetaDataOp, Unit](LogError(() => message))
  def warn(message: => String): DatabaseMetaDataIO[Unit] = FF.liftF[DatabaseMetaDataOp, Unit](LogWarn(() => message))
  def info(message: => String): DatabaseMetaDataIO[Unit] = FF.liftF[DatabaseMetaDataOp, Unit](LogInfo(() => message))
  def debug(message: => String): DatabaseMetaDataIO[Unit] = FF.liftF[DatabaseMetaDataOp, Unit](LogDebug(() => message))
  def trace(message: => String): DatabaseMetaDataIO[Unit] = FF.liftF[DatabaseMetaDataOp, Unit](LogTrace(() => message))

  // Smart constructors for DatabaseMetaData-specific operations.
  val allProceduresAreCallable: DatabaseMetaDataIO[Boolean] = FF.liftF(AllProceduresAreCallable)
  val allTablesAreSelectable: DatabaseMetaDataIO[Boolean] = FF.liftF(AllTablesAreSelectable)
  val autoCommitFailureClosesAllResultSets: DatabaseMetaDataIO[Boolean] = FF.liftF(AutoCommitFailureClosesAllResultSets)
  val dataDefinitionCausesTransactionCommit: DatabaseMetaDataIO[Boolean] = FF.liftF(DataDefinitionCausesTransactionCommit)
  val dataDefinitionIgnoredInTransactions: DatabaseMetaDataIO[Boolean] = FF.liftF(DataDefinitionIgnoredInTransactions)
  def deletesAreDetected(a: Int): DatabaseMetaDataIO[Boolean] = FF.liftF(DeletesAreDetected(a))
  val doesMaxRowSizeIncludeBlobs: DatabaseMetaDataIO[Boolean] = FF.liftF(DoesMaxRowSizeIncludeBlobs)
  val generatedKeyAlwaysReturned: DatabaseMetaDataIO[Boolean] = FF.liftF(GeneratedKeyAlwaysReturned)
  def getAttributes(a: String, b: String, c: String, d: String): DatabaseMetaDataIO[ResultSet] = FF.liftF(GetAttributes(a, b, c, d))
  def getBestRowIdentifier(a: String, b: String, c: String, d: Int, e: Boolean): DatabaseMetaDataIO[ResultSet] = FF.liftF(GetBestRowIdentifier(a, b, c, d, e))
  val getCatalogSeparator: DatabaseMetaDataIO[String] = FF.liftF(GetCatalogSeparator)
  val getCatalogTerm: DatabaseMetaDataIO[String] = FF.liftF(GetCatalogTerm)
  val getCatalogs: DatabaseMetaDataIO[ResultSet] = FF.liftF(GetCatalogs)
  val getClientInfoProperties: DatabaseMetaDataIO[ResultSet] = FF.liftF(GetClientInfoProperties)
  def getColumnPrivileges(a: String, b: String, c: String, d: String): DatabaseMetaDataIO[ResultSet] = FF.liftF(GetColumnPrivileges(a, b, c, d))
  def getColumns(a: String, b: String, c: String, d: String): DatabaseMetaDataIO[ResultSet] = FF.liftF(GetColumns(a, b, c, d))
  val getConnection: DatabaseMetaDataIO[Connection] = FF.liftF(GetConnection)
  def getCrossReference(a: String, b: String, c: String, d: String, e: String, f: String): DatabaseMetaDataIO[ResultSet] = FF.liftF(GetCrossReference(a, b, c, d, e, f))
  val getDatabaseMajorVersion: DatabaseMetaDataIO[Int] = FF.liftF(GetDatabaseMajorVersion)
  val getDatabaseMinorVersion: DatabaseMetaDataIO[Int] = FF.liftF(GetDatabaseMinorVersion)
  val getDatabaseProductName: DatabaseMetaDataIO[String] = FF.liftF(GetDatabaseProductName)
  val getDatabaseProductVersion: DatabaseMetaDataIO[String] = FF.liftF(GetDatabaseProductVersion)
  val getDefaultTransactionIsolation: DatabaseMetaDataIO[Int] = FF.liftF(GetDefaultTransactionIsolation)
  val getDriverMajorVersion: DatabaseMetaDataIO[Int] = FF.liftF(GetDriverMajorVersion)
  val getDriverMinorVersion: DatabaseMetaDataIO[Int] = FF.liftF(GetDriverMinorVersion)
  val getDriverName: DatabaseMetaDataIO[String] = FF.liftF(GetDriverName)
  val getDriverVersion: DatabaseMetaDataIO[String] = FF.liftF(GetDriverVersion)
  def getExportedKeys(a: String, b: String, c: String): DatabaseMetaDataIO[ResultSet] = FF.liftF(GetExportedKeys(a, b, c))
  val getExtraNameCharacters: DatabaseMetaDataIO[String] = FF.liftF(GetExtraNameCharacters)
  def getFunctionColumns(a: String, b: String, c: String, d: String): DatabaseMetaDataIO[ResultSet] = FF.liftF(GetFunctionColumns(a, b, c, d))
  def getFunctions(a: String, b: String, c: String): DatabaseMetaDataIO[ResultSet] = FF.liftF(GetFunctions(a, b, c))
  val getIdentifierQuoteString: DatabaseMetaDataIO[String] = FF.liftF(GetIdentifierQuoteString)
  def getImportedKeys(a: String, b: String, c: String): DatabaseMetaDataIO[ResultSet] = FF.liftF(GetImportedKeys(a, b, c))
  def getIndexInfo(a: String, b: String, c: String, d: Boolean, e: Boolean): DatabaseMetaDataIO[ResultSet] = FF.liftF(GetIndexInfo(a, b, c, d, e))
  val getJDBCMajorVersion: DatabaseMetaDataIO[Int] = FF.liftF(GetJDBCMajorVersion)
  val getJDBCMinorVersion: DatabaseMetaDataIO[Int] = FF.liftF(GetJDBCMinorVersion)
  val getMaxBinaryLiteralLength: DatabaseMetaDataIO[Int] = FF.liftF(GetMaxBinaryLiteralLength)
  val getMaxCatalogNameLength: DatabaseMetaDataIO[Int] = FF.liftF(GetMaxCatalogNameLength)
  val getMaxCharLiteralLength: DatabaseMetaDataIO[Int] = FF.liftF(GetMaxCharLiteralLength)
  val getMaxColumnNameLength: DatabaseMetaDataIO[Int] = FF.liftF(GetMaxColumnNameLength)
  val getMaxColumnsInGroupBy: DatabaseMetaDataIO[Int] = FF.liftF(GetMaxColumnsInGroupBy)
  val getMaxColumnsInIndex: DatabaseMetaDataIO[Int] = FF.liftF(GetMaxColumnsInIndex)
  val getMaxColumnsInOrderBy: DatabaseMetaDataIO[Int] = FF.liftF(GetMaxColumnsInOrderBy)
  val getMaxColumnsInSelect: DatabaseMetaDataIO[Int] = FF.liftF(GetMaxColumnsInSelect)
  val getMaxColumnsInTable: DatabaseMetaDataIO[Int] = FF.liftF(GetMaxColumnsInTable)
  val getMaxConnections: DatabaseMetaDataIO[Int] = FF.liftF(GetMaxConnections)
  val getMaxCursorNameLength: DatabaseMetaDataIO[Int] = FF.liftF(GetMaxCursorNameLength)
  val getMaxIndexLength: DatabaseMetaDataIO[Int] = FF.liftF(GetMaxIndexLength)
  val getMaxLogicalLobSize: DatabaseMetaDataIO[Long] = FF.liftF(GetMaxLogicalLobSize)
  val getMaxProcedureNameLength: DatabaseMetaDataIO[Int] = FF.liftF(GetMaxProcedureNameLength)
  val getMaxRowSize: DatabaseMetaDataIO[Int] = FF.liftF(GetMaxRowSize)
  val getMaxSchemaNameLength: DatabaseMetaDataIO[Int] = FF.liftF(GetMaxSchemaNameLength)
  val getMaxStatementLength: DatabaseMetaDataIO[Int] = FF.liftF(GetMaxStatementLength)
  val getMaxStatements: DatabaseMetaDataIO[Int] = FF.liftF(GetMaxStatements)
  val getMaxTableNameLength: DatabaseMetaDataIO[Int] = FF.liftF(GetMaxTableNameLength)
  val getMaxTablesInSelect: DatabaseMetaDataIO[Int] = FF.liftF(GetMaxTablesInSelect)
  val getMaxUserNameLength: DatabaseMetaDataIO[Int] = FF.liftF(GetMaxUserNameLength)
  val getNumericFunctions: DatabaseMetaDataIO[String] = FF.liftF(GetNumericFunctions)
  def getPrimaryKeys(a: String, b: String, c: String): DatabaseMetaDataIO[ResultSet] = FF.liftF(GetPrimaryKeys(a, b, c))
  def getProcedureColumns(a: String, b: String, c: String, d: String): DatabaseMetaDataIO[ResultSet] = FF.liftF(GetProcedureColumns(a, b, c, d))
  val getProcedureTerm: DatabaseMetaDataIO[String] = FF.liftF(GetProcedureTerm)
  def getProcedures(a: String, b: String, c: String): DatabaseMetaDataIO[ResultSet] = FF.liftF(GetProcedures(a, b, c))
  def getPseudoColumns(a: String, b: String, c: String, d: String): DatabaseMetaDataIO[ResultSet] = FF.liftF(GetPseudoColumns(a, b, c, d))
  val getResultSetHoldability: DatabaseMetaDataIO[Int] = FF.liftF(GetResultSetHoldability)
  val getRowIdLifetime: DatabaseMetaDataIO[RowIdLifetime] = FF.liftF(GetRowIdLifetime)
  val getSQLKeywords: DatabaseMetaDataIO[String] = FF.liftF(GetSQLKeywords)
  val getSQLStateType: DatabaseMetaDataIO[Int] = FF.liftF(GetSQLStateType)
  val getSchemaTerm: DatabaseMetaDataIO[String] = FF.liftF(GetSchemaTerm)
  val getSchemas: DatabaseMetaDataIO[ResultSet] = FF.liftF(GetSchemas)
  def getSchemas(a: String, b: String): DatabaseMetaDataIO[ResultSet] = FF.liftF(GetSchemas1(a, b))
  val getSearchStringEscape: DatabaseMetaDataIO[String] = FF.liftF(GetSearchStringEscape)
  val getStringFunctions: DatabaseMetaDataIO[String] = FF.liftF(GetStringFunctions)
  def getSuperTables(a: String, b: String, c: String): DatabaseMetaDataIO[ResultSet] = FF.liftF(GetSuperTables(a, b, c))
  def getSuperTypes(a: String, b: String, c: String): DatabaseMetaDataIO[ResultSet] = FF.liftF(GetSuperTypes(a, b, c))
  val getSystemFunctions: DatabaseMetaDataIO[String] = FF.liftF(GetSystemFunctions)
  def getTablePrivileges(a: String, b: String, c: String): DatabaseMetaDataIO[ResultSet] = FF.liftF(GetTablePrivileges(a, b, c))
  val getTableTypes: DatabaseMetaDataIO[ResultSet] = FF.liftF(GetTableTypes)
  def getTables(a: String, b: String, c: String, d: Array[String]): DatabaseMetaDataIO[ResultSet] = FF.liftF(GetTables(a, b, c, d))
  val getTimeDateFunctions: DatabaseMetaDataIO[String] = FF.liftF(GetTimeDateFunctions)
  val getTypeInfo: DatabaseMetaDataIO[ResultSet] = FF.liftF(GetTypeInfo)
  def getUDTs(a: String, b: String, c: String, d: Array[Int]): DatabaseMetaDataIO[ResultSet] = FF.liftF(GetUDTs(a, b, c, d))
  val getURL: DatabaseMetaDataIO[String] = FF.liftF(GetURL)
  val getUserName: DatabaseMetaDataIO[String] = FF.liftF(GetUserName)
  def getVersionColumns(a: String, b: String, c: String): DatabaseMetaDataIO[ResultSet] = FF.liftF(GetVersionColumns(a, b, c))
  def insertsAreDetected(a: Int): DatabaseMetaDataIO[Boolean] = FF.liftF(InsertsAreDetected(a))
  val isCatalogAtStart: DatabaseMetaDataIO[Boolean] = FF.liftF(IsCatalogAtStart)
  val isReadOnly: DatabaseMetaDataIO[Boolean] = FF.liftF(IsReadOnly)
  def isWrapperFor(a: Class[_]): DatabaseMetaDataIO[Boolean] = FF.liftF(IsWrapperFor(a))
  val locatorsUpdateCopy: DatabaseMetaDataIO[Boolean] = FF.liftF(LocatorsUpdateCopy)
  val nullPlusNonNullIsNull: DatabaseMetaDataIO[Boolean] = FF.liftF(NullPlusNonNullIsNull)
  val nullsAreSortedAtEnd: DatabaseMetaDataIO[Boolean] = FF.liftF(NullsAreSortedAtEnd)
  val nullsAreSortedAtStart: DatabaseMetaDataIO[Boolean] = FF.liftF(NullsAreSortedAtStart)
  val nullsAreSortedHigh: DatabaseMetaDataIO[Boolean] = FF.liftF(NullsAreSortedHigh)
  val nullsAreSortedLow: DatabaseMetaDataIO[Boolean] = FF.liftF(NullsAreSortedLow)
  def othersDeletesAreVisible(a: Int): DatabaseMetaDataIO[Boolean] = FF.liftF(OthersDeletesAreVisible(a))
  def othersInsertsAreVisible(a: Int): DatabaseMetaDataIO[Boolean] = FF.liftF(OthersInsertsAreVisible(a))
  def othersUpdatesAreVisible(a: Int): DatabaseMetaDataIO[Boolean] = FF.liftF(OthersUpdatesAreVisible(a))
  def ownDeletesAreVisible(a: Int): DatabaseMetaDataIO[Boolean] = FF.liftF(OwnDeletesAreVisible(a))
  def ownInsertsAreVisible(a: Int): DatabaseMetaDataIO[Boolean] = FF.liftF(OwnInsertsAreVisible(a))
  def ownUpdatesAreVisible(a: Int): DatabaseMetaDataIO[Boolean] = FF.liftF(OwnUpdatesAreVisible(a))
  val storesLowerCaseIdentifiers: DatabaseMetaDataIO[Boolean] = FF.liftF(StoresLowerCaseIdentifiers)
  val storesLowerCaseQuotedIdentifiers: DatabaseMetaDataIO[Boolean] = FF.liftF(StoresLowerCaseQuotedIdentifiers)
  val storesMixedCaseIdentifiers: DatabaseMetaDataIO[Boolean] = FF.liftF(StoresMixedCaseIdentifiers)
  val storesMixedCaseQuotedIdentifiers: DatabaseMetaDataIO[Boolean] = FF.liftF(StoresMixedCaseQuotedIdentifiers)
  val storesUpperCaseIdentifiers: DatabaseMetaDataIO[Boolean] = FF.liftF(StoresUpperCaseIdentifiers)
  val storesUpperCaseQuotedIdentifiers: DatabaseMetaDataIO[Boolean] = FF.liftF(StoresUpperCaseQuotedIdentifiers)
  val supportsANSI92EntryLevelSQL: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsANSI92EntryLevelSQL)
  val supportsANSI92FullSQL: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsANSI92FullSQL)
  val supportsANSI92IntermediateSQL: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsANSI92IntermediateSQL)
  val supportsAlterTableWithAddColumn: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsAlterTableWithAddColumn)
  val supportsAlterTableWithDropColumn: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsAlterTableWithDropColumn)
  val supportsBatchUpdates: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsBatchUpdates)
  val supportsCatalogsInDataManipulation: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsCatalogsInDataManipulation)
  val supportsCatalogsInIndexDefinitions: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsCatalogsInIndexDefinitions)
  val supportsCatalogsInPrivilegeDefinitions: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsCatalogsInPrivilegeDefinitions)
  val supportsCatalogsInProcedureCalls: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsCatalogsInProcedureCalls)
  val supportsCatalogsInTableDefinitions: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsCatalogsInTableDefinitions)
  val supportsColumnAliasing: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsColumnAliasing)
  val supportsConvert: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsConvert)
  def supportsConvert(a: Int, b: Int): DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsConvert1(a, b))
  val supportsCoreSQLGrammar: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsCoreSQLGrammar)
  val supportsCorrelatedSubqueries: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsCorrelatedSubqueries)
  val supportsDataDefinitionAndDataManipulationTransactions: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsDataDefinitionAndDataManipulationTransactions)
  val supportsDataManipulationTransactionsOnly: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsDataManipulationTransactionsOnly)
  val supportsDifferentTableCorrelationNames: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsDifferentTableCorrelationNames)
  val supportsExpressionsInOrderBy: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsExpressionsInOrderBy)
  val supportsExtendedSQLGrammar: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsExtendedSQLGrammar)
  val supportsFullOuterJoins: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsFullOuterJoins)
  val supportsGetGeneratedKeys: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsGetGeneratedKeys)
  val supportsGroupBy: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsGroupBy)
  val supportsGroupByBeyondSelect: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsGroupByBeyondSelect)
  val supportsGroupByUnrelated: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsGroupByUnrelated)
  val supportsIntegrityEnhancementFacility: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsIntegrityEnhancementFacility)
  val supportsLikeEscapeClause: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsLikeEscapeClause)
  val supportsLimitedOuterJoins: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsLimitedOuterJoins)
  val supportsMinimumSQLGrammar: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsMinimumSQLGrammar)
  val supportsMixedCaseIdentifiers: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsMixedCaseIdentifiers)
  val supportsMixedCaseQuotedIdentifiers: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsMixedCaseQuotedIdentifiers)
  val supportsMultipleOpenResults: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsMultipleOpenResults)
  val supportsMultipleResultSets: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsMultipleResultSets)
  val supportsMultipleTransactions: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsMultipleTransactions)
  val supportsNamedParameters: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsNamedParameters)
  val supportsNonNullableColumns: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsNonNullableColumns)
  val supportsOpenCursorsAcrossCommit: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsOpenCursorsAcrossCommit)
  val supportsOpenCursorsAcrossRollback: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsOpenCursorsAcrossRollback)
  val supportsOpenStatementsAcrossCommit: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsOpenStatementsAcrossCommit)
  val supportsOpenStatementsAcrossRollback: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsOpenStatementsAcrossRollback)
  val supportsOrderByUnrelated: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsOrderByUnrelated)
  val supportsOuterJoins: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsOuterJoins)
  val supportsPositionedDelete: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsPositionedDelete)
  val supportsPositionedUpdate: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsPositionedUpdate)
  val supportsRefCursors: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsRefCursors)
  def supportsResultSetConcurrency(a: Int, b: Int): DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsResultSetConcurrency(a, b))
  def supportsResultSetHoldability(a: Int): DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsResultSetHoldability(a))
  def supportsResultSetType(a: Int): DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsResultSetType(a))
  val supportsSavepoints: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsSavepoints)
  val supportsSchemasInDataManipulation: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsSchemasInDataManipulation)
  val supportsSchemasInIndexDefinitions: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsSchemasInIndexDefinitions)
  val supportsSchemasInPrivilegeDefinitions: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsSchemasInPrivilegeDefinitions)
  val supportsSchemasInProcedureCalls: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsSchemasInProcedureCalls)
  val supportsSchemasInTableDefinitions: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsSchemasInTableDefinitions)
  val supportsSelectForUpdate: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsSelectForUpdate)
  val supportsSharding: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsSharding)
  val supportsStatementPooling: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsStatementPooling)
  val supportsStoredFunctionsUsingCallSyntax: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsStoredFunctionsUsingCallSyntax)
  val supportsStoredProcedures: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsStoredProcedures)
  val supportsSubqueriesInComparisons: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsSubqueriesInComparisons)
  val supportsSubqueriesInExists: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsSubqueriesInExists)
  val supportsSubqueriesInIns: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsSubqueriesInIns)
  val supportsSubqueriesInQuantifieds: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsSubqueriesInQuantifieds)
  val supportsTableCorrelationNames: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsTableCorrelationNames)
  def supportsTransactionIsolationLevel(a: Int): DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsTransactionIsolationLevel(a))
  val supportsTransactions: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsTransactions)
  val supportsUnion: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsUnion)
  val supportsUnionAll: DatabaseMetaDataIO[Boolean] = FF.liftF(SupportsUnionAll)
  def unwrap[T](a: Class[T]): DatabaseMetaDataIO[T] = FF.liftF(Unwrap(a))
  def updatesAreDetected(a: Int): DatabaseMetaDataIO[Boolean] = FF.liftF(UpdatesAreDetected(a))
  val usesLocalFilePerTable: DatabaseMetaDataIO[Boolean] = FF.liftF(UsesLocalFilePerTable)
  val usesLocalFiles: DatabaseMetaDataIO[Boolean] = FF.liftF(UsesLocalFiles)

  // DatabaseMetaDataIO is an Async
  implicit val AsyncDatabaseMetaDataIO: Async[DatabaseMetaDataIO] =
    new Async[DatabaseMetaDataIO] {
      val asyncM = FF.catsFreeMonadForFree[DatabaseMetaDataOp]
      def bracketCase[A, B](acquire: DatabaseMetaDataIO[A])(use: A => DatabaseMetaDataIO[B])(release: (A, ExitCase[Throwable]) => DatabaseMetaDataIO[Unit]): DatabaseMetaDataIO[B] = module.bracketCase(acquire)(use)(release)
      def pure[A](x: A): DatabaseMetaDataIO[A] = asyncM.pure(x)
      def handleErrorWith[A](fa: DatabaseMetaDataIO[A])(f: Throwable => DatabaseMetaDataIO[A]): DatabaseMetaDataIO[A] = module.handleErrorWith(fa, f)
      def raiseError[A](e: Throwable): DatabaseMetaDataIO[A] = module.raiseError(e)
      def async[A](k: (Either[Throwable,A] => Unit) => Unit): DatabaseMetaDataIO[A] = module.async(k)
      def asyncF[A](k: (Either[Throwable,A] => Unit) => DatabaseMetaDataIO[Unit]): DatabaseMetaDataIO[A] = module.asyncF(k)
      def flatMap[A, B](fa: DatabaseMetaDataIO[A])(f: A => DatabaseMetaDataIO[B]): DatabaseMetaDataIO[B] = asyncM.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => DatabaseMetaDataIO[Either[A, B]]): DatabaseMetaDataIO[B] = asyncM.tailRecM(a)(f)
      def suspend[A](thunk: => DatabaseMetaDataIO[A]): DatabaseMetaDataIO[A] = asyncM.flatten(module.delay(thunk))
    }

  // DatabaseMetaDataIO is a ContextShift
  implicit val ContextShiftDatabaseMetaDataIO: ContextShift[DatabaseMetaDataIO] =
    new ContextShift[DatabaseMetaDataIO] {
      def shift: DatabaseMetaDataIO[Unit] = module.shift
      def evalOn[A](ec: ExecutionContext)(fa: DatabaseMetaDataIO[A]) = module.evalOn(ec)(fa)
    }

  // DatabaseMetaDataIO is a MessageLogger
  implicit val MessageLoggerDatabaseMetaDataIO: MessageLogger[DatabaseMetaDataIO] =
    new MessageLogger[DatabaseMetaDataIO] {
      def error(message: => String): DatabaseMetaDataIO[Unit] =  module.error(message)
      def warn(message: => String): DatabaseMetaDataIO[Unit] = module.warn(message)
      def info(message: => String): DatabaseMetaDataIO[Unit] = module.info(message)
      def debug(message: => String): DatabaseMetaDataIO[Unit] =  module.debug(message)
      def trace(message: => String): DatabaseMetaDataIO[Unit] =  module.trace(message)
    }
}

