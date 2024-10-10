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

import java.lang.Class
import java.lang.String
import java.sql.Connection
import java.sql.DatabaseMetaData
import java.sql.ResultSet
import java.sql.RowIdLifetime

// This file is Auto-generated using FreeGen2.scala
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
        def embed[A](j: DatabaseMetaData, fa: FF[DatabaseMetaDataOp, A]): Embedded.DatabaseMetaData[A] = Embedded.DatabaseMetaData(j, fa)
      }

    // Interface for a natural transformation DatabaseMetaDataOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (DatabaseMetaDataOp ~> F) {
      final def apply[A](fa: DatabaseMetaDataOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](f: DatabaseMetaData => A): F[A]
      def embed[A](e: Embedded[A]): F[A]
      def raiseError[A](e: Throwable): F[A]
      def handleErrorWith[A](fa: DatabaseMetaDataIO[A])(f: Throwable => DatabaseMetaDataIO[A]): F[A]
      def monotonic: F[FiniteDuration]
      def realTime: F[FiniteDuration]
      def delay[A](thunk: => A): F[A]
      def suspend[A](hint: Sync.Type)(thunk: => A): F[A]
      def forceR[A, B](fa: DatabaseMetaDataIO[A])(fb: DatabaseMetaDataIO[B]): F[B]
      def uncancelable[A](body: Poll[DatabaseMetaDataIO] => DatabaseMetaDataIO[A]): F[A]
      def poll[A](poll: Any, fa: DatabaseMetaDataIO[A]): F[A]
      def canceled: F[Unit]
      def onCancel[A](fa: DatabaseMetaDataIO[A], fin: DatabaseMetaDataIO[Unit]): F[A]
      def fromFuture[A](fut: DatabaseMetaDataIO[Future[A]]): F[A]
      def fromFutureCancelable[A](fut: DatabaseMetaDataIO[(Future[A], DatabaseMetaDataIO[Unit])]): F[A]
      def cancelable[A](fa: DatabaseMetaDataIO[A], fin: DatabaseMetaDataIO[Unit]): F[A]
      def performLogging(event: LogEvent): F[Unit]

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
      def isWrapperFor(a: Class[?]): F[Boolean]
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
    final case class Raw[A](f: DatabaseMetaData => A) extends DatabaseMetaDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(f)
    }
    final case class Embed[A](e: Embedded[A]) extends DatabaseMetaDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class RaiseError[A](e: Throwable) extends DatabaseMetaDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raiseError(e)
    }
    final case class HandleErrorWith[A](fa: DatabaseMetaDataIO[A], f: Throwable => DatabaseMetaDataIO[A]) extends DatabaseMetaDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa)(f)
    }
    case object Monotonic extends DatabaseMetaDataOp[FiniteDuration] {
      def visit[F[_]](v: Visitor[F]) = v.monotonic
    }
    case object Realtime extends DatabaseMetaDataOp[FiniteDuration] {
      def visit[F[_]](v: Visitor[F]) = v.realTime
    }
    case class Suspend[A](hint: Sync.Type, thunk: () => A) extends DatabaseMetaDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.suspend(hint)(thunk())
    }
    case class ForceR[A, B](fa: DatabaseMetaDataIO[A], fb: DatabaseMetaDataIO[B]) extends DatabaseMetaDataOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.forceR(fa)(fb)
    }
    case class Uncancelable[A](body: Poll[DatabaseMetaDataIO] => DatabaseMetaDataIO[A]) extends DatabaseMetaDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.uncancelable(body)
    }
    case class Poll1[A](poll: Any, fa: DatabaseMetaDataIO[A]) extends DatabaseMetaDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.poll(poll, fa)
    }
    case object Canceled extends DatabaseMetaDataOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.canceled
    }
    case class OnCancel[A](fa: DatabaseMetaDataIO[A], fin: DatabaseMetaDataIO[Unit]) extends DatabaseMetaDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.onCancel(fa, fin)
    }
    case class FromFuture[A](fut: DatabaseMetaDataIO[Future[A]]) extends DatabaseMetaDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.fromFuture(fut)
    }
    case class FromFutureCancelable[A](fut: DatabaseMetaDataIO[(Future[A], DatabaseMetaDataIO[Unit])]) extends DatabaseMetaDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.fromFutureCancelable(fut)
    }
    case class Cancelable[A](fa: DatabaseMetaDataIO[A], fin: DatabaseMetaDataIO[Unit]) extends DatabaseMetaDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.cancelable(fa, fin)
    }
    case class PerformLogging(event: LogEvent) extends DatabaseMetaDataOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.performLogging(event)
    }

    // DatabaseMetaData-specific operations.
    case object AllProceduresAreCallable extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.allProceduresAreCallable
    }
    case object AllTablesAreSelectable extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.allTablesAreSelectable
    }
    case object AutoCommitFailureClosesAllResultSets extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.autoCommitFailureClosesAllResultSets
    }
    case object DataDefinitionCausesTransactionCommit extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.dataDefinitionCausesTransactionCommit
    }
    case object DataDefinitionIgnoredInTransactions extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.dataDefinitionIgnoredInTransactions
    }
    final case class DeletesAreDetected(a: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.deletesAreDetected(a)
    }
    case object DoesMaxRowSizeIncludeBlobs extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.doesMaxRowSizeIncludeBlobs
    }
    case object GeneratedKeyAlwaysReturned extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.generatedKeyAlwaysReturned
    }
    final case class GetAttributes(a: String, b: String, c: String, d: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getAttributes(a, b, c, d)
    }
    final case class GetBestRowIdentifier(a: String, b: String, c: String, d: Int, e: Boolean) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getBestRowIdentifier(a, b, c, d, e)
    }
    case object GetCatalogSeparator extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getCatalogSeparator
    }
    case object GetCatalogTerm extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getCatalogTerm
    }
    case object GetCatalogs extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getCatalogs
    }
    case object GetClientInfoProperties extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getClientInfoProperties
    }
    final case class GetColumnPrivileges(a: String, b: String, c: String, d: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getColumnPrivileges(a, b, c, d)
    }
    final case class GetColumns(a: String, b: String, c: String, d: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getColumns(a, b, c, d)
    }
    case object GetConnection extends DatabaseMetaDataOp[Connection] {
      def visit[F[_]](v: Visitor[F]) = v.getConnection
    }
    final case class GetCrossReference(a: String, b: String, c: String, d: String, e: String, f: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getCrossReference(a, b, c, d, e, f)
    }
    case object GetDatabaseMajorVersion extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getDatabaseMajorVersion
    }
    case object GetDatabaseMinorVersion extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getDatabaseMinorVersion
    }
    case object GetDatabaseProductName extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getDatabaseProductName
    }
    case object GetDatabaseProductVersion extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getDatabaseProductVersion
    }
    case object GetDefaultTransactionIsolation extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getDefaultTransactionIsolation
    }
    case object GetDriverMajorVersion extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getDriverMajorVersion
    }
    case object GetDriverMinorVersion extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getDriverMinorVersion
    }
    case object GetDriverName extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getDriverName
    }
    case object GetDriverVersion extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getDriverVersion
    }
    final case class GetExportedKeys(a: String, b: String, c: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getExportedKeys(a, b, c)
    }
    case object GetExtraNameCharacters extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getExtraNameCharacters
    }
    final case class GetFunctionColumns(a: String, b: String, c: String, d: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getFunctionColumns(a, b, c, d)
    }
    final case class GetFunctions(a: String, b: String, c: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getFunctions(a, b, c)
    }
    case object GetIdentifierQuoteString extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getIdentifierQuoteString
    }
    final case class GetImportedKeys(a: String, b: String, c: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getImportedKeys(a, b, c)
    }
    final case class GetIndexInfo(a: String, b: String, c: String, d: Boolean, e: Boolean) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getIndexInfo(a, b, c, d, e)
    }
    case object GetJDBCMajorVersion extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getJDBCMajorVersion
    }
    case object GetJDBCMinorVersion extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getJDBCMinorVersion
    }
    case object GetMaxBinaryLiteralLength extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxBinaryLiteralLength
    }
    case object GetMaxCatalogNameLength extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxCatalogNameLength
    }
    case object GetMaxCharLiteralLength extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxCharLiteralLength
    }
    case object GetMaxColumnNameLength extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxColumnNameLength
    }
    case object GetMaxColumnsInGroupBy extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxColumnsInGroupBy
    }
    case object GetMaxColumnsInIndex extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxColumnsInIndex
    }
    case object GetMaxColumnsInOrderBy extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxColumnsInOrderBy
    }
    case object GetMaxColumnsInSelect extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxColumnsInSelect
    }
    case object GetMaxColumnsInTable extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxColumnsInTable
    }
    case object GetMaxConnections extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxConnections
    }
    case object GetMaxCursorNameLength extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxCursorNameLength
    }
    case object GetMaxIndexLength extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxIndexLength
    }
    case object GetMaxLogicalLobSize extends DatabaseMetaDataOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxLogicalLobSize
    }
    case object GetMaxProcedureNameLength extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxProcedureNameLength
    }
    case object GetMaxRowSize extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxRowSize
    }
    case object GetMaxSchemaNameLength extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxSchemaNameLength
    }
    case object GetMaxStatementLength extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxStatementLength
    }
    case object GetMaxStatements extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxStatements
    }
    case object GetMaxTableNameLength extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxTableNameLength
    }
    case object GetMaxTablesInSelect extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxTablesInSelect
    }
    case object GetMaxUserNameLength extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getMaxUserNameLength
    }
    case object GetNumericFunctions extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getNumericFunctions
    }
    final case class GetPrimaryKeys(a: String, b: String, c: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getPrimaryKeys(a, b, c)
    }
    final case class GetProcedureColumns(a: String, b: String, c: String, d: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getProcedureColumns(a, b, c, d)
    }
    case object GetProcedureTerm extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getProcedureTerm
    }
    final case class GetProcedures(a: String, b: String, c: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getProcedures(a, b, c)
    }
    final case class GetPseudoColumns(a: String, b: String, c: String, d: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getPseudoColumns(a, b, c, d)
    }
    case object GetResultSetHoldability extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getResultSetHoldability
    }
    case object GetRowIdLifetime extends DatabaseMetaDataOp[RowIdLifetime] {
      def visit[F[_]](v: Visitor[F]) = v.getRowIdLifetime
    }
    case object GetSQLKeywords extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getSQLKeywords
    }
    case object GetSQLStateType extends DatabaseMetaDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getSQLStateType
    }
    case object GetSchemaTerm extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getSchemaTerm
    }
    case object GetSchemas extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getSchemas
    }
    final case class GetSchemas1(a: String, b: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getSchemas(a, b)
    }
    case object GetSearchStringEscape extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getSearchStringEscape
    }
    case object GetStringFunctions extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getStringFunctions
    }
    final case class GetSuperTables(a: String, b: String, c: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getSuperTables(a, b, c)
    }
    final case class GetSuperTypes(a: String, b: String, c: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getSuperTypes(a, b, c)
    }
    case object GetSystemFunctions extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getSystemFunctions
    }
    final case class GetTablePrivileges(a: String, b: String, c: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getTablePrivileges(a, b, c)
    }
    case object GetTableTypes extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getTableTypes
    }
    final case class GetTables(a: String, b: String, c: String, d: Array[String]) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getTables(a, b, c, d)
    }
    case object GetTimeDateFunctions extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getTimeDateFunctions
    }
    case object GetTypeInfo extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getTypeInfo
    }
    final case class GetUDTs(a: String, b: String, c: String, d: Array[Int]) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getUDTs(a, b, c, d)
    }
    case object GetURL extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getURL
    }
    case object GetUserName extends DatabaseMetaDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getUserName
    }
    final case class GetVersionColumns(a: String, b: String, c: String) extends DatabaseMetaDataOp[ResultSet] {
      def visit[F[_]](v: Visitor[F]) = v.getVersionColumns(a, b, c)
    }
    final case class InsertsAreDetected(a: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.insertsAreDetected(a)
    }
    case object IsCatalogAtStart extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isCatalogAtStart
    }
    case object IsReadOnly extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isReadOnly
    }
    final case class IsWrapperFor(a: Class[?]) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isWrapperFor(a)
    }
    case object LocatorsUpdateCopy extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.locatorsUpdateCopy
    }
    case object NullPlusNonNullIsNull extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.nullPlusNonNullIsNull
    }
    case object NullsAreSortedAtEnd extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.nullsAreSortedAtEnd
    }
    case object NullsAreSortedAtStart extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.nullsAreSortedAtStart
    }
    case object NullsAreSortedHigh extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.nullsAreSortedHigh
    }
    case object NullsAreSortedLow extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.nullsAreSortedLow
    }
    final case class OthersDeletesAreVisible(a: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.othersDeletesAreVisible(a)
    }
    final case class OthersInsertsAreVisible(a: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.othersInsertsAreVisible(a)
    }
    final case class OthersUpdatesAreVisible(a: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.othersUpdatesAreVisible(a)
    }
    final case class OwnDeletesAreVisible(a: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.ownDeletesAreVisible(a)
    }
    final case class OwnInsertsAreVisible(a: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.ownInsertsAreVisible(a)
    }
    final case class OwnUpdatesAreVisible(a: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.ownUpdatesAreVisible(a)
    }
    case object StoresLowerCaseIdentifiers extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.storesLowerCaseIdentifiers
    }
    case object StoresLowerCaseQuotedIdentifiers extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.storesLowerCaseQuotedIdentifiers
    }
    case object StoresMixedCaseIdentifiers extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.storesMixedCaseIdentifiers
    }
    case object StoresMixedCaseQuotedIdentifiers extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.storesMixedCaseQuotedIdentifiers
    }
    case object StoresUpperCaseIdentifiers extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.storesUpperCaseIdentifiers
    }
    case object StoresUpperCaseQuotedIdentifiers extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.storesUpperCaseQuotedIdentifiers
    }
    case object SupportsANSI92EntryLevelSQL extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsANSI92EntryLevelSQL
    }
    case object SupportsANSI92FullSQL extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsANSI92FullSQL
    }
    case object SupportsANSI92IntermediateSQL extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsANSI92IntermediateSQL
    }
    case object SupportsAlterTableWithAddColumn extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsAlterTableWithAddColumn
    }
    case object SupportsAlterTableWithDropColumn extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsAlterTableWithDropColumn
    }
    case object SupportsBatchUpdates extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsBatchUpdates
    }
    case object SupportsCatalogsInDataManipulation extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsCatalogsInDataManipulation
    }
    case object SupportsCatalogsInIndexDefinitions extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsCatalogsInIndexDefinitions
    }
    case object SupportsCatalogsInPrivilegeDefinitions extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsCatalogsInPrivilegeDefinitions
    }
    case object SupportsCatalogsInProcedureCalls extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsCatalogsInProcedureCalls
    }
    case object SupportsCatalogsInTableDefinitions extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsCatalogsInTableDefinitions
    }
    case object SupportsColumnAliasing extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsColumnAliasing
    }
    case object SupportsConvert extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsConvert
    }
    final case class SupportsConvert1(a: Int, b: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsConvert(a, b)
    }
    case object SupportsCoreSQLGrammar extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsCoreSQLGrammar
    }
    case object SupportsCorrelatedSubqueries extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsCorrelatedSubqueries
    }
    case object SupportsDataDefinitionAndDataManipulationTransactions extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsDataDefinitionAndDataManipulationTransactions
    }
    case object SupportsDataManipulationTransactionsOnly extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsDataManipulationTransactionsOnly
    }
    case object SupportsDifferentTableCorrelationNames extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsDifferentTableCorrelationNames
    }
    case object SupportsExpressionsInOrderBy extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsExpressionsInOrderBy
    }
    case object SupportsExtendedSQLGrammar extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsExtendedSQLGrammar
    }
    case object SupportsFullOuterJoins extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsFullOuterJoins
    }
    case object SupportsGetGeneratedKeys extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsGetGeneratedKeys
    }
    case object SupportsGroupBy extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsGroupBy
    }
    case object SupportsGroupByBeyondSelect extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsGroupByBeyondSelect
    }
    case object SupportsGroupByUnrelated extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsGroupByUnrelated
    }
    case object SupportsIntegrityEnhancementFacility extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsIntegrityEnhancementFacility
    }
    case object SupportsLikeEscapeClause extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsLikeEscapeClause
    }
    case object SupportsLimitedOuterJoins extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsLimitedOuterJoins
    }
    case object SupportsMinimumSQLGrammar extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsMinimumSQLGrammar
    }
    case object SupportsMixedCaseIdentifiers extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsMixedCaseIdentifiers
    }
    case object SupportsMixedCaseQuotedIdentifiers extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsMixedCaseQuotedIdentifiers
    }
    case object SupportsMultipleOpenResults extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsMultipleOpenResults
    }
    case object SupportsMultipleResultSets extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsMultipleResultSets
    }
    case object SupportsMultipleTransactions extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsMultipleTransactions
    }
    case object SupportsNamedParameters extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsNamedParameters
    }
    case object SupportsNonNullableColumns extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsNonNullableColumns
    }
    case object SupportsOpenCursorsAcrossCommit extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsOpenCursorsAcrossCommit
    }
    case object SupportsOpenCursorsAcrossRollback extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsOpenCursorsAcrossRollback
    }
    case object SupportsOpenStatementsAcrossCommit extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsOpenStatementsAcrossCommit
    }
    case object SupportsOpenStatementsAcrossRollback extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsOpenStatementsAcrossRollback
    }
    case object SupportsOrderByUnrelated extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsOrderByUnrelated
    }
    case object SupportsOuterJoins extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsOuterJoins
    }
    case object SupportsPositionedDelete extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsPositionedDelete
    }
    case object SupportsPositionedUpdate extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsPositionedUpdate
    }
    case object SupportsRefCursors extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsRefCursors
    }
    final case class SupportsResultSetConcurrency(a: Int, b: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsResultSetConcurrency(a, b)
    }
    final case class SupportsResultSetHoldability(a: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsResultSetHoldability(a)
    }
    final case class SupportsResultSetType(a: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsResultSetType(a)
    }
    case object SupportsSavepoints extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsSavepoints
    }
    case object SupportsSchemasInDataManipulation extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsSchemasInDataManipulation
    }
    case object SupportsSchemasInIndexDefinitions extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsSchemasInIndexDefinitions
    }
    case object SupportsSchemasInPrivilegeDefinitions extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsSchemasInPrivilegeDefinitions
    }
    case object SupportsSchemasInProcedureCalls extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsSchemasInProcedureCalls
    }
    case object SupportsSchemasInTableDefinitions extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsSchemasInTableDefinitions
    }
    case object SupportsSelectForUpdate extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsSelectForUpdate
    }
    case object SupportsSharding extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsSharding
    }
    case object SupportsStatementPooling extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsStatementPooling
    }
    case object SupportsStoredFunctionsUsingCallSyntax extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsStoredFunctionsUsingCallSyntax
    }
    case object SupportsStoredProcedures extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsStoredProcedures
    }
    case object SupportsSubqueriesInComparisons extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsSubqueriesInComparisons
    }
    case object SupportsSubqueriesInExists extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsSubqueriesInExists
    }
    case object SupportsSubqueriesInIns extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsSubqueriesInIns
    }
    case object SupportsSubqueriesInQuantifieds extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsSubqueriesInQuantifieds
    }
    case object SupportsTableCorrelationNames extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsTableCorrelationNames
    }
    final case class SupportsTransactionIsolationLevel(a: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsTransactionIsolationLevel(a)
    }
    case object SupportsTransactions extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsTransactions
    }
    case object SupportsUnion extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsUnion
    }
    case object SupportsUnionAll extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.supportsUnionAll
    }
    final case class Unwrap[T](a: Class[T]) extends DatabaseMetaDataOp[T] {
      def visit[F[_]](v: Visitor[F]) = v.unwrap(a)
    }
    final case class UpdatesAreDetected(a: Int) extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.updatesAreDetected(a)
    }
    case object UsesLocalFilePerTable extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.usesLocalFilePerTable
    }
    case object UsesLocalFiles extends DatabaseMetaDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.usesLocalFiles
    }

  }
  import DatabaseMetaDataOp.*

  // Smart constructors for operations common to all algebras.
  val unit: DatabaseMetaDataIO[Unit] = FF.pure[DatabaseMetaDataOp, Unit](())
  def pure[A](a: A): DatabaseMetaDataIO[A] = FF.pure[DatabaseMetaDataOp, A](a)
  def raw[A](f: DatabaseMetaData => A): DatabaseMetaDataIO[A] = FF.liftF(Raw(f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[DatabaseMetaDataOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def raiseError[A](err: Throwable): DatabaseMetaDataIO[A] = FF.liftF[DatabaseMetaDataOp, A](RaiseError(err))
  def handleErrorWith[A](fa: DatabaseMetaDataIO[A])(f: Throwable => DatabaseMetaDataIO[A]): DatabaseMetaDataIO[A] = FF.liftF[DatabaseMetaDataOp, A](HandleErrorWith(fa, f))
  val monotonic = FF.liftF[DatabaseMetaDataOp, FiniteDuration](Monotonic)
  val realtime = FF.liftF[DatabaseMetaDataOp, FiniteDuration](Realtime)
  def delay[A](thunk: => A) = FF.liftF[DatabaseMetaDataOp, A](Suspend(Sync.Type.Delay, () => thunk))
  def suspend[A](hint: Sync.Type)(thunk: => A) = FF.liftF[DatabaseMetaDataOp, A](Suspend(hint, () => thunk))
  def forceR[A, B](fa: DatabaseMetaDataIO[A])(fb: DatabaseMetaDataIO[B]) = FF.liftF[DatabaseMetaDataOp, B](ForceR(fa, fb))
  def uncancelable[A](body: Poll[DatabaseMetaDataIO] => DatabaseMetaDataIO[A]) = FF.liftF[DatabaseMetaDataOp, A](Uncancelable(body))
  def capturePoll[M[_]](mpoll: Poll[M]) = new Poll[DatabaseMetaDataIO] {
    def apply[A](fa: DatabaseMetaDataIO[A]) = FF.liftF[DatabaseMetaDataOp, A](Poll1(mpoll, fa))
  }
  val canceled = FF.liftF[DatabaseMetaDataOp, Unit](Canceled)
  def onCancel[A](fa: DatabaseMetaDataIO[A], fin: DatabaseMetaDataIO[Unit]) = FF.liftF[DatabaseMetaDataOp, A](OnCancel(fa, fin))
  def fromFuture[A](fut: DatabaseMetaDataIO[Future[A]]) = FF.liftF[DatabaseMetaDataOp, A](FromFuture(fut))
  def fromFutureCancelable[A](fut: DatabaseMetaDataIO[(Future[A], DatabaseMetaDataIO[Unit])]) = FF.liftF[DatabaseMetaDataOp, A](FromFutureCancelable(fut))
  def cancelable[A](fa: DatabaseMetaDataIO[A], fin: DatabaseMetaDataIO[Unit]) = FF.liftF[DatabaseMetaDataOp, A](Cancelable(fa, fin))
  def performLogging(event: LogEvent) = FF.liftF[DatabaseMetaDataOp, Unit](PerformLogging(event))

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
  def isWrapperFor(a: Class[?]): DatabaseMetaDataIO[Boolean] = FF.liftF(IsWrapperFor(a))
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

  // Typeclass instances for DatabaseMetaDataIO
  implicit val WeakAsyncDatabaseMetaDataIO: WeakAsync[DatabaseMetaDataIO] =
    new WeakAsync[DatabaseMetaDataIO] {
      val monad = FF.catsFreeMonadForFree[DatabaseMetaDataOp]
      override val applicative: Applicative[DatabaseMetaDataIO] = monad
      override val rootCancelScope: CancelScope = CancelScope.Cancelable
      override def pure[A](x: A): DatabaseMetaDataIO[A] = monad.pure(x)
      override def flatMap[A, B](fa: DatabaseMetaDataIO[A])(f: A => DatabaseMetaDataIO[B]): DatabaseMetaDataIO[B] = monad.flatMap(fa)(f)
      override def tailRecM[A, B](a: A)(f: A => DatabaseMetaDataIO[Either[A, B]]): DatabaseMetaDataIO[B] = monad.tailRecM(a)(f)
      override def raiseError[A](e: Throwable): DatabaseMetaDataIO[A] = module.raiseError(e)
      override def handleErrorWith[A](fa: DatabaseMetaDataIO[A])(f: Throwable => DatabaseMetaDataIO[A]): DatabaseMetaDataIO[A] = module.handleErrorWith(fa)(f)
      override def monotonic: DatabaseMetaDataIO[FiniteDuration] = module.monotonic
      override def realTime: DatabaseMetaDataIO[FiniteDuration] = module.realtime
      override def suspend[A](hint: Sync.Type)(thunk: => A): DatabaseMetaDataIO[A] = module.suspend(hint)(thunk)
      override def forceR[A, B](fa: DatabaseMetaDataIO[A])(fb: DatabaseMetaDataIO[B]): DatabaseMetaDataIO[B] = module.forceR(fa)(fb)
      override def uncancelable[A](body: Poll[DatabaseMetaDataIO] => DatabaseMetaDataIO[A]): DatabaseMetaDataIO[A] = module.uncancelable(body)
      override def canceled: DatabaseMetaDataIO[Unit] = module.canceled
      override def onCancel[A](fa: DatabaseMetaDataIO[A], fin: DatabaseMetaDataIO[Unit]): DatabaseMetaDataIO[A] = module.onCancel(fa, fin)
      override def fromFuture[A](fut: DatabaseMetaDataIO[Future[A]]): DatabaseMetaDataIO[A] = module.fromFuture(fut)
      override def fromFutureCancelable[A](fut: DatabaseMetaDataIO[(Future[A], DatabaseMetaDataIO[Unit])]): DatabaseMetaDataIO[A] = module.fromFutureCancelable(fut)
      override def cancelable[A](fa: DatabaseMetaDataIO[A], fin: DatabaseMetaDataIO[Unit]): DatabaseMetaDataIO[A] = module.cancelable(fa, fin)
    }
    
  implicit def MonoidDatabaseMetaDataIO[A : Monoid]: Monoid[DatabaseMetaDataIO[A]] = new Monoid[DatabaseMetaDataIO[A]] {
    override def empty: DatabaseMetaDataIO[A] = Applicative[DatabaseMetaDataIO].pure(Monoid[A].empty)
    override def combine(x: DatabaseMetaDataIO[A], y: DatabaseMetaDataIO[A]): DatabaseMetaDataIO[A] =
      Applicative[DatabaseMetaDataIO].product(x, y).map { case (x, y) => Monoid[A].combine(x, y) }
  }
 
  implicit def SemigroupDatabaseMetaDataIO[A : Semigroup]: Semigroup[DatabaseMetaDataIO[A]] = new Semigroup[DatabaseMetaDataIO[A]] {
    override def combine(x: DatabaseMetaDataIO[A], y: DatabaseMetaDataIO[A]): DatabaseMetaDataIO[A] =
      Applicative[DatabaseMetaDataIO].product(x, y).map { case (x, y) => Semigroup[A].combine(x, y) }
  }
}

