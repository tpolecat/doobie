// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres.syntax

import cats._
import cats.syntax.all._
import doobie.implicits._
import doobie.postgres.sqlstate._
import doobie._
import doobie.util.catchsql.exceptSomeSqlState
import doobie.util.query.{Query, Query0}
import doobie.hi.{HPS, HRS, HC}
import doobie.free.ConnectionIO

class PostgresMonadErrorOps[M[_], A](ma: M[A])(
    implicit ev: MonadError[M, Throwable]
) {

  def onSuccessfulCompletion(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class00.SUCCESSFUL_COMPLETION => handler }

  def onWarning(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class01.WARNING => handler }

  def onDynamicResultSetsReturned(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class01.DYNAMIC_RESULT_SETS_RETURNED => handler }

  def onImplicitZeroBitPadding(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class01.IMPLICIT_ZERO_BIT_PADDING => handler }

  def onNullValueEliminatedInSetFunction(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class01.NULL_VALUE_ELIMINATED_IN_SET_FUNCTION => handler }

  def onPrivilegeNotGranted(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class01.PRIVILEGE_NOT_GRANTED => handler }

  def onPrivilegeNotRevoked(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class01.PRIVILEGE_NOT_REVOKED => handler }

  def onStringDataRightTruncationClass01(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class01.STRING_DATA_RIGHT_TRUNCATION => handler }

  def onDeprecatedFeature(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class01.DEPRECATED_FEATURE => handler }

  def onNoData(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class02.NO_DATA => handler }

  def onNoAdditionalDynamicResultSetsReturned(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class02.NO_ADDITIONAL_DYNAMIC_RESULT_SETS_RETURNED => handler }

  def onSqlStatementNotYetComplete(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class03.SQL_STATEMENT_NOT_YET_COMPLETE => handler }

  def onConnectionException(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class08.CONNECTION_EXCEPTION => handler }

  def onConnectionDoesNotExist(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class08.CONNECTION_DOES_NOT_EXIST => handler }

  def onConnectionFailure(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class08.CONNECTION_FAILURE => handler }

  def onSqlclientUnableToEstablishSqlconnection(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class08.SQLCLIENT_UNABLE_TO_ESTABLISH_SQLCONNECTION => handler }

  def onSqlserverRejectedEstablishmentOfSqlconnection(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class08.SQLSERVER_REJECTED_ESTABLISHMENT_OF_SQLCONNECTION => handler }

  def onTransactionResolutionUnknown(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class08.TRANSACTION_RESOLUTION_UNKNOWN => handler }

  def onProtocolViolation(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class08.PROTOCOL_VIOLATION => handler }

  def onTriggeredActionException(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class09.TRIGGERED_ACTION_EXCEPTION => handler }

  def onFeatureNotSupported(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class0A.FEATURE_NOT_SUPPORTED => handler }

  def onInvalidTransactionInitiation(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class0B.INVALID_TRANSACTION_INITIATION => handler }

  def onLocatorException(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class0F.LOCATOR_EXCEPTION => handler }

  def onInvalidLocatorSpecification(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class0F.INVALID_LOCATOR_SPECIFICATION => handler }

  def onInvalidGrantor(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class0L.INVALID_GRANTOR => handler }

  def onInvalidGrantOperation(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class0L.INVALID_GRANT_OPERATION => handler }

  def onInvalidRoleSpecification(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class0P.INVALID_ROLE_SPECIFICATION => handler }

  def onCaseNotFound(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class20.CASE_NOT_FOUND => handler }

  def onCardinalityViolation(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class21.CARDINALITY_VIOLATION => handler }

  def onDataException(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.DATA_EXCEPTION => handler }

  def onArraySubscriptError(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.ARRAY_SUBSCRIPT_ERROR => handler }

  def onCharacterNotInRepertoire(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.CHARACTER_NOT_IN_REPERTOIRE => handler }

  def onDatetimeFieldOverflow(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.DATETIME_FIELD_OVERFLOW => handler }

  def onDivisionByZero(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.DIVISION_BY_ZERO => handler }

  def onErrorInAssignment(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.ERROR_IN_ASSIGNMENT => handler }

  def onEscapeCharacterConflict(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.ESCAPE_CHARACTER_CONFLICT => handler }

  def onIndicatorOverflow(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.INDICATOR_OVERFLOW => handler }

  def onIntervalFieldOverflow(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.INTERVAL_FIELD_OVERFLOW => handler }

  def onInvalidArgumentForLogarithm(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.INVALID_ARGUMENT_FOR_LOGARITHM => handler }

  def onInvalidArgumentForNtileFunction(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.INVALID_ARGUMENT_FOR_NTILE_FUNCTION => handler }

  def onInvalidArgumentForNthValueFunction(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.INVALID_ARGUMENT_FOR_NTH_VALUE_FUNCTION => handler }

  def onInvalidArgumentForPowerFunction(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.INVALID_ARGUMENT_FOR_POWER_FUNCTION => handler }

  def onInvalidArgumentForWidthBucketFunction(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.INVALID_ARGUMENT_FOR_WIDTH_BUCKET_FUNCTION => handler }

  def onInvalidCharacterValueForCast(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.INVALID_CHARACTER_VALUE_FOR_CAST => handler }

  def onInvalidDatetimeFormat(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.INVALID_DATETIME_FORMAT => handler }

  def onInvalidEscapeCharacter(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.INVALID_ESCAPE_CHARACTER => handler }

  def onInvalidEscapeOctet(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.INVALID_ESCAPE_OCTET => handler }

  def onInvalidEscapeSequence(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.INVALID_ESCAPE_SEQUENCE => handler }

  def onNonstandardUseOfEscapeCharacter(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.NONSTANDARD_USE_OF_ESCAPE_CHARACTER => handler }

  def onInvalidIndicatorParameterValue(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.INVALID_INDICATOR_PARAMETER_VALUE => handler }

  def onInvalidParameterValue(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.INVALID_PARAMETER_VALUE => handler }

  def onInvalidRegularExpression(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.INVALID_REGULAR_EXPRESSION => handler }

  def onInvalidRowCountInLimitClause(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.INVALID_ROW_COUNT_IN_LIMIT_CLAUSE => handler }

  def onInvalidRowCountInResultOffsetClause(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE => handler }

  def onInvalidTimeZoneDisplacementValue(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.INVALID_TIME_ZONE_DISPLACEMENT_VALUE => handler }

  def onInvalidUseOfEscapeCharacter(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.INVALID_USE_OF_ESCAPE_CHARACTER => handler }

  def onMostSpecificTypeMismatch(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.MOST_SPECIFIC_TYPE_MISMATCH => handler }

  def onNullValueNotAllowedClass22(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.NULL_VALUE_NOT_ALLOWED => handler }

  def onNullValueNoIndicatorParameter(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.NULL_VALUE_NO_INDICATOR_PARAMETER => handler }

  def onNumericValueOutOfRange(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.NUMERIC_VALUE_OUT_OF_RANGE => handler }

  def onStringDataLengthMismatch(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.STRING_DATA_LENGTH_MISMATCH => handler }

  def onStringDataRightTruncationClass22(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.STRING_DATA_RIGHT_TRUNCATION => handler }

  def onSubstringError(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.SUBSTRING_ERROR => handler }

  def onTrimError(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.TRIM_ERROR => handler }

  def onUnterminatedCString(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.UNTERMINATED_C_STRING => handler }

  def onZeroLengthCharacterString(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.ZERO_LENGTH_CHARACTER_STRING => handler }

  def onFloatingPointException(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.FLOATING_POINT_EXCEPTION => handler }

  def onInvalidTextRepresentation(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.INVALID_TEXT_REPRESENTATION => handler }

  def onInvalidBinaryRepresentation(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.INVALID_BINARY_REPRESENTATION => handler }

  def onBadCopyFileFormat(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.BAD_COPY_FILE_FORMAT => handler }

  def onUntranslatableCharacter(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.UNTRANSLATABLE_CHARACTER => handler }

  def onNotAnXmlDocument(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.NOT_AN_XML_DOCUMENT => handler }

  def onInvalidXmlDocument(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.INVALID_XML_DOCUMENT => handler }

  def onInvalidXmlContent(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.INVALID_XML_CONTENT => handler }

  def onInvalidXmlComment(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.INVALID_XML_COMMENT => handler }

  def onInvalidXmlProcessingInstruction(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class22.INVALID_XML_PROCESSING_INSTRUCTION => handler }

  def onIntegrityConstraintViolation(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class23.INTEGRITY_CONSTRAINT_VIOLATION => handler }

  def onRestrictViolation(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class23.RESTRICT_VIOLATION => handler }

  def onNotNullViolation(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class23.NOT_NULL_VIOLATION => handler }

  def onForeignKeyViolation(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class23.FOREIGN_KEY_VIOLATION => handler }

  def onUniqueViolation(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class23.UNIQUE_VIOLATION => handler }

  def onCheckViolation(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class23.CHECK_VIOLATION => handler }

  def onExclusionViolation(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class23.EXCLUSION_VIOLATION => handler }

  def onInvalidCursorState(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class24.INVALID_CURSOR_STATE => handler }

  def onInvalidTransactionState(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class25.INVALID_TRANSACTION_STATE => handler }

  def onActiveSqlTransaction(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class25.ACTIVE_SQL_TRANSACTION => handler }

  def onBranchTransactionAlreadyActive(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class25.BRANCH_TRANSACTION_ALREADY_ACTIVE => handler }

  def onHeldCursorRequiresSameIsolationLevel(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class25.HELD_CURSOR_REQUIRES_SAME_ISOLATION_LEVEL => handler }

  def onInappropriateAccessModeForBranchTransaction(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class25.INAPPROPRIATE_ACCESS_MODE_FOR_BRANCH_TRANSACTION => handler }

  def onInappropriateIsolationLevelForBranchTransaction(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class25.INAPPROPRIATE_ISOLATION_LEVEL_FOR_BRANCH_TRANSACTION => handler }

  def onNoActiveSqlTransactionForBranchTransaction(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class25.NO_ACTIVE_SQL_TRANSACTION_FOR_BRANCH_TRANSACTION => handler }

  def onReadOnlySqlTransaction(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class25.READ_ONLY_SQL_TRANSACTION => handler }

  def onSchemaAndDataStatementMixingNotSupported(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class25.SCHEMA_AND_DATA_STATEMENT_MIXING_NOT_SUPPORTED => handler }

  def onNoActiveSqlTransaction(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class25.NO_ACTIVE_SQL_TRANSACTION => handler }

  def onInFailedSqlTransaction(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class25.IN_FAILED_SQL_TRANSACTION => handler }

  def onInvalidSqlStatementName(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class26.INVALID_SQL_STATEMENT_NAME => handler }

  def onTriggeredDataChangeViolation(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class27.TRIGGERED_DATA_CHANGE_VIOLATION => handler }

  def onInvalidAuthorizationSpecification(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class28.INVALID_AUTHORIZATION_SPECIFICATION => handler }

  def onInvalidPassword(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class28.INVALID_PASSWORD => handler }

  def onDependentPrivilegeDescriptorsStillExist(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class2B.DEPENDENT_PRIVILEGE_DESCRIPTORS_STILL_EXIST => handler }

  def onDependentObjectsStillExist(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class2B.DEPENDENT_OBJECTS_STILL_EXIST => handler }

  def onInvalidTransactionTermination(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class2D.INVALID_TRANSACTION_TERMINATION => handler }

  def onSqlRoutineException(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class2F.SQL_ROUTINE_EXCEPTION => handler }

  def onFunctionExecutedNoReturnStatement(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class2F.FUNCTION_EXECUTED_NO_RETURN_STATEMENT => handler }

  def onModifyingSqlDataNotPermittedClass2F(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class2F.MODIFYING_SQL_DATA_NOT_PERMITTED => handler }

  def onProhibitedSqlStatementAttemptedClass2F(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class2F.PROHIBITED_SQL_STATEMENT_ATTEMPTED => handler }

  def onReadingSqlDataNotPermittedClass2F(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class2F.READING_SQL_DATA_NOT_PERMITTED => handler }

  def onInvalidCursorName(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class34.INVALID_CURSOR_NAME => handler }

  def onExternalRoutineException(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class38.EXTERNAL_ROUTINE_EXCEPTION => handler }

  def onContainingSqlNotPermitted(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class38.CONTAINING_SQL_NOT_PERMITTED => handler }

  def onModifyingSqlDataNotPermittedClass38(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class38.MODIFYING_SQL_DATA_NOT_PERMITTED => handler }

  def onProhibitedSqlStatementAttemptedClass38(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class38.PROHIBITED_SQL_STATEMENT_ATTEMPTED => handler }

  def onReadingSqlDataNotPermittedClass38(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class38.READING_SQL_DATA_NOT_PERMITTED => handler }

  def onExternalRoutineInvocationException(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class39.EXTERNAL_ROUTINE_INVOCATION_EXCEPTION => handler }

  def onInvalidSqlstateReturned(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class39.INVALID_SQLSTATE_RETURNED => handler }

  def onNullValueNotAllowedClass39(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class39.NULL_VALUE_NOT_ALLOWED => handler }

  def onTriggerProtocolViolated(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class39.TRIGGER_PROTOCOL_VIOLATED => handler }

  def onSrfProtocolViolated(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class39.SRF_PROTOCOL_VIOLATED => handler }

  def onSavepointException(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class3B.SAVEPOINT_EXCEPTION => handler }

  def onInvalidSavepointSpecification(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class3B.INVALID_SAVEPOINT_SPECIFICATION => handler }

  def onInvalidCatalogName(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class3D.INVALID_CATALOG_NAME => handler }

  def onInvalidSchemaName(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class3F.INVALID_SCHEMA_NAME => handler }

  def onTransactionRollback(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class40.TRANSACTION_ROLLBACK => handler }

  def onTransactionIntegrityConstraintViolation(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class40.TRANSACTION_INTEGRITY_CONSTRAINT_VIOLATION => handler }

  def onSerializationFailure(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class40.SERIALIZATION_FAILURE => handler }

  def onStatementCompletionUnknown(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class40.STATEMENT_COMPLETION_UNKNOWN => handler }

  def onDeadlockDetected(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class40.DEADLOCK_DETECTED => handler }

  def onSyntaxErrorOrAccessRuleViolation(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.SYNTAX_ERROR_OR_ACCESS_RULE_VIOLATION => handler }

  def onSyntaxError(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.SYNTAX_ERROR => handler }

  def onInsufficientPrivilege(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.INSUFFICIENT_PRIVILEGE => handler }

  def onCannotCoerce(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.CANNOT_COERCE => handler }

  def onGroupingError(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.GROUPING_ERROR => handler }

  def onWindowingError(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.WINDOWING_ERROR => handler }

  def onInvalidRecursion(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.INVALID_RECURSION => handler }

  def onInvalidForeignKey(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.INVALID_FOREIGN_KEY => handler }

  def onInvalidName(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.INVALID_NAME => handler }

  def onNameTooLong(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.NAME_TOO_LONG => handler }

  def onReservedName(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.RESERVED_NAME => handler }

  def onDatatypeMismatch(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.DATATYPE_MISMATCH => handler }

  def onIndeterminateDatatype(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.INDETERMINATE_DATATYPE => handler }

  def onWrongObjectType(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.WRONG_OBJECT_TYPE => handler }

  def onUndefinedColumn(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.UNDEFINED_COLUMN => handler }

  def onUndefinedFunction(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.UNDEFINED_FUNCTION => handler }

  def onUndefinedTable(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.UNDEFINED_TABLE => handler }

  def onUndefinedParameter(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.UNDEFINED_PARAMETER => handler }

  def onUndefinedObject(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.UNDEFINED_OBJECT => handler }

  def onDuplicateColumn(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.DUPLICATE_COLUMN => handler }

  def onDuplicateCursor(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.DUPLICATE_CURSOR => handler }

  def onDuplicateDatabase(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.DUPLICATE_DATABASE => handler }

  def onDuplicateFunction(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.DUPLICATE_FUNCTION => handler }

  def onDuplicatePreparedStatement(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.DUPLICATE_PREPARED_STATEMENT => handler }

  def onDuplicateSchema(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.DUPLICATE_SCHEMA => handler }

  def onDuplicateTable(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.DUPLICATE_TABLE => handler }

  def onDuplicateAlias(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.DUPLICATE_ALIAS => handler }

  def onDuplicateObject(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.DUPLICATE_OBJECT => handler }

  def onAmbiguousColumn(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.AMBIGUOUS_COLUMN => handler }

  def onAmbiguousFunction(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.AMBIGUOUS_FUNCTION => handler }

  def onAmbiguousParameter(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.AMBIGUOUS_PARAMETER => handler }

  def onAmbiguousAlias(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.AMBIGUOUS_ALIAS => handler }

  def onInvalidColumnReference(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.INVALID_COLUMN_REFERENCE => handler }

  def onInvalidColumnDefinition(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.INVALID_COLUMN_DEFINITION => handler }

  def onInvalidCursorDefinition(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.INVALID_CURSOR_DEFINITION => handler }

  def onInvalidDatabaseDefinition(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.INVALID_DATABASE_DEFINITION => handler }

  def onInvalidFunctionDefinition(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.INVALID_FUNCTION_DEFINITION => handler }

  def onInvalidPreparedStatementDefinition(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.INVALID_PREPARED_STATEMENT_DEFINITION => handler }

  def onInvalidSchemaDefinition(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.INVALID_SCHEMA_DEFINITION => handler }

  def onInvalidTableDefinition(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.INVALID_TABLE_DEFINITION => handler }

  def onInvalidObjectDefinition(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class42.INVALID_OBJECT_DEFINITION => handler }

  def onWithCheckOptionViolation(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class44.WITH_CHECK_OPTION_VIOLATION => handler }

  def onInsufficientResources(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class53.INSUFFICIENT_RESOURCES => handler }

  def onDiskFull(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class53.DISK_FULL => handler }

  def onOutOfMemory(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class53.OUT_OF_MEMORY => handler }

  def onTooManyConnections(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class53.TOO_MANY_CONNECTIONS => handler }

  def onProgramLimitExceeded(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class54.PROGRAM_LIMIT_EXCEEDED => handler }

  def onStatementTooComplex(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class54.STATEMENT_TOO_COMPLEX => handler }

  def onTooManyColumns(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class54.TOO_MANY_COLUMNS => handler }

  def onTooManyArguments(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class54.TOO_MANY_ARGUMENTS => handler }

  def onObjectNotInPrerequisiteState(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class55.OBJECT_NOT_IN_PREREQUISITE_STATE => handler }

  def onObjectInUse(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class55.OBJECT_IN_USE => handler }

  def onCantChangeRuntimeParam(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class55.CANT_CHANGE_RUNTIME_PARAM => handler }

  def onLockNotAvailable(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class55.LOCK_NOT_AVAILABLE => handler }

  def onOperatorIntervention(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class57.OPERATOR_INTERVENTION => handler }

  def onQueryCanceled(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class57.QUERY_CANCELED => handler }

  def onAdminShutdown(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class57.ADMIN_SHUTDOWN => handler }

  def onCrashShutdown(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class57.CRASH_SHUTDOWN => handler }

  def onCannotConnectNow(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class57.CANNOT_CONNECT_NOW => handler }

  def onDatabaseDropped(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class57.DATABASE_DROPPED => handler }

  def onIoError(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class58.IO_ERROR => handler }

  def onUndefinedFile(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class58.UNDEFINED_FILE => handler }

  def onDuplicateFile(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case class58.DUPLICATE_FILE => handler }

  def onConfigFileError(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case classF0.CONFIG_FILE_ERROR => handler }

  def onLockFileExists(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case classF0.LOCK_FILE_EXISTS => handler }

  def onPlpgsqlError(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case classP0.PLPGSQL_ERROR => handler }

  def onRaiseException(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case classP0.RAISE_EXCEPTION => handler }

  def onNoDataFound(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case classP0.NO_DATA_FOUND => handler }

  def onTooManyRows(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case classP0.TOO_MANY_ROWS => handler }

  def onInternalError(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case classXX.INTERNAL_ERROR => handler }

  def onDataCorrupted(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case classXX.DATA_CORRUPTED => handler }

  def onIndexCorrupted(handler: => M[A]): M[A] =
    exceptSomeSqlState(ma) { case classXX.INDEX_CORRUPTED => handler }

}

trait ToPostgresMonadErrorOps {
  implicit def toPostgresMonadErrorOps[M[_], A](ma: M[A])(
      implicit ev: MonadError[M, Throwable]
  ): PostgresMonadErrorOps[M, A] =
    new PostgresMonadErrorOps(ma)
}

trait ToPostgresExplainOps {
  implicit def toPostgresExplainQuery0Ops(q: Query0[_]): PostgresExplainQuery0Ops =
    new PostgresExplainQuery0Ops(q)

  implicit def toPostgresExplainQueryOps[A](q: Query[A, _]): PostgresExplainQueryOps[A] =
    new PostgresExplainQueryOps(q)

  implicit def toPostgresExplainUpdate0Ops(u: Update0): PostgresExplainUpdate0Ops =
    new PostgresExplainUpdate0Ops(u)

  implicit def toPostgresExplainUpdateOps[A](u: Update[A]): PostgresExplainUpdateOps[A] =
    new PostgresExplainUpdateOps(u)
}

class PostgresExplainQuery0Ops(self: Query0[_]) {

  /** Construct a program in `[[doobie.free.connection.ConnectionIO ConnectionIO]]` which returns the server's query
    * plan for the query (i.e., `EXPLAIN` output). The query is not actually executed.
    */
  def explain: ConnectionIO[List[String]] =
    self.inspect { (sql, prepare) =>
      HC.prepareStatement(s"EXPLAIN $sql")(prepare *> HPS.executeQuery(HRS.build[List, String]))
    }

  /** Construct a program in `[[doobie.free.connection.ConnectionIO ConnectionIO]]` which returns the server's query
    * plan for the query, with a comparison to the actual execution (i.e., `EXPLAIN ANALYZE` output). The query will be
    * executed, but no results are returned.
    */
  def explainAnalyze: ConnectionIO[List[String]] =
    self.inspect { (sql, prepare) =>
      HC.prepareStatement(s"EXPLAIN ANALYZE $sql")(prepare *> HPS.executeQuery(HRS.build[List, String]))
    }
}

class PostgresExplainQueryOps[A](self: Query[A, _]) {

  /** Apply the argument `a` to construct a program in `[[doobie.free.connection.ConnectionIO ConnectionIO]]` which
    * returns the server's query plan for the query (i.e., `EXPLAIN` output). The query is not actually executed.
    */
  def explain(a: A): ConnectionIO[List[String]] = {
    self.inspect(a) { (sql, prepare) =>
      HC.prepareStatement(s"EXPLAIN $sql")(prepare *> HPS.executeQuery(HRS.build[List, String]))
    }
  }

  /** Apply the argument `a` to construct a program in `[[doobie.free.connection.ConnectionIO ConnectionIO]]` which
    * returns the server's query plan for the query, with a comparison to the actual execution (i.e., `EXPLAIN ANALYZE`
    * output). The query will be executed, but no results are returned.
    */
  def explainAnalyze(a: A): ConnectionIO[List[String]] =
    self.inspect(a) { (sql, prepare) =>
      HC.prepareStatement(s"EXPLAIN ANALYZE $sql")(prepare *> HPS.executeQuery(HRS.build[List, String]))
    }
}

class PostgresExplainUpdate0Ops(self: Update0) {

  /** Construct a program in `[[doobie.free.connection.ConnectionIO ConnectionIO]]` which returns the server's query
    * plan for the query (i.e., `EXPLAIN` output). The query is not actually executed.
    */
  def explain: ConnectionIO[List[String]] =
    self.inspect { (sql, prepare) =>
      HC.prepareStatement(s"EXPLAIN $sql")(prepare *> HPS.executeQuery(HRS.build[List, String]))
    }

  /** Construct a program in `[[doobie.free.connection.ConnectionIO ConnectionIO]]` which returns the server's query
    * plan for the query, with a comparison to the actual execution (i.e., `EXPLAIN ANALYZE` output). The query will be
    * executed, but no results are returned.
    */
  def explainAnalyze: ConnectionIO[List[String]] =
    self.inspect { (sql, prepare) =>
      HC.prepareStatement(s"EXPLAIN ANALYZE $sql")(prepare *> HPS.executeQuery(HRS.build[List, String]))
    }
}

class PostgresExplainUpdateOps[A](self: Update[A]) {

  /** Apply the argument `a` to construct a program in `[[doobie.free.connection.ConnectionIO ConnectionIO]]` which
    * returns the server's query plan for the query (i.e., `EXPLAIN` output). The query is not actually executed.
    */
  def explain(a: A): ConnectionIO[List[String]] = {
    self.inspect(a) { (sql, prepare) =>
      HC.prepareStatement(s"EXPLAIN $sql")(prepare *> HPS.executeQuery(HRS.build[List, String]))
    }
  }

  /** Apply the argument `a` to construct a program in `[[doobie.free.connection.ConnectionIO ConnectionIO]]` which
    * returns the server's query plan for the query, with a comparison to the actual execution (i.e., `EXPLAIN ANALYZE`
    * output). The query will be executed, but no results are returned.
    */
  def explainAnalyze(a: A): ConnectionIO[List[String]] =
    self.inspect(a) { (sql, prepare) =>
      HC.prepareStatement(s"EXPLAIN ANALYZE $sql")(prepare *> HPS.executeQuery(HRS.build[List, String]))
    }
}

object monaderror extends ToPostgresMonadErrorOps
