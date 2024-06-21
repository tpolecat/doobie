// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.free

import cats.{Monoid, Semigroup}
import doobie.WeakAsync

trait Types {

  /** @group Type Aliases - Free API */
  type BlobIO[A] = blob.BlobIO[A]

  /** @group Type Aliases - Free API */
  type CallableStatementIO[A] = callablestatement.CallableStatementIO[A]

  /** @group Type Aliases - Free API */
  type ClobIO[A] = clob.ClobIO[A]

  /** @group Type Aliases - Free API */
  type ConnectionIO[A] = connection.ConnectionIO[A]

  /** @group Type Aliases - Free API */
  type DatabaseMetaDataIO[A] = databasemetadata.DatabaseMetaDataIO[A]

  /** @group Type Aliases - Free API */
  type DriverIO[A] = driver.DriverIO[A]

  /** @group Type Aliases - Free API */
  type NClobIO[A] = nclob.NClobIO[A]

  /** @group Type Aliases - Free API */
  type PreparedStatementIO[A] = preparedstatement.PreparedStatementIO[A]

  /** @group Type Aliases - Free API */
  type RefIO[A] = ref.RefIO[A]

  /** @group Type Aliases - Free API */
  type ResultSetIO[A] = resultset.ResultSetIO[A]

  /** @group Type Aliases - Free API */
  type SQLDataIO[A] = sqldata.SQLDataIO[A]

  /** @group Type Aliases - Free API */
  type SQLInputIO[A] = sqlinput.SQLInputIO[A]

  /** @group Type Aliases - Free API */
  type SQLOutputIO[A] = sqloutput.SQLOutputIO[A]

  /** @group Type Aliases - Free API */
  type StatementIO[A] = statement.StatementIO[A]
}

trait Modules {

  /** @group Module Aliases - Free API */
  val FB = blob

  /** @group Module Aliases - Free API */
  val FCS = callablestatement

  /** @group Module Aliases - Free API */
  val FCL = clob

  /** @group Module Aliases - Free API */
  val FC = connection

  /** @group Module Aliases - Free API */
  val FDMD = databasemetadata

  /** @group Module Aliases - Free API */
  val FD = driver

  /** @group Module Aliases - Free API */
  val FNCL = nclob

  /** @group Module Aliases - Free API */
  val FPS = preparedstatement

  /** @group Module Aliases - Free API */
  val FREF = ref

  /** @group Module Aliases - Free API */
  val FRS = resultset

  /** @group Module Aliases - Free API */
  val FSD = sqldata

  /** @group Module Aliases - Free API */
  val FSI = sqlinput

  /** @group Module Aliases - Free API */
  val FSO = sqloutput

  /** @group Module Aliases - Free API */
  val FS = statement
}

trait Instances {

  /** @group Typeclass Instances */
  implicit val WeakAsyncBlobIO: WeakAsync[BlobIO] =
    blob.WeakAsyncBlobIO

  /** @group Typeclass Instances */
  implicit val WeakAsyncCallableStatementIO: WeakAsync[CallableStatementIO] =
    callablestatement.WeakAsyncCallableStatementIO

  /** @group Typeclass Instances */
  implicit val WeakAsyncClobIO: WeakAsync[ClobIO] =
    clob.WeakAsyncClobIO

  /** @group Typeclass Instances */
  implicit val WeakAsyncConnectionIO: WeakAsync[ConnectionIO] =
    connection.WeakAsyncConnectionIO

  /** @group Typeclass Instances */
  implicit def MonoidConnectionIO[A: Monoid]: Monoid[ConnectionIO[A]] =
    connection.MonoidConnectionIO[A]

  /** @group Typeclass Instances */
  implicit def SemigroupConnectionIO[A: Semigroup]: Semigroup[ConnectionIO[A]] =
    connection.SemigroupConnectionIO[A]

  /** @group Typeclass Instances */
  implicit val WeakAsyncDatabaseMetaDataIO: WeakAsync[DatabaseMetaDataIO] =
    databasemetadata.WeakAsyncDatabaseMetaDataIO

  /** @group Typeclass Instances */
  implicit val WeakAsyncDriverIO: WeakAsync[DriverIO] =
    driver.WeakAsyncDriverIO

  /** @group Typeclass Instances */
  implicit val WeakAsyncNClobIO: WeakAsync[NClobIO] =
    nclob.WeakAsyncNClobIO

  /** @group Typeclass Instances */
  implicit val WeakAsyncPreparedStatementIO: WeakAsync[PreparedStatementIO] =
    preparedstatement.WeakAsyncPreparedStatementIO

  /** @group Typeclass Instances */
  implicit val WeakAsyncRefIO: WeakAsync[RefIO] =
    ref.WeakAsyncRefIO

  /** @group Typeclass Instances */
  implicit val WeakAsyncResultSetIO: WeakAsync[ResultSetIO] =
    resultset.WeakAsyncResultSetIO

  /** @group Typeclass Instances */
  implicit val WeakAsyncSQLDataIO: WeakAsync[SQLDataIO] =
    sqldata.WeakAsyncSQLDataIO

  /** @group Typeclass Instances */
  implicit val WeakAsyncSQLInputIO: WeakAsync[SQLInputIO] =
    sqlinput.WeakAsyncSQLInputIO

  /** @group Typeclass Instances */
  implicit val WeakAsyncSQLOutputIO: WeakAsync[SQLOutputIO] =
    sqloutput.WeakAsyncSQLOutputIO

  /** @group Typeclass Instances */
  implicit val WeakAsyncStatementIO: WeakAsync[StatementIO] =
    statement.WeakAsyncStatementIO

}
