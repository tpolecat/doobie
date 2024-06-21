// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.hi

import cats.effect.kernel.syntax.monadCancel._
import doobie.enumerated.Holdability
import doobie.enumerated.FetchDirection
import doobie.enumerated.ResultSetConcurrency
import doobie.enumerated.ResultSetType
import doobie.implicits._
import doobie.free.{statement => IFS, resultset => IFRS}

import java.sql.SQLWarning

import scala.Predef.intArrayOps

/** Module of high-level constructors for `StatementIO` actions.
  * @group Modules
  */
object statement {

  /** @group Batching */
  def addBatch(sql: String): StatementIO[Unit] =
    IFS.addBatch(sql)

  /** @group Batching */
  val clearBatch: StatementIO[Unit] =
    IFS.clearBatch

  /** @group Execution */
  val executeBatch: StatementIO[List[Int]] =
    IFS.executeBatch.map(_.toIndexedSeq.toList) // intArrayOps does not have `toList` in 2.13

  /** @group Execution */
  def executeQuery[A](sql: String)(k: ResultSetIO[A]): StatementIO[A] =
    IFS.executeQuery(sql).bracket(s => IFS.embed(s, k))(s => IFS.embed(s, IFRS.close))

  /** @group Execution */
  def executeUpdate(sql: String): StatementIO[Int] =
    IFS.executeUpdate(sql)

  /** @group Properties */
  val getFetchDirection: StatementIO[FetchDirection] =
    IFS.getFetchDirection.flatMap(FetchDirection.fromIntF[StatementIO])

  /** @group Properties */
  val getFetchSize: StatementIO[Int] =
    IFS.getFetchSize

  /** @group Results */
  def getGeneratedKeys[A](k: ResultSetIO[A]): StatementIO[A] =
    IFS.getGeneratedKeys.bracket(s => IFS.embed(s, k))(s => IFS.embed(s, IFRS.close))

  /** @group Properties */
  val getMaxFieldSize: StatementIO[Int] =
    IFS.getMaxFieldSize

  /** @group Properties */
  val getMaxRows: StatementIO[Int] =
    IFS.getMaxRows

  // /** @group Batching */
  // def getMoreResults(a: Int): StatementIO[Boolean] =
  //   Predef.???

  /** @group Batching */
  val getMoreResults: StatementIO[Boolean] =
    IFS.getMoreResults

  /** @group Properties */
  val getQueryTimeout: StatementIO[Int] =
    IFS.getQueryTimeout

  /** @group Batching */
  def getResultSet[A](k: ResultSetIO[A]): StatementIO[A] =
    IFS.getResultSet.flatMap(s => IFS.embed(s, k))

  /** @group Properties */
  val getResultSetConcurrency: StatementIO[ResultSetConcurrency] =
    IFS.getResultSetConcurrency.flatMap(ResultSetConcurrency.fromIntF[StatementIO])

  /** @group Properties */
  val getResultSetHoldability: StatementIO[Holdability] =
    IFS.getResultSetHoldability.flatMap(Holdability.fromIntF[StatementIO])

  /** @group Properties */
  val getResultSetType: StatementIO[ResultSetType] =
    IFS.getResultSetType.flatMap(ResultSetType.fromIntF[StatementIO])

  /** @group Results */
  val getUpdateCount: StatementIO[Int] =
    IFS.getUpdateCount

  /** @group Results */
  val getWarnings: StatementIO[SQLWarning] =
    IFS.getWarnings

  /** @group Properties */
  def setCursorName(name: String): StatementIO[Unit] =
    IFS.setCursorName(name)

  /** @group Properties */
  def setEscapeProcessing(a: Boolean): StatementIO[Unit] =
    IFS.setEscapeProcessing(a)

  /** @group Properties */
  def setFetchDirection(fd: FetchDirection): StatementIO[Unit] =
    IFS.setFetchDirection(fd.toInt)

  /** @group Properties */
  def setFetchSize(n: Int): StatementIO[Unit] =
    IFS.setFetchSize(n)

  /** @group Properties */
  def setMaxFieldSize(n: Int): StatementIO[Unit] =
    IFS.setMaxFieldSize(n)

  /** @group Properties */
  def setMaxRows(n: Int): StatementIO[Unit] =
    IFS.setMaxRows(n)

  /** @group Properties */
  def setQueryTimeout(a: Int): StatementIO[Unit] =
    IFS.setQueryTimeout(a)

}
