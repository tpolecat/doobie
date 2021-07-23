// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.hi

import doobie.enumerated.JdbcType
import doobie.util.{ Get, Put }
import doobie.enumerated.ColumnNullable
import doobie.enumerated.ParameterNullable
import doobie.enumerated.ParameterMode
import doobie.enumerated.Holdability
import doobie.enumerated.Nullability.NullabilityKnown
import doobie.enumerated.FetchDirection
import doobie.enumerated.ResultSetConcurrency
import doobie.enumerated.ResultSetType

import doobie.util.{ Read, Write }
import doobie.util.analysis._
import doobie.util.stream.repeatEvalChunks

import doobie.syntax.align._

import java.sql.{ ParameterMetaData, ResultSetMetaData, SQLWarning }

import scala.Predef.{ intArrayOps, intWrapper }

import cats.Foldable
import cats.syntax.all._
import cats.data.Ior
import cats.effect.syntax.bracket._
import fs2.Stream
import fs2.Stream.bracket

/**
 * Module of high-level constructors for `PreparedStatementIO` actions. Batching operations are not
 * provided; see the `statement` module for this functionality.
 * @group Modules
 */

object preparedstatement {
  import implicits._

  // fs2 handler, not public
  private def unrolled[A: Read](rs: java.sql.ResultSet, chunkSize: Int): Stream[PreparedStatementIO, A] =
    repeatEvalChunks(FPS.embed(rs, resultset.getNextChunk[A](chunkSize)))

  /** @group Execution */
  def stream[A: Read](chunkSize: Int): Stream[PreparedStatementIO, A] =
    bracket(FPS.executeQuery)(FPS.embed(_, FRS.close)).flatMap(unrolled[A](_, chunkSize))

  /**
   * Non-strict unit for capturing effects.
   * @group Constructors (Lifting)
   */
  def delay[A](a: => A): PreparedStatementIO[A] =
    FPS.delay(a)

  /** @group Batching */
  val executeBatch: PreparedStatementIO[List[Int]] =
    FPS.executeBatch.map(_.toIndexedSeq.toList) // intArrayOps does not have `toList` in 2.13

  /** @group Batching */
  val addBatch: PreparedStatementIO[Unit] =
    FPS.addBatch

  /**
   * Add many sets of parameters and execute as a batch update, returning total rows updated. Note
   * that failed updates are not reported (see https://github.com/tpolecat/doobie/issues/706). This
   * API is likely to change.
   * @group Batching
   */
  def addBatchesAndExecute[F[_]: Foldable, A: Write](fa: F[A]): PreparedStatementIO[Int] =
    fa.toList
      .foldRight(executeBatch)((a, b) => set(a) *> addBatch *> b)
      .map(_.foldLeft(0)((acc, n) => acc + (n max 0))) // treat negatives (failures) as no rows updated

  /**
   * Add many sets of parameters.
   * @group Batching
   */
  def addBatches[F[_]: Foldable, A: Write](fa: F[A]): PreparedStatementIO[Unit] =
    fa.toList.foldRight(().pure[PreparedStatementIO])((a, b) => set(a) *> addBatch *> b)

  /** @group Execution */
  def executeQuery[A](k: ResultSetIO[A]): PreparedStatementIO[A] =
    FPS.executeQuery.bracket(s => FPS.embed(s, k))(s => FPS.embed(s, FRS.close))

  /** @group Execution */
  val executeUpdate: PreparedStatementIO[Int] =
    FPS.executeUpdate

  /** @group Execution */
  def executeUpdateWithUniqueGeneratedKeys[A: Read]: PreparedStatementIO[A] =
    executeUpdate.flatMap(_ => getUniqueGeneratedKeys[A])

  /** @group Execution */
  def executeUpdateWithOptionallyGeneratedKeys[A: Read]: PreparedStatementIO[Option[A]] =
    executeUpdate.flatMap(_ => getOptionallyGeneratedKeys[A])

 /** @group Execution */
  def executeUpdateWithGeneratedKeys[A: Read](chunkSize: Int): Stream[PreparedStatementIO, A] =
    bracket(FPS.executeUpdate *> FPS.getGeneratedKeys)(FPS.embed(_, FRS.close)).flatMap(unrolled[A](_, chunkSize))

  /**
   * Compute the column `JdbcMeta` list for this `PreparedStatement`.
   * @group Metadata
   */
  def getColumnJdbcMeta: PreparedStatementIO[List[ColumnMeta]] =
    FPS.getMetaData.flatMap {
      case null => FPS.pure(Nil) // https://github.com/tpolecat/doobie/issues/262
      case md   =>
        (1 to md.getColumnCount).toList.traverse { i =>
          for {
            n <- ColumnNullable.fromIntF[PreparedStatementIO](md.isNullable(i))
          } yield {
            val j = JdbcType.fromInt(md.getColumnType(i))
            val s = md.getColumnTypeName(i)
            val c = md.getColumnName(i)
            ColumnMeta(j, s, n.toNullability, c)
          }
        }
    }

  /**
   * Compute the column mappings for this `PreparedStatement` by aligning its `JdbcMeta`
   * with the `JdbcMeta` provided by a `Write` instance.
   * @group Metadata
   */
  def getColumnMappings[A](implicit A: Read[A]): PreparedStatementIO[List[(Get[_], NullabilityKnown) Ior ColumnMeta]] =
    getColumnJdbcMeta.map(m => A.gets align m)

  /** @group Properties */
  val getFetchDirection: PreparedStatementIO[FetchDirection] =
    FPS.getFetchDirection.flatMap(FetchDirection.fromIntF[PreparedStatementIO])

  /** @group Properties */
  val getFetchSize: PreparedStatementIO[Int] =
    FPS.getFetchSize

  /** @group Results */
  def getGeneratedKeys[A](k: ResultSetIO[A]): PreparedStatementIO[A] =
    FPS.getGeneratedKeys.bracket(s => FPS.embed(s, k))(s => FPS.embed(s, FRS.close))

  /** @group Results */
  def getUniqueGeneratedKeys[A: Read]: PreparedStatementIO[A] =
    getGeneratedKeys(resultset.getUnique[A])

  /** @group Results */
  def getOptionallyGeneratedKeys[A: Read]: PreparedStatementIO[Option[A]] =
    getGeneratedKeys(resultset.getOption[A])

  /**
   * Compute the parameter `JdbcMeta` list for this `PreparedStatement`.
   * @group Metadata
   */
  def getParameterJdbcMeta: PreparedStatementIO[List[ParameterMeta]] =
    FPS.getParameterMetaData.flatMap { md =>
      (1 to md.getParameterCount).toList.traverse { i =>
        for {
          n <- ParameterNullable.fromIntF[PreparedStatementIO](md.isNullable(i))
          m <- ParameterMode.fromIntF[PreparedStatementIO](md.getParameterMode(i))
        } yield {
          val j = JdbcType.fromInt(md.getParameterType(i))
          val s = md.getParameterTypeName(i)
          ParameterMeta(j, s, n.toNullability, m)
        }
      }
    }

  /**
   * Compute the parameter mappings for this `PreparedStatement` by aligning its `JdbcMeta`
   * with the `JdbcMeta` provided by a `Write` instance.
   * @group Metadata
   */
  def getParameterMappings[A](implicit A: Write[A]): PreparedStatementIO[List[(Put[_], NullabilityKnown) Ior ParameterMeta]] =
    getParameterJdbcMeta.map(m => A.puts align m)

  /** @group Properties */
  val getMaxFieldSize: PreparedStatementIO[Int] =
    FPS.getMaxFieldSize

  /** @group Properties */
  val getMaxRows: PreparedStatementIO[Int] =
    FPS.getMaxRows

  /** @group MetaData */
  val getMetaData: PreparedStatementIO[ResultSetMetaData] =
    FPS.getMetaData

  /** @group MetaData */
  val getParameterMetaData: PreparedStatementIO[ParameterMetaData] =
    FPS.getParameterMetaData

  /** @group Properties */
  val getQueryTimeout: PreparedStatementIO[Int] =
    FPS.getQueryTimeout

  /** @group Properties */
  val getResultSetConcurrency: PreparedStatementIO[ResultSetConcurrency] =
    FPS.getResultSetConcurrency.flatMap(ResultSetConcurrency.fromIntF[PreparedStatementIO])

  /** @group Properties */
  val getResultSetHoldability: PreparedStatementIO[Holdability] =
    FPS.getResultSetHoldability.flatMap(Holdability.fromIntF[PreparedStatementIO])

  /** @group Properties */
  val getResultSetType: PreparedStatementIO[ResultSetType] =
    FPS.getResultSetType.flatMap(ResultSetType.fromIntF[PreparedStatementIO])

  /** @group Results */
  val getWarnings: PreparedStatementIO[SQLWarning] =
    FPS.getWarnings

  /**
   * Set the given writable value, starting at column `n`.
   * @group Parameters
   */
  def set[A](n: Int, a: A)(implicit A: Write[A]): PreparedStatementIO[Unit] =
    A.set(n, a)

  /**
   * Set the given writable value, starting at column `1`.
   * @group Parameters
   */
  def set[A](a: A)(implicit A: Write[A]): PreparedStatementIO[Unit] =
    A.set(1, a)

  /** @group Properties */
  def setCursorName(name: String): PreparedStatementIO[Unit] =
    FPS.setCursorName(name)

  /** @group Properties */
  def setEscapeProcessing(a: Boolean): PreparedStatementIO[Unit] =
    FPS.setEscapeProcessing(a)

  /** @group Properties */
  def setFetchDirection(fd: FetchDirection): PreparedStatementIO[Unit] =
    FPS.setFetchDirection(fd.toInt)

  /** @group Properties */
  def setFetchSize(n: Int): PreparedStatementIO[Unit] =
    FPS.setFetchSize(n)

  /** @group Properties */
  def setMaxFieldSize(n: Int): PreparedStatementIO[Unit] =
    FPS.setMaxFieldSize(n)

  /** @group Properties */
  def setMaxRows(n: Int): PreparedStatementIO[Unit] =
    FPS.setMaxRows(n)

  /** @group Properties */
  def setQueryTimeout(a: Int): PreparedStatementIO[Unit] =
    FPS.setQueryTimeout(a)

}
