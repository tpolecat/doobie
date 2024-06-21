// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.hi

import cats.{Alternative, Monad}
import cats.data.NonEmptyList
import cats.syntax.all._

import doobie.enumerated.Holdability
import doobie.enumerated.FetchDirection
import doobie.util.{Read, Write}
import doobie.util.compat.FactoryCompat
import doobie.util.invariant._
import doobie.util.stream.repeatEvalChunks
import doobie.free.{resultset => IFRS}

import fs2.Stream

import java.sql.{ResultSetMetaData, SQLWarning}

/** Module of high-level constructors for `ResultSetIO` actions.
  * @group Modules
  */

object resultset {
  import implicits._

  /** Non-strict unit for capturing effects.
    * @group Constructors (Lifting)
    */
  def delay[A](a: => A): ResultSetIO[A] =
    IFRS.delay(a)

  /** @group Cursor Control */
  def absolute(row: Int): ResultSetIO[Boolean] =
    IFRS.absolute(row)

  /** @group Cursor Control */
  val afterLast: ResultSetIO[Unit] =
    IFRS.afterLast

  /** @group Cursor Control */
  val beforeFirst: ResultSetIO[Unit] =
    IFRS.beforeFirst

  /** @group Updating */
  val cancelRowUpdates: ResultSetIO[Unit] =
    IFRS.cancelRowUpdates

  /** @group Warnings */
  val clearWarnings: ResultSetIO[Unit] =
    IFRS.clearWarnings

  /** @group Updating */
  val deleteRow: ResultSetIO[Unit] =
    IFRS.deleteRow

  /** @group Cursor Control */
  val first: ResultSetIO[Boolean] =
    IFRS.first

  /** Read a value of type `A` starting at column `n`.
    * @group Results
    */
  def get[A](n: Int)(implicit A: Read[A]): ResultSetIO[A] =
    A.get(n)

  /** Read a value of type `A` starting at column 1.
    * @group Results
    */
  def get[A: Read]: ResultSetIO[A] =
    get(1)

  /** Consumes the remainder of the resultset, reading each row as a value of type `A` and accumulating them in a
    * standard library collection via `CanBuildFrom`.
    * @group Results
    */
  @SuppressWarnings(Array("org.wartremover.warts.While", "org.wartremover.warts.NonUnitStatements"))
  def build[F[_], A](implicit F: FactoryCompat[A, F[A]], A: Read[A]): ResultSetIO[F[A]] =
    IFRS.raw { rs =>
      val b = F.newBuilder
      while (rs.next)
        b += A.unsafeGet(rs, 1)
      b.result()
    }

  /** Consumes the remainder of the resultset, reading each row as a value of type `(A, B)` and accumulating them in a
    * standard library collection via `CanBuildFrom`.
    * @group Results
    */
  def buildPair[F[_, _], A, B](implicit F: FactoryCompat[(A, B), F[A, B]], A: Read[(A, B)]): ResultSetIO[F[A, B]] =
    IFRS.raw { rs =>
      val b = F.newBuilder
      while (rs.next)
        b += A.unsafeGet(rs, 1)
      b.result()
    }

  /** Consumes the remainder of the resultset, reading each row as a value of type `A`, mapping to `B`, and accumulating
    * them in a standard library collection via `CanBuildFrom`. This unusual constructor is a workaround for the
    * CanBuildFrom not having a sensible contravariant functor instance.
    * @group Results
    */
  @SuppressWarnings(Array("org.wartremover.warts.While", "org.wartremover.warts.NonUnitStatements"))
  def buildMap[F[_], A, B](f: A => B)(implicit F: FactoryCompat[B, F[B]], A: Read[A]): ResultSetIO[F[B]] =
    IFRS.raw { rs =>
      val b = F.newBuilder
      while (rs.next)
        b += f(A.unsafeGet(rs, 1))
      b.result()
    }

  /** Consumes the remainder of the resultset, reading each row as a value of type `A` and accumulating them in a
    * `Vector`.
    * @group Results
    */
  def vector[A: Read]: ResultSetIO[Vector[A]] =
    build[Vector, A]

  /** Consumes the remainder of the resultset, reading each row as a value of type `A` and accumulating them in a
    * `List`.
    * @group Results
    */
  def list[A: Read]: ResultSetIO[List[A]] =
    build[List, A]

  /** Like `getNext` but loops until the end of the resultset, gathering results in a `MonadPlus`.
    * @group Results
    */
  def accumulate[G[_]: Alternative, A: Read]: ResultSetIO[G[A]] =
    get[A].whileM(next)

  /** Updates a value of type `A` starting at column `n`.
    * @group Updating
    */
  def update[A](n: Int, a: A)(implicit A: Write[A]): ResultSetIO[Unit] =
    A.update(n, a)

  /** Updates a value of type `A` starting at column 1.
    * @group Updating
    */
  def update[A](a: A)(implicit A: Write[A]): ResultSetIO[Unit] =
    A.update(1, a)

  /** Similar to `next >> get` but lifted into `Option`; returns `None` when no more rows are available.
    * @group Results
    */
  def getNext[A: Read]: ResultSetIO[Option[A]] =
    next >>= {
      case true  => get[A].map(Some(_))
      case false => Monad[ResultSetIO].pure(None)
    }

  /** Similar to `getNext` but reads `chunkSize` rows at a time (the final chunk in a resultset may be smaller). A
    * non-positive `chunkSize` yields an empty `Seq` and consumes no rows. This method delegates to `getNextChunkV` and
    * widens to `Seq` for easier interoperability with streaming libraries that like `Seq` better.
    * @group Results
    */
  def getNextChunk[A: Read](chunkSize: Int): ResultSetIO[Seq[A]] =
    getNextChunkV[A](chunkSize).widen[Seq[A]]

  /** Similar to `getNext` but reads `chunkSize` rows at a time (the final chunk in a resultset may be smaller). A
    * non-positive `chunkSize` yields an empty `Vector` and consumes no rows.
    * @group Results
    */
  @SuppressWarnings(Array(
    "org.wartremover.warts.Var",
    "org.wartremover.warts.While",
    "org.wartremover.warts.NonUnitStatements"))
  def getNextChunkV[A](chunkSize: Int)(implicit A: Read[A]): ResultSetIO[Vector[A]] =
    IFRS.raw { rs =>
      var n = chunkSize
      val b = Vector.newBuilder[A]
      while (n > 0 && rs.next) {
        b += A.unsafeGet(rs, 1)
        n -= 1
      }
      b.result()
    }

  /** Equivalent to `getNext`, but verifies that there is exactly one row remaining.
    * @throws UnexpectedCursorPosition
    *   if there is not exactly one row remaining
    * @group Results
    */
  def getUnique[A: Read]: ResultSetIO[A] =
    (getNext[A], next).tupled.flatMap {
      case (Some(a), false) => IFRS.delay(a)
      case (Some(_), true)  => IFRS.raiseError(UnexpectedContinuation)
      case (None, _)        => IFRS.raiseError(UnexpectedEnd)
    }

  /** Equivalent to `getNext`, but verifies that there is at most one row remaining.
    * @throws UnexpectedContinuation
    *   if there is more than one row remaining
    * @group Results
    */
  def getOption[A: Read]: ResultSetIO[Option[A]] =
    (getNext[A], next).tupled.flatMap {
      case (a @ Some(_), false) => IFRS.delay(a)
      case (Some(_), true)      => IFRS.raiseError(UnexpectedContinuation)
      case (None, _)            => IFRS.delay(None)
    }

  /** Consumes the remainder of the resultset, but verifies that there is at least one row remaining.
    * @throws UnexpectedEnd
    *   if there is not at least one row remaining
    * @group Results
    */
  def nel[A: Read]: ResultSetIO[NonEmptyList[A]] =
    (getNext[A], list).tupled.flatMap {
      case (Some(a), as) => IFRS.delay(NonEmptyList(a, as))
      case (None, _)     => IFRS.raiseError(UnexpectedEnd)
    }

  /** Stream that reads from the `ResultSet` and returns a stream of `A`s. This is the preferred mechanism for dealing
    * with query results.
    * @group Results
    */
  def stream[A: Read](chunkSize: Int): Stream[ResultSetIO, A] =
    repeatEvalChunks(getNextChunk[A](chunkSize))

  /** @group Properties */
  val getFetchDirection: ResultSetIO[FetchDirection] =
    IFRS.getFetchDirection.flatMap(FetchDirection.fromIntF[ResultSetIO])

  /** @group Properties */
  val getFetchSize: ResultSetIO[Int] =
    IFRS.getFetchSize

  /** @group Properties */
  val getHoldability: ResultSetIO[Holdability] =
    IFRS.getHoldability.flatMap(Holdability.fromIntF[ResultSetIO])

  /** @group Properties */
  val getMetaData: ResultSetIO[ResultSetMetaData] =
    IFRS.getMetaData

  /** @group Cursor Control */
  val getRow: ResultSetIO[Int] =
    IFRS.getRow

  /** @group Warnings */
  val getWarnings: ResultSetIO[Option[SQLWarning]] =
    IFRS.getWarnings.map(Option(_))

  /** @group Updating */
  val insertRow: ResultSetIO[Unit] =
    IFRS.insertRow

  /** @group Cursor Control */
  val isAfterLast: ResultSetIO[Boolean] =
    IFRS.isAfterLast

  /** @group Cursor Control */
  val isBeforeFirst: ResultSetIO[Boolean] =
    IFRS.isBeforeFirst

  /** @group Cursor Control */
  val isFirst: ResultSetIO[Boolean] =
    IFRS.isFirst

  /** @group Cursor Control */
  val isLast: ResultSetIO[Boolean] =
    IFRS.isLast

  /** @group Cursor Control */
  val last: ResultSetIO[Boolean] =
    IFRS.last

  /** @group Cursor Control */
  val moveToCurrentRow: ResultSetIO[Unit] =
    IFRS.moveToCurrentRow

  /** @group Cursor Control */
  val moveToInsertRow: ResultSetIO[Unit] =
    IFRS.moveToInsertRow

  /** @group Cursor Control */
  val next: ResultSetIO[Boolean] =
    IFRS.next

  /** @group Cursor Control */
  val previous: ResultSetIO[Boolean] =
    IFRS.previous

  /** @group Cursor Control */
  val refreshRow: ResultSetIO[Unit] =
    IFRS.refreshRow

  /** @group Cursor Control */
  def relative(n: Int): ResultSetIO[Boolean] =
    IFRS.relative(n)

  /** @group Cursor Control */
  val rowDeleted: ResultSetIO[Boolean] =
    IFRS.rowDeleted

  /** @group Cursor Control */
  val rowInserted: ResultSetIO[Boolean] =
    IFRS.rowInserted

  /** @group Cursor Control */
  val rowUpdated: ResultSetIO[Boolean] =
    IFRS.rowUpdated

  /** @group Properties */
  def setFetchDirection(fd: FetchDirection): ResultSetIO[Unit] =
    IFRS.setFetchDirection(fd.toInt)

  /** @group Properties */
  def setFetchSize(n: Int): ResultSetIO[Unit] =
    IFRS.setFetchSize(n)

}
