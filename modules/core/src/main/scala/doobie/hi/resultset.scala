// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.hi

import cats.{ Alternative, Monad }
import cats.data.NonEmptyList
import cats.implicits._

import doobie.enum.Holdability
import doobie.enum.FetchDirection
import doobie.util.{ Read, Write }
import doobie.util.compat.FactoryCompat
import doobie.util.invariant._
import doobie.util.stream.repeatEvalChunks

import fs2.Stream

import java.sql.{ ResultSetMetaData, SQLWarning }

/**
 * Module of high-level constructors for `ResultSetIO` actions.
 * @group Modules
 */
@SuppressWarnings(Array("org.wartremover.warts.Overloading"))
object resultset {
  import implicits._

  /**
   * Non-strict unit for capturing effects.
   * @group Constructors (Lifting)
   */
  def delay[A](a: => A): ResultSetIO[A] =
    FRS.delay(a)

  /** @group Cursor Control */
  def absolute(row: Int): ResultSetIO[Boolean] =
    FRS.absolute(row)

  /** @group Cursor Control */
  val afterLast: ResultSetIO[Unit] =
    FRS.afterLast

  /** @group Cursor Control */
  val beforeFirst: ResultSetIO[Unit] =
    FRS.beforeFirst

  /** @group Updating */
  val cancelRowUpdates: ResultSetIO[Unit] =
    FRS.cancelRowUpdates

  /** @group Warnings */
  val clearWarnings: ResultSetIO[Unit] =
    FRS.clearWarnings

  /** @group Updating */
  val deleteRow: ResultSetIO[Unit] =
    FRS.deleteRow

  /** @group Cursor Control */
  val first: ResultSetIO[Boolean] =
    FRS.first

  /**
   * Read a value of type `A` starting at column `n`.
   * @group Results
   */
  def get[A](n: Int)(implicit A: Read[A]): ResultSetIO[A] =
    A.get(n)

  /**
   * Read a value of type `A` starting at column 1.
   * @group Results
   */
  def get[A: Read]: ResultSetIO[A] =
    get(1)

  /**
   * Consumes the remainder of the resultset, reading each row as a value of type `A` and
   * accumulating them in a standard library collection via `CanBuildFrom`.
   * @group Results
   */
  @SuppressWarnings(Array("org.wartremover.warts.While", "org.wartremover.warts.NonUnitStatements"))
  def build[F[_], A](implicit F: FactoryCompat[A, F[A]], A: Read[A]): ResultSetIO[F[A]] =
    FRS.raw { e =>
      e.unsafeTrace(s"HRS.build ${Read[A].gets.map(_._1.typeStack.head.getOrElse("«unknown»")).mkString("(", ", ", ")")}") { _ =>
        val b = F.newBuilder
        while (e.jdbc.next)
          b += A.unsafeGet(e.jdbc, 1)
        b.result()
      }
    }

  /**
   * Consumes the remainder of the resultset, reading each row as a value of type `A`, mapping to
   * `B`, and accumulating them in a standard library collection via `CanBuildFrom`. This unusual
   * constructor is a workaround for the CanBuildFrom not having a sensible contravariant functor
   * instance.
   * @group Results
   */
  @SuppressWarnings(Array("org.wartremover.warts.While", "org.wartremover.warts.NonUnitStatements"))
  def buildMap[F[_], A, B](f: A => B)(implicit F: FactoryCompat[B, F[B]], A: Read[A]): ResultSetIO[F[B]] =
    FRS.raw { e =>
      e.unsafeTrace(s"HRS.buildMap ${Read[A].gets.map(_._1.typeStack.head.getOrElse("«unknown»")).mkString("(", ", ", ")")}") { _ =>
        val b = F.newBuilder
        while (e.jdbc.next)
          b += f(A.unsafeGet(e.jdbc, 1))
        b.result()
      }
    }

  /**
   * Consumes the remainder of the resultset, reading each row as a value of type `A` and
   * accumulating them in a `Vector`.
   * @group Results
   */
  def vector[A: Read]: ResultSetIO[Vector[A]] =
    build[Vector, A]

  /**
   * Consumes the remainder of the resultset, reading each row as a value of type `A` and
   * accumulating them in a `List`.
   * @group Results
   */
  def list[A: Read]: ResultSetIO[List[A]] =
    build[List, A]

  /**
   * Like `getNext` but loops until the end of the resultset, gathering results in a `MonadPlus`.
   * @group Results
   */
  def accumulate[G[_]: Alternative, A: Read]: ResultSetIO[G[A]] =
    get[A].whileM(next)

  /**
   * Updates a value of type `A` starting at column `n`.
   * @group Updating
   */
  def update[A](n: Int, a:A)(implicit A: Write[A]): ResultSetIO[Unit] =
    A.update(n, a)

  /**
   * Updates a value of type `A` starting at column 1.
   * @group Updating
   */
  def update[A](a: A)(implicit A: Write[A]): ResultSetIO[Unit] =
    A.update(1, a)

  /**
   * Similar to `next >> get` but lifted into `Option`; returns `None` when no more rows are
   * available.
   * @group Results
   */
  def getNext[A: Read]: ResultSetIO[Option[A]] =
    next >>= {
      case true  => get[A].map(Some(_))
      case false => Monad[ResultSetIO].pure(None)
    }

  /**
   * Similar to `getNext` but reads `chunkSize` rows at a time (the final chunk in a resultset may
   * be smaller). A non-positive `chunkSize` yields an empty `Seq` and consumes no rows. This method
   * delegates to `getNextChunkV` and widens to `Seq` for easier interoperability with streaming
   * libraries that like `Seq` better.
   * @group Results
   */
  def getNextChunk[A: Read](chunkSize: Int): ResultSetIO[Seq[A]] =
    getNextChunkV[A](chunkSize).widen[Seq[A]]

  /**
   * Similar to `getNext` but reads `chunkSize` rows at a time (the final chunk in a resultset may
   * be smaller). A non-positive `chunkSize` yields an empty `Vector` and consumes no rows.
   * @group Results
   */
  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.While", "org.wartremover.warts.NonUnitStatements"))
  def getNextChunkV[A](chunkSize: Int)(implicit A: Read[A]): ResultSetIO[Vector[A]] =
    FRS.raw { e =>
      e.unsafeTrace(s"HRS.getNextChunkV ${Read[A].gets.map(_._1.typeStack.head.getOrElse("«unknown»")).mkString("(", ", ", ")")}") { _ =>
        var n = chunkSize
        val b = Vector.newBuilder[A]
        while (n > 0 && e.jdbc.next) {
          b += A.unsafeGet(e.jdbc, 1)
          n -= 1
        }
        b.result()
      }
    }

  /**
   * Equivalent to `getNext`, but verifies that there is exactly one row remaining.
   * @throws `UnexpectedCursorPosition` if there is not exactly one row remaining
   * @group Results
   */
  def getUnique[A: Read]: ResultSetIO[A] =
    (getNext[A], next) mapN {
      case (Some(a), false) => a
      case (Some(_), true)  => throw UnexpectedContinuation
      case (None, _)        => throw UnexpectedEnd
    }

  /**
   * Equivalent to `getNext`, but verifies that there is at most one row remaining.
   * @throws `UnexpectedContinuation` if there is more than one row remaining
   * @group Results
   */
  def getOption[A: Read]: ResultSetIO[Option[A]] =
    (getNext[A], next) mapN {
      case (a @ Some(_), false) => a
      case (Some(_), true)      => throw UnexpectedContinuation
      case (None, _)            => None
    }

  /**
    * Consumes the remainder of the resultset, but verifies that there is at least one row remaining.
    * @throws `UnexpectedEnd` if there is not at least one row remaining
    * @group Results
    */
  def nel[A: Read]: ResultSetIO[NonEmptyList[A]] =
    (getNext[A], list) mapN {
      case (Some(a), as) => NonEmptyList(a, as)
      case (None, _)     => throw UnexpectedEnd
    }

  /**
   * Stream that reads from the `ResultSet` and returns a stream of `A`s. This is the preferred
   * mechanism for dealing with query results.
   * @group Results
   */
  def stream[A: Read](chunkSize: Int): Stream[ResultSetIO, A] =
    repeatEvalChunks(getNextChunk[A](chunkSize))

  /** @group Properties */
  val getFetchDirection: ResultSetIO[FetchDirection] =
    FRS.getFetchDirection.map(FetchDirection.unsafeFromInt)

  /** @group Properties */
  val getFetchSize: ResultSetIO[Int] =
    FRS.getFetchSize

  /** @group Properties */
  val getHoldability: ResultSetIO[Holdability] =
    FRS.getHoldability.map(Holdability.unsafeFromInt)

  /** @group Properties */
  val getMetaData: ResultSetIO[ResultSetMetaData] =
    FRS.getMetaData

  /** @group Cursor Control */
  val getRow: ResultSetIO[Int] =
    FRS.getRow

  /** @group Warnings */
  val getWarnings: ResultSetIO[Option[SQLWarning]] =
    FRS.getWarnings.map(Option(_))

  /** @group Updating */
  val insertRow: ResultSetIO[Unit] =
    FRS.insertRow

  /** @group Cursor Control */
  val isAfterLast: ResultSetIO[Boolean] =
    FRS.isAfterLast

  /** @group Cursor Control */
  val isBeforeFirst: ResultSetIO[Boolean] =
    FRS.isBeforeFirst

  /** @group Cursor Control */
  val isFirst: ResultSetIO[Boolean] =
    FRS.isFirst

  /** @group Cursor Control */
  val isLast: ResultSetIO[Boolean] =
    FRS.isLast

  /** @group Cursor Control */
  val last: ResultSetIO[Boolean] =
    FRS.last

  /** @group Cursor Control */
  val moveToCurrentRow: ResultSetIO[Unit] =
    FRS.moveToCurrentRow

  /** @group Cursor Control */
  val moveToInsertRow: ResultSetIO[Unit] =
    FRS.moveToInsertRow

  /** @group Cursor Control */
  val next: ResultSetIO[Boolean] =
    FRS.next

  /** @group Cursor Control */
  val previous: ResultSetIO[Boolean] =
    FRS.previous

  /** @group Cursor Control */
  val refreshRow: ResultSetIO[Unit] =
    FRS.refreshRow

  /** @group Cursor Control */
  def relative(n: Int): ResultSetIO[Boolean] =
    FRS.relative(n)

  /** @group Cursor Control */
  val rowDeleted: ResultSetIO[Boolean] =
    FRS.rowDeleted

  /** @group Cursor Control */
  val rowInserted: ResultSetIO[Boolean] =
    FRS.rowInserted

  /** @group Cursor Control */
  val rowUpdated: ResultSetIO[Boolean] =
    FRS.rowUpdated

  /** @group Properties */
  def setFetchDirection(fd: FetchDirection): ResultSetIO[Unit] =
    FRS.setFetchDirection(fd.toInt)

  /** @group Properties */
  def setFetchSize(n: Int): ResultSetIO[Unit] =
    FRS.setFetchSize(n)

}
