// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package org.typelevel.doobie.util

import cats.*
import cats.syntax.all.*
import org.typelevel.doobie.free.connection.ConnectionIO
import org.typelevel.doobie.free.preparedstatement.PreparedStatementIO
import org.typelevel.doobie.implicits.*
import org.typelevel.doobie.util.analysis.Analysis
import org.typelevel.doobie.util.pos.Pos
import org.typelevel.doobie.free.{connection as IFC, preparedstatement as IFPS}
import org.typelevel.doobie.hi.connection.{PreparedExecution, PreparedExecutionWithoutProcessStep}
import org.typelevel.doobie.hi.{connection as IHC, preparedstatement as IHPS, resultset as IHRS}
import org.typelevel.doobie.util.fragment.Fragment
import org.typelevel.doobie.util.log.{LoggingInfo, Parameters}
import fs2.Stream

import scala.Predef.genericArrayOps

/** Module defining updates parameterized by input type. */
object update {

  val DefaultChunkSize: Int = query.DefaultChunkSize

  /** Partial application hack to allow calling updateManyWithGeneratedKeys without passing the F[_] type argument
    * explicitly.
    */
  trait UpdateManyWithGeneratedKeysPartiallyApplied[A, K] {
    def apply[F[_]](as: F[A])(implicit F: Foldable[F], K: Read[K]): Stream[ConnectionIO, K] =
      withChunkSize(as, DefaultChunkSize)
    def withChunkSize[F[_]](as: F[A], chunkSize: Int)(implicit F: Foldable[F], K: Read[K]): Stream[ConnectionIO, K]
  }

  /** An update parameterized by some input type `A`. This is the type constructed by the `sql` interpolator.
    */
  trait Update[A] { u =>

    // Contravariant coyoneda trick for A
    protected implicit val write: Write[A]

    /** The SQL string.
      * @group Diagnostics
      */
    val sql: String

    /** An optional [[doobie.util.pos.Pos]] indicating the source location where this [[Update]] was constructed. This
      * is used only for diagnostic purposes.
      * @group Diagnostics
      */
    val pos: Option[Pos]

    /** Convert this Update to a `Fragment`. */
    def toFragment(a: A): Fragment =
      write.toFragment(a, sql)

    /** Label to be used during logging */
    val label: String

    /** Program to construct an analysis of this query's SQL statement and asserted parameter types.
      * @group Diagnostics
      */
    def analysis: ConnectionIO[Analysis] =
      IHC.prepareUpdateAnalysis[A](sql)

    /** Program to construct an analysis of this query's SQL statement and result set column types.
      * @group Diagnostics
      */
    def outputAnalysis: ConnectionIO[Analysis] =
      IHC.prepareUpdateAnalysis0(sql)

    /** Program to construct an inspection of the query. Given arguments `a`, calls `f` with the SQL representation of
      * the query and a statement with all arguments set. Returns the result of the `ConnectionIO` program constructed.
      *
      * @group Diagnostics
      */
    def inspect[R](a: A)(f: (String, PreparedStatementIO[Unit]) => ConnectionIO[R]): ConnectionIO[R] =
      f(sql, IHPS.set(a))

    /** Construct a program to execute the update and yield a count of affected rows, given the writable argument `a`.
      * @group Execution
      */
    def run(a: A): ConnectionIO[Long] =
      IHC.executeWithoutResultSet(prepareExecutionForRun(a), loggingForRun(a))

    /** Just like `run` but allowing to alter `PreparedExecutionWithoutProcessStep`.
      */
    def runAlteringExecution(
        a: A,
        fn: PreparedExecutionWithoutProcessStep[Long] => PreparedExecutionWithoutProcessStep[Long]
    ): ConnectionIO[Long] =
      IHC.executeWithoutResultSet(fn(prepareExecutionForRun(a)), loggingForRun(a))

    private def prepareExecutionForRun(a: A): PreparedExecutionWithoutProcessStep[Long] =
      PreparedExecutionWithoutProcessStep(
        create = IFC.prepareStatement(sql),
        prep = IHPS.set(a),
        exec = IFPS.executeLargeUpdate
      )

    private def loggingForRun(a: A): LoggingInfo =
      LoggingInfo(
        sql,
        Parameters.NonBatch(write.toList(a)),
        label
      )

    /** Add many sets of parameters and execute as a batch update, returning total rows updated. Note that when an error
      * occurred while executing the batch, your JDBC driver may decide to continue executing the rest of the batch
      * instead of raising a `BatchUpdateException`. Please refer to your JDBC driver's documentation for its exact
      * behaviour. See
      * [[https://docs.oracle.com/en/java/javase/11/docs/api/java.sql/java/sql/Statement.html#executeBatch()]] for more
      * information
      * @group Execution
      */
    def updateMany[F[_]: Foldable](fa: F[A]): ConnectionIO[Long] =
      IHC.executeWithoutResultSet(prepareExecutionForUpdateMany(fa), loggingInfoForUpdateMany(fa))

    /** Just like `updateMany` but allowing to alter `PreparedExecutionWithoutProcessStep`.
      */
    def updateManyAlteringExecution[F[_]: Foldable](
        fa: F[A],
        fn: PreparedExecutionWithoutProcessStep[Long] => PreparedExecutionWithoutProcessStep[Long]
    ): ConnectionIO[Long] =
      IHC.executeWithoutResultSet(fn(prepareExecutionForUpdateMany(fa)), loggingInfoForUpdateMany(fa))

    private def prepareExecutionForUpdateMany[F[_]: Foldable](fa: F[A]): PreparedExecutionWithoutProcessStep[Long] =
      PreparedExecutionWithoutProcessStep(
        create = IFC.prepareStatement(sql),
        prep = fa.foldMap(a => IHPS.set(a) *> IFPS.addBatch),
        exec = IFPS.executeLargeBatch.map(updateCounts => updateCounts.foldLeft(0L)((acc, n) => acc + n.max(0L)))
      )

    private def loggingInfoForUpdateMany[F[_]: Foldable](fa: F[A]) =
      LoggingInfo(
        sql,
        Parameters.Batch(() => fa.toList.map(write.toList)),
        label
      )

    /** Construct a stream that performs a batch update as with `updateMany`, yielding generated keys of readable type
      * `K`, identified by the specified columns. Note that not all drivers support generated keys, and some support
      * only a single key column.
      * @group Execution
      */
    def updateManyWithGeneratedKeys[K](columns: String*): UpdateManyWithGeneratedKeysPartiallyApplied[A, K] =
      new UpdateManyWithGeneratedKeysPartiallyApplied[A, K] {
        def withChunkSize[F[_]](as: F[A], chunkSize: Int)(implicit
            F: Foldable[F],
            K: Read[K]
        ): Stream[ConnectionIO, K] =
          IHC.stream(
            create = IFC.prepareStatement(sql, columns.toArray),
            prep = IHPS.addBatches(as),
            exec = IFPS.executeBatch *> IFPS.getGeneratedKeys,
            chunkSize = chunkSize,
            loggingInfo = loggingInfoForUpdateMany(as)
          )
      }

    /** Construct a stream that performs the update, yielding generated keys of readable type `K`, identified by the
      * specified columns, given a writable argument `a`. Note that not all drivers support generated keys, and some
      * support only a single key column.
      * @group Execution
      */
    def withGeneratedKeys[K: Read](columns: String*)(a: A): Stream[ConnectionIO, K] =
      withGeneratedKeysWithChunkSize[K](columns*)(a, DefaultChunkSize)

    /** Construct a stream that performs the update, yielding generated keys of readable type `K`, identified by the
      * specified columns, given a writable argument `a` and `chunkSize`. Note that not all drivers support generated
      * keys, and some support only a single key column.
      * @group Execution
      */
    def withGeneratedKeysWithChunkSize[K: Read](columns: String*)(a: A, chunkSize: Int): Stream[ConnectionIO, K] =
      IHC.stream(
        create = IFC.prepareStatement(sql, columns.toArray),
        prep = IHPS.set(a),
        exec = IFPS.executeUpdate *> IFPS.getGeneratedKeys,
        chunkSize = chunkSize,
        loggingInfo = loggingInfoForUpdateWithGeneratedKeys(a)
      )

    /** Construct a program that performs the update, yielding a single set of generated keys of readable type `K`,
      * identified by the specified columns, given a writable argument `a`. Note that not all drivers support generated
      * keys, and some support only a single key column.
      * @group Execution
      */
    def withUniqueGeneratedKeys[K: Read](columns: String*)(a: A): ConnectionIO[K] =
      IHC.executeWithResultSet(
        prepareExecutionForWithUniqueGeneratedKeys(columns*)(a),
        loggingInfoForUpdateWithGeneratedKeys(a)
      )

    /** Just like `withUniqueGeneratedKeys` but allowing to alter `PreparedExecution`.
      */
    def withUniqueGeneratedKeysAlteringExecution[K: Read](columns: String*)(
        a: A,
        fn: PreparedExecution[K] => PreparedExecution[K]
    ): ConnectionIO[K] =
      IHC.executeWithResultSet(
        fn(prepareExecutionForWithUniqueGeneratedKeys(columns*)(a)),
        loggingInfoForUpdateWithGeneratedKeys(a)
      )

    private def prepareExecutionForWithUniqueGeneratedKeys[K: Read](columns: String*)(a: A): PreparedExecution[K] =
      PreparedExecution(
        create = IFC.prepareStatement(sql, columns.toArray),
        prep = IHPS.set(a),
        exec = IFPS.executeUpdate *> IFPS.getGeneratedKeys,
        process = IHRS.getUnique
      )

    private def loggingInfoForUpdateWithGeneratedKeys(a: A) =
      LoggingInfo(
        sql,
        Parameters.NonBatch(write.toList(a)),
        label
      )

    /** Update is a contravariant functor.
      * @group Transformations
      */
    def contramap[C](f: C => A): Update[C] =
      new Update[C] {
        val write: Write[C] = u.write.contramap(f)
        val sql: String = u.sql
        val pos: Option[Pos] = u.pos
        val label: String = u.label
      }

    /** Apply an argument, yielding a residual [[Update0]].
      * @group Transformations
      */
    def toUpdate0(a: A): Update0 =
      new Update0 {
        override val sql: String = u.sql
        override val pos: Option[Pos] = u.pos
        override def toFragment: Fragment = u.toFragment(a)
        override def analysis: ConnectionIO[Analysis] = u.analysis
        override def outputAnalysis: ConnectionIO[Analysis] = u.outputAnalysis
        override def run: ConnectionIO[Long] = u.run(a)
        override def runAlteringExecution(
            fn: PreparedExecutionWithoutProcessStep[Long] => PreparedExecutionWithoutProcessStep[Long]
        ): ConnectionIO[Long] =
          u.runAlteringExecution(a, fn)
        override def withGeneratedKeysWithChunkSize[K: Read](columns: String*)(chunkSize: Int)
            : Stream[ConnectionIO, K] =
          u.withGeneratedKeysWithChunkSize[K](columns*)(a, chunkSize)
        override def withUniqueGeneratedKeys[K: Read](columns: String*): ConnectionIO[K] =
          u.withUniqueGeneratedKeys(columns*)(a)
        override def withUniqueGeneratedKeysAlteringExecution[K: Read](columns: String*)(
            fn: PreparedExecution[K] => PreparedExecution[K]
        ): ConnectionIO[K] =
          u.withUniqueGeneratedKeysAlteringExecution(columns*)(a, fn)
        override def inspect[R](f: (String, PreparedStatementIO[Unit]) => ConnectionIO[R]): ConnectionIO[R] =
          u.inspect(a)(f)
      }

  }

  object Update {

    /** Construct an `Update` for some writable parameter type `A` with the given SQL string, and optionally a `Pos`
      * and/or `LogHandler` for diagnostics. The normal mechanism for construction is the `sql/fr/fr0` interpolators.
      * @group Constructors
      */
    def apply[A](sql: String, pos: Option[Pos] = None, label: String = unlabeled)(
        implicit W: Write[A]
    ): Update[A] = {
      val sql0 = sql
      val label0 = label
      val pos0 = pos
      new Update[A] {
        val write: Write[A] = W
        val sql: String = sql0
        val label: String = label0
        val pos: Option[Pos] = pos0
      }
    }

    /** Update is a contravariant functor.
      * @group Typeclass Instances
      */
    implicit val updateContravariant: Contravariant[Update] =
      new Contravariant[Update] {
        def contramap[A, B](fa: Update[A])(f: B => A): Update[B] = fa `contramap` f
      }

  }

  trait Update0 {

    /** The SQL string.
      * @group Diagnostics
      */
    val sql: String

    /** An optional [[doobie.util.pos.Pos]] indicating the source location where this [[Update0]] was constructed. This
      * is used only for diagnostic purposes.
      * @group Diagnostics
      */
    val pos: Option[Pos]

    /** Convert this Update0 to a `Fragment`. */
    def toFragment: Fragment

    /** Program to construct an analysis of this query's SQL statement and asserted parameter types.
      * @group Diagnostics
      */
    def analysis: ConnectionIO[Analysis]

    /** Program to construct an analysis of this query's SQL statement and result set column types.
      * @group Diagnostics
      */
    def outputAnalysis: ConnectionIO[Analysis]

    /** Program to construct an inspection of the query. Calls `f` with the SQL representation of the query and a
      * statement with all statement arguments set. Returns the result of the `ConnectionIO` program constructed.
      *
      * @group Diagnostics
      */
    def inspect[R](f: (String, PreparedStatementIO[Unit]) => ConnectionIO[R]): ConnectionIO[R]

    /** Program to execute the update and yield a count of affected rows.
      * @group Execution
      */
    def run: ConnectionIO[Long]

    def runAlteringExecution(
        fn: PreparedExecutionWithoutProcessStep[Long] => PreparedExecutionWithoutProcessStep[Long]
    ): ConnectionIO[Long]

    /** Construct a stream that performs the update, yielding generated keys of readable type `K`, identified by the
      * specified columns. Note that not all drivers support generated keys, and some support only a single key column.
      * @group Execution
      */
    def withGeneratedKeys[K: Read](columns: String*): Stream[ConnectionIO, K] =
      withGeneratedKeysWithChunkSize(columns*)(DefaultChunkSize)

    /** Construct a stream that performs the update, yielding generated keys of readable type `K`, identified by the
      * specified columns, given a `chunkSize`. Note that not all drivers support generated keys, and some support only
      * a single key column.
      * @group Execution
      */
    def withGeneratedKeysWithChunkSize[K: Read](columns: String*)(chunkSize: Int): Stream[ConnectionIO, K]

    /** Construct a program that performs the update, yielding a single set of generated keys of readable type `K`,
      * identified by the specified columns. Note that not all drivers support generated keys, and some support only a
      * single key column.
      * @group Execution
      */
    def withUniqueGeneratedKeys[K: Read](columns: String*): ConnectionIO[K]

    def withUniqueGeneratedKeysAlteringExecution[K: Read](columns: String*)(
        fn: PreparedExecution[K] => PreparedExecution[K]
    ): ConnectionIO[K]

  }

  object Update0 {

    /** Construct an `Update0` with the given SQL string, and optionally a `Pos` and/or `LogHandler` for diagnostics.
      * The normal mechanism for construction is the `sql/fr/fr0` interpolators.
      * @group Constructors
      */
    def apply(sql0: String, pos0: Option[Pos]): Update0 =
      Update[Unit](sql0, pos0).toUpdate0(())

  }

}
