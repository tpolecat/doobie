// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats._
import cats.syntax.all._
import doobie._
import doobie.implicits._
import doobie.util.analysis.Analysis
import doobie.util.log.{Success, ExecFailure, LogEvent}
import doobie.util.pos.Pos
import doobie.free.{preparedstatement => IFPS}
import doobie.hi.{connection => IHC, preparedstatement => IHPS}
import fs2.Stream
import scala.Predef.longWrapper
import scala.concurrent.duration.{FiniteDuration, NANOSECONDS}

/** Module defining updates parameterized by input type. */
object update {

  val DefaultChunkSize = query.DefaultChunkSize

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

    private val now: PreparedStatementIO[Long] =
      IFPS.delay(System.nanoTime)

    // Equivalent to HPS.executeUpdate(k) but with logging if logHandler is defined
    private def executeUpdate[T](a: A): PreparedStatementIO[Int] = {
      val args = write.toList(a)
      def diff(a: Long, b: Long) = FiniteDuration((a - b).abs, NANOSECONDS)
      def log(e: LogEvent): PreparedStatementIO[Unit] =
        for {
          _ <- IFPS.performLogging(e)
        } yield ()

      for {
        t0 <- now
        en <- IFPS.executeUpdate.attempt
        t1 <- now
        n <- en.liftTo[PreparedStatementIO].onError { case e => log(ExecFailure(sql, args, label, diff(t1, t0), e)) }
        _ <- log(Success(sql, args, label, diff(t1, t0), FiniteDuration(0L, NANOSECONDS)))
      } yield n
    }

    /** The SQL string.
      * @group Diagnostics
      */
    val sql: String

    /** An optional `[[Pos]]` indicating the source location where this `[[Update]]` was constructed. This is used only
      * for diagnostic purposes.
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
    def run(a: A): ConnectionIO[Int] =
      IHC.prepareStatement(sql)(IHPS.set(a) *> executeUpdate(a))

    /** Add many sets of parameters and execute as a batch update, returning total rows updated. Note that when an error
      * occurred while executing the batch, your JDBC driver may decide to continue executing the rest of the batch
      * instead of raising a `BatchUpdateException`. Please refer to your JDBC driver's documentation for its exact
      * behaviour. See
      * [[https://docs.oracle.com/en/java/javase/11/docs/api/java.sql/java/sql/Statement.html#executeBatch()]] for more
      * information
      * @group Execution
      */
    def updateMany[F[_]: Foldable](fa: F[A]): ConnectionIO[Int] =
      IHC.prepareStatement(sql)(IHPS.addBatchesAndExecute(fa))

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
          IHC.updateManyWithGeneratedKeys[List, A, K](columns.toList)(sql, IFPS.unit, as.toList, chunkSize)
      }

    /** Construct a stream that performs the update, yielding generated keys of readable type `K`, identified by the
      * specified columns, given a writable argument `a`. Note that not all drivers support generated keys, and some
      * support only a single key column.
      * @group Execution
      */
    def withGeneratedKeys[K: Read](columns: String*)(a: A): Stream[ConnectionIO, K] =
      withGeneratedKeysWithChunkSize[K](columns: _*)(a, DefaultChunkSize)

    /** Construct a stream that performs the update, yielding generated keys of readable type `K`, identified by the
      * specified columns, given a writable argument `a` and `chunkSize`. Note that not all drivers support generated
      * keys, and some support only a single key column.
      * @group Execution
      */
    def withGeneratedKeysWithChunkSize[K: Read](columns: String*)(a: A, chunkSize: Int): Stream[ConnectionIO, K] =
      IHC.updateWithGeneratedKeys[K](columns.toList)(sql, IHPS.set(a), chunkSize)

    /** Construct a program that performs the update, yielding a single set of generated keys of readable type `K`,
      * identified by the specified columns, given a writable argument `a`. Note that not all drivers support generated
      * keys, and some support only a single key column.
      * @group Execution
      */
    def withUniqueGeneratedKeys[K: Read](columns: String*)(a: A): ConnectionIO[K] =
      IHC.prepareStatementS(sql, columns.toList)(IHPS.set(a) *> IHPS.executeUpdateWithUniqueGeneratedKeys)

    /** Update is a contravariant functor.
      * @group Transformations
      */
    def contramap[C](f: C => A): Update[C] =
      new Update[C] {
        val write = u.write.contramap(f)
        val sql = u.sql
        val pos = u.pos
        val label = u.label
      }

    /** Apply an argument, yielding a residual `[[Update0]]`.
      * @group Transformations
      */
    def toUpdate0(a: A): Update0 =
      new Update0 {
        val sql = u.sql
        val pos = u.pos
        def toFragment: Fragment = u.toFragment(a)
        def analysis = u.analysis
        def outputAnalysis = u.outputAnalysis
        def run = u.run(a)
        def withGeneratedKeysWithChunkSize[K: Read](columns: String*)(chunkSize: Int) =
          u.withGeneratedKeysWithChunkSize[K](columns: _*)(a, chunkSize)
        def withUniqueGeneratedKeys[K: Read](columns: String*) =
          u.withUniqueGeneratedKeys(columns: _*)(a)
        def inspect[R](f: (String, PreparedStatementIO[Unit]) => ConnectionIO[R]) = u.inspect(a)(f)
      }

  }

  object Update {

    /** Construct an `Update` for some writable parameter type `A` with the given SQL string, and optionally a `Pos`
      * and/or `LogHandler` for diagnostics. The normal mechanism for construction is the `sql/fr/fr0` interpolators.
      * @group Constructors
      */
    @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
    def apply[A](sql: String, pos: Option[Pos] = None, label: String = unlabeled)(
        implicit W: Write[A]
    ): Update[A] = {
      val sql0 = sql
      val label0 = label
      val pos0 = pos
      new Update[A] {
        val write = W
        val sql = sql0
        val label = label0
        val pos = pos0
      }
    }

    /** Update is a contravariant functor.
      * @group Typeclass Instances
      */
    implicit val updateContravariant: Contravariant[Update] =
      new Contravariant[Update] {
        def contramap[A, B](fa: Update[A])(f: B => A) = fa contramap f
      }

  }

  trait Update0 {

    /** The SQL string.
      * @group Diagnostics
      */
    val sql: String

    /** An optional `[[Pos]]` indicating the source location where this `[[Query]]` was constructed. This is used only
      * for diagnostic purposes.
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
    def run: ConnectionIO[Int]

    /** Construct a stream that performs the update, yielding generated keys of readable type `K`, identified by the
      * specified columns. Note that not all drivers support generated keys, and some support only a single key column.
      * @group Execution
      */
    def withGeneratedKeys[K: Read](columns: String*): Stream[ConnectionIO, K] =
      withGeneratedKeysWithChunkSize(columns: _*)(DefaultChunkSize)

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
