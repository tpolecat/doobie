// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats._
import cats.arrow.Profunctor
import cats.data.NonEmptyList
import cats.syntax.all._
import cats.effect.kernel.syntax.monadCancel._
import doobie._
import doobie.implicits._
import doobie.util.analysis.Analysis
import doobie.util.compat.FactoryCompat
import doobie.util.log.{LogEvent, ExecFailure, ProcessingFailure, Success}
import doobie.util.pos.Pos
import doobie.free.{preparedstatement => IFPS, resultset => IFRS}
import doobie.hi.{connection => IHC, preparedstatement => IHPS, resultset => IHRS}

import fs2.Stream
import scala.Predef.longWrapper
import scala.concurrent.duration.{FiniteDuration, NANOSECONDS}
import scala.collection.immutable.Map
import doobie.util.MultiVersionTypeSupport.=:=

/** Module defining queries parameterized by input and output types. */
object query {

  val DefaultChunkSize = 512

  /** A query parameterized by some input type `A` yielding values of type `B`. We define here the core operations that
    * are needed. Additional operations are provided on `[[Query0]]` which is the residual query after applying an `A`.
    * This is the type constructed by the `sql` interpolator.
    */
  trait Query[A, B] { outer =>

    protected implicit val write: Write[A]
    protected implicit val read: Read[B]

    private val now: PreparedStatementIO[Long] =
      IFPS.delay(System.nanoTime)

    // Equivalent to IHPS.executeQuery(k) but with logging
    private def executeQuery[T](a: A, k: ResultSetIO[T]): PreparedStatementIO[T] = {
      val args = write.toList(a)
      def diff(a: Long, b: Long) = FiniteDuration((a - b).abs, NANOSECONDS)
      def log(e: LogEvent): PreparedStatementIO[Unit] =
        for {
          _ <- IFPS.performLogging(e)
        } yield ()

      for {
        t0 <- now
        eet <- IFPS.executeQuery.flatMap(rs =>
          (for {
            t1 <- now
            et <- IFPS.embed(rs, k).attempt
            t2 <- now
          } yield (t1, et, t2)).guarantee(IFPS.embed(rs, IFRS.close))).attempt
        tuple <- eet.liftTo[PreparedStatementIO].onError { case e =>
          for {
            t1 <- now
            _ <- log(ExecFailure(sql, args, label, diff(t1, t0), e))
          } yield ()
        }
        (t1, et, t2) = tuple
        t <- et.liftTo[PreparedStatementIO].onError { case e =>
          log(ProcessingFailure(sql, args, label, diff(t1, t0), diff(t2, t1), e))
        }
        _ <- log(Success(sql, args, label, diff(t1, t0), diff(t2, t1)))
      } yield t
    }

    /** The SQL string.
      * @group Diagnostics
      */
    def sql: String

    /** An optional `[[Pos]]` indicating the source location where this `[[Query]]` was constructed. This is used only
      * for diagnostic purposes.
      * @group Diagnostics
      */
    def pos: Option[Pos]

    /** Convert this Query to a `Fragment`. */
    def toFragment(a: A): Fragment =
      write.toFragment(a, sql)

    /** Label to be used during logging */
    val label: String

    /** Program to construct an analysis of this query's SQL statement and asserted parameter and column types.
      * @group Diagnostics
      */
    def analysis: ConnectionIO[Analysis] =
      IHC.prepareQueryAnalysis[A, B](sql)

    /** Program to construct an analysis of this query's SQL statement and result set column types.
      * @group Diagnostics
      */
    def outputAnalysis: ConnectionIO[Analysis] =
      IHC.prepareQueryAnalysis0[B](sql)

    /** Program to construct an inspection of the query. Given arguments `a`, calls `f` with the SQL representation of
      * the query and a statement with all arguments set. Returns the result of the `ConnectionIO` program constructed.
      *
      * @group Diagnostics
      */
    def inspect[R](a: A)(f: (String, PreparedStatementIO[Unit]) => ConnectionIO[R]): ConnectionIO[R] =
      f(sql, IHPS.set(a))

    /** Apply the argument `a` to construct a `Stream` with the given chunking factor, with effect type
      * `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding elements of type `B`.
      * @group Results
      */
    def streamWithChunkSize(a: A, chunkSize: Int): Stream[ConnectionIO, B] =
      IHC.stream[B](sql, IHPS.set(a), chunkSize)

    /** Apply the argument `a` to construct a `Stream` with `DefaultChunkSize`, with effect type
      * `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding elements of type `B`.
      * @group Results
      */
    def stream(a: A): Stream[ConnectionIO, B] =
      streamWithChunkSize(a, DefaultChunkSize)

    /** Apply the argument `a` to construct a program in `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding
      * an `F[B]` accumulated via the provided `CanBuildFrom`. This is the fastest way to accumulate a collection.
      * @group Results
      */
    def to[F[_]](a: A)(implicit f: FactoryCompat[B, F[B]]): ConnectionIO[F[B]] =
      IHC.prepareStatement(sql)(IHPS.set(a) *> executeQuery(a, IHRS.build[F, B]))

    /** Apply the argument `a` to construct a program in `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding
      * an `Map[(K, V)]` accumulated via the provided `CanBuildFrom`. This is the fastest way to accumulate a
      * collection. this function can call only when B is (K, V).
      * @group Results
      */
    def toMap[K, V](a: A)(implicit ev: B =:= (K, V), f: FactoryCompat[(K, V), Map[K, V]]): ConnectionIO[Map[K, V]] =
      IHC.prepareStatement(sql)(IHPS.set(a) *> executeQuery(a, IHRS.buildPair[Map, K, V](f, read.map(ev))))

    /** Apply the argument `a` to construct a program in `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding
      * an `F[B]` accumulated via `MonadPlus` append. This method is more general but less efficient than `to`.
      * @group Results
      */
    def accumulate[F[_]: Alternative](a: A): ConnectionIO[F[B]] =
      IHC.prepareStatement(sql)(IHPS.set(a) *> executeQuery(a, IHRS.accumulate[F, B]))

    /** Apply the argument `a` to construct a program in `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding
      * a unique `B` and raising an exception if the resultset does not have exactly one row. See also `option`.
      * @group Results
      */
    def unique(a: A): ConnectionIO[B] =
      IHC.prepareStatement(sql)(IHPS.set(a) *> executeQuery(a, IHRS.getUnique[B]))

    /** Apply the argument `a` to construct a program in `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding
      * an optional `B` and raising an exception if the resultset has more than one row. See also `unique`.
      * @group Results
      */
    def option(a: A): ConnectionIO[Option[B]] =
      IHC.prepareStatement(sql)(IHPS.set(a) *> executeQuery(a, IHRS.getOption[B]))

    /** Apply the argument `a` to construct a program in `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding
      * an `NonEmptyList[B]` and raising an exception if the resultset does not have at least one row. See also
      * `unique`.
      * @group Results
      */
    def nel(a: A): ConnectionIO[NonEmptyList[B]] =
      IHC.prepareStatement(sql)(IHPS.set(a) *> executeQuery(a, IHRS.nel[B]))

    /** @group Transformations */
    def map[C](f: B => C): Query[A, C] =
      new Query[A, C] {
        val write = outer.write
        val read = outer.read.map(f)
        def sql = outer.sql
        def pos = outer.pos
        val label = outer.label
      }

    /** @group Transformations */
    def contramap[C](f: C => A): Query[C, B] =
      new Query[C, B] {
        val write = outer.write.contramap(f)
        val read = outer.read
        def sql = outer.sql
        def pos = outer.pos
        val label = outer.label
      }

    /** Apply an argument, yielding a residual `[[Query0]]`.
      * @group Transformations
      */
    @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
    def toQuery0(a: A): Query0[B] =
      new Query0[B] {
        def sql = outer.sql
        def pos = outer.pos
        def toFragment = outer.toFragment(a)
        def analysis = outer.analysis
        def outputAnalysis = outer.outputAnalysis
        def streamWithChunkSize(n: Int) = outer.streamWithChunkSize(a, n)
        def accumulate[F[_]: Alternative] = outer.accumulate[F](a)
        def to[F[_]](implicit f: FactoryCompat[B, F[B]]) = outer.to[F](a)
        def toMap[K, V](implicit ev: B =:= (K, V), f: FactoryCompat[(K, V), Map[K, V]]) = outer.toMap(a)
        def unique = outer.unique(a)
        def option = outer.option(a)
        def nel = outer.nel(a)
        def map[C](f: B => C): Query0[C] = outer.map(f).toQuery0(a)
        def inspect[R](f: (String, PreparedStatementIO[Unit]) => ConnectionIO[R]) = outer.inspect(a)(f)
      }

  }

  object Query {

    /** Construct a `Query` with the given SQL string, an optional `Pos` for diagnostic purposes, and type arguments for
      * writable input and readable output types. Note that the most common way to construct a `Query` is via the `sql`
      * interpolator.
      * @group Constructors
      */
    @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
    def apply[A, B](sql: String, pos: Option[Pos] = None, label: String = unlabeled)(implicit
        A: Write[A],
        B: Read[B]
    ): Query[A, B] = {
      val sql0 = sql
      val label0 = label
      val pos0 = pos
      new Query[A, B] {
        val write = A
        val read = B
        val sql = sql0
        val pos = pos0
        val label = label0
      }
    }

    /** @group Typeclass Instances */
    implicit val queryProfunctor: Profunctor[Query] =
      new Profunctor[Query] {
        def dimap[A, B, C, D](fab: Query[A, B])(f: C => A)(g: B => D): Query[C, D] =
          fab.contramap(f).map(g)
      }

    /** @group Typeclass Instances */
    implicit def queryCovariant[A]: Functor[Query[A, *]] =
      new Functor[Query[A, *]] {
        def map[B, C](fa: Query[A, B])(f: B => C): Query[A, C] =
          fa.map(f)
      }

    /** @group Typeclass Instances */
    implicit def queryContravariant[B]: Contravariant[Query[*, B]] =
      new Contravariant[Query[*, B]] {
        def contramap[A, C](fa: Query[A, B])(f: C => A): Query[C, B] =
          fa.contramap(f)
      }

  }

  /** An abstract query closed over its input arguments and yielding values of type `B`, without a specified
    * disposition. Methods provided on `[[Query0]]` allow the query to be interpreted as a stream or program in
    * `CollectionIO`.
    */
  trait Query0[B] { outer =>

    /** The SQL string.
      * @group Diagnostics
      */
    def sql: String

    /** An optional `Pos` indicating the source location where this `Query` was constructed. This is used only for
      * diagnostic purposes.
      * @group Diagnostics
      */
    def pos: Option[Pos]

    /** Program to construct an analysis of this query's SQL statement and asserted parameter and column types.
      * @group Diagnostics
      */
    def analysis: ConnectionIO[Analysis]

    /** Convert this Query0 to a `Fragment`. */
    def toFragment: Fragment

    /** Program to construct an inspection of the query. Calls `f` with the SQL representation of the query and a
      * statement with all statement arguments set. Returns the result of the `ConnectionIO` program constructed.
      *
      * @group Diagnostics
      */
    def inspect[R](f: (String, PreparedStatementIO[Unit]) => ConnectionIO[R]): ConnectionIO[R]

    /** Program to construct an analysis of this query's SQL statement and result set column types.
      * @group Diagnostics
      */
    def outputAnalysis: ConnectionIO[Analysis]

    /** `Stream` with default chunk factor, with effect type `[[doobie.free.connection.ConnectionIO ConnectionIO]]`
      * yielding elements of type `B`.
      * @group Results
      */
    def stream: Stream[ConnectionIO, B] =
      streamWithChunkSize(DefaultChunkSize)

    /** `Stream` with given chunk factor, with effect type `[[doobie.free.connection.ConnectionIO ConnectionIO]]`
      * yielding elements of type `B`.
      * @group Results
      */
    def streamWithChunkSize(n: Int): Stream[ConnectionIO, B]

    /** Program in `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding an `F[B]` accumulated via the
      * provided `CanBuildFrom`. This is the fastest way to accumulate a collection.
      * @group Results
      */
    def to[F[_]](implicit f: FactoryCompat[B, F[B]]): ConnectionIO[F[B]]

    /** Apply the argument `a` to construct a program in `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding
      * an `Map[(K, V)]` accumulated via the provided `CanBuildFrom`. This is the fastest way to accumulate a
      * collection. this function can call only when B is (K, V).
      * @group Results
      */
    def toMap[K, V](implicit ev: B =:= (K, V), f: FactoryCompat[(K, V), Map[K, V]]): ConnectionIO[Map[K, V]]

    /** Program in `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding an `F[B]` accumulated via `MonadPlus`
      * append. This method is more general but less efficient than `to`.
      * @group Results
      */
    def accumulate[F[_]: Alternative]: ConnectionIO[F[B]]

    /** Program in `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding a unique `B` and raising an exception
      * if the resultset does not have exactly one row. See also `option`.
      * @group Results
      */
    def unique: ConnectionIO[B]

    /** Program in `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding an optional `B` and raising an
      * exception if the resultset has more than one row. See also `unique`.
      * @group Results
      */
    def option: ConnectionIO[Option[B]]

    /** Program in `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding a `NonEmptyList[B]` and raising an
      * exception if the resultset does not have at least one row. See also `unique`.
      * @group Results
      */
    def nel: ConnectionIO[NonEmptyList[B]]

    /** @group Transformations */
    def map[C](f: B => C): Query0[C]

  }

  object Query0 {

    /** Construct a `Query` with the given SQL string, an optional `Pos` for diagnostic purposes, with no parameters.
      * Note that the most common way to construct a `Query` is via the `sql`interpolator.
      * @group Constructors
      */
    @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
    def apply[A: Read](sql: String, pos: Option[Pos] = None, label: String = unlabeled): Query0[A] =
      Query[Unit, A](sql, pos, label).toQuery0(())

    /** @group Typeclass Instances */
    implicit val queryFunctor: Functor[Query0] =
      new Functor[Query0] {
        def map[A, B](fa: Query0[A])(f: A => B) = fa map f
      }

  }

}
