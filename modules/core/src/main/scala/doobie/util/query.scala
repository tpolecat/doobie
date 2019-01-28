// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats._
import cats.arrow.Profunctor
import cats.data.NonEmptyList
import cats.implicits._
import com.github.ghik.silencer.silent
import cats.effect.syntax.bracket._
import doobie._
import doobie.implicits.AsyncPreparedStatementIO
import doobie.util.analysis.Analysis
import doobie.util.compat.FactoryCompat
import doobie.util.log.{ LogEvent, ExecFailure, ProcessingFailure, Success }
import doobie.util.pos.Pos
import fs2.Stream
import scala.Predef.longWrapper
import scala.concurrent.duration.{ FiniteDuration, NANOSECONDS }

/** Module defining queries parameterized by input and output types. */
object query {

  val DefaultChunkSize = 512

  /**
   * A query parameterized by some input type `A` yielding values of type `B`. We define here the
   * core operations that are needed. Additional operations are provided on `[[Query0]]` which is the
   * residual query after applying an `A`. This is the type constructed by the `sql` interpolator.
   */
  trait Query[A, B] { outer =>

    protected implicit val write: Write[A]
    protected implicit val read: Read[B]

    // LogHandler is protected for now.
    @silent // deprecation
    protected val logHandler: LogHandler

    private val now: PreparedStatementIO[Long] =
      FPS.delay(System.nanoTime)

    // Equivalent to HPS.executeQuery(k) but with logging
    @silent // deprecation
    private def executeQuery[T](a: A, k: ResultSetIO[T]): PreparedStatementIO[T] = {
      val args = write.toList(a)
      def diff(a: Long, b: Long) = FiniteDuration((a - b).abs, NANOSECONDS)
      def log(e: LogEvent) = FPS.delay(logHandler.unsafeRun(e))
      for {
        t0 <- now
        eet <- FPS.executeQuery.bracket(rs => for {
          t1 <- now
          et <- FPS.embed(rs, k).attempt
          t2 <- now
        } yield (t1, et, t2))(FPS.embed(_, FRS.close)).attempt
        tuple <- eet.liftTo[PreparedStatementIO].onError { case e =>
          for {
            t1 <- now
            _ <- log(ExecFailure(sql, args, diff(t1, t0), e))
          } yield ()
        }
        (t1, et, t2) = tuple
        t <- et.liftTo[PreparedStatementIO].onError { case e => log(ProcessingFailure(sql, args, diff(t1, t0), diff(t2, t1), e)) }
        _  <- log(Success(sql, args, diff(t1, t0), diff(t2, t1)))
      } yield t
    }

    /**
     * The SQL string.
     * @group Diagnostics
     */
    def sql: String

    /**
     * An optional `[[Pos]]` indicating the source location where this `[[Query]]` was
     * constructed. This is used only for diagnostic purposes.
     * @group Diagnostics
     */
    def pos: Option[Pos]

    /**
     * Program to construct an analysis of this query's SQL statement and asserted parameter and
     * column types.
     * @group Diagnostics
     */
    def analysis: ConnectionIO[Analysis] =
      HC.prepareQueryAnalysis[A, B](sql)

    /**
     * Program to construct an analysis of this query's SQL statement and result set column types.
     * @group Diagnostics
     */
    def outputAnalysis: ConnectionIO[Analysis] =
      HC.prepareQueryAnalysis0[B](sql)

    /**
     * Apply the argument `a` to construct a `Stream` with the given chunking factor, with
     * effect type  `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding elements of
     * type `B`.
     * @group Results
     */
    def streamWithChunkSize(a: A, chunkSize: Int): Stream[ConnectionIO, B] =
      HC.stream[B](sql, HPS.set(a), chunkSize)

    /**
     * Apply the argument `a` to construct a `Stream` with `DefaultChunkSize`, with
     * effect type  `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding elements of
     * type `B`.
     * @group Results
     */
    def stream(a: A): Stream[ConnectionIO, B] =
      streamWithChunkSize(a, DefaultChunkSize)

    /**
     * Apply the argument `a` to construct a program in
     *`[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding an `F[B]` accumulated
     * via the provided `CanBuildFrom`. This is the fastest way to accumulate a collection.
     * @group Results
     */
    def to[F[_]](a: A)(implicit f: FactoryCompat[B, F[B]]): ConnectionIO[F[B]] =
      HC.prepareStatement(sql)(HPS.set(a) *> executeQuery(a, HRS.build[F,B]))

    /**
     * Apply the argument `a` to construct a program in
     * `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding an `F[B]` accumulated
     * via `MonadPlus` append. This method is more general but less efficient than `to`.
     * @group Results
     */
    def accumulate[F[_]: Alternative](a: A): ConnectionIO[F[B]] =
      HC.prepareStatement(sql)(HPS.set(a) *> executeQuery(a, HRS.accumulate[F, B]))

    /**
     * Apply the argument `a` to construct a program in
     * `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding a unique `B` and
     * raising an exception if the resultset does not have exactly one row. See also `option`.
     * @group Results
     */
    def unique(a: A): ConnectionIO[B] =
      HC.prepareStatement(sql)(HPS.set(a) *> executeQuery(a, HRS.getUnique[B]))

    /**
     * Apply the argument `a` to construct a program in
     * `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding an optional `B` and
     * raising an exception if the resultset has more than one row. See also `unique`.
     * @group Results
     */
    def option(a: A): ConnectionIO[Option[B]] =
      HC.prepareStatement(sql)(HPS.set(a) *> executeQuery(a, HRS.getOption[B]))

    /**
      * Apply the argument `a` to construct a program in
      * `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding an `NonEmptyList[B]` and
      * raising an exception if the resultset does not have at least one row. See also `unique`.
      * @group Results
      */
    def nel(a: A): ConnectionIO[NonEmptyList[B]] =
      HC.prepareStatement(sql)(HPS.set(a) *> executeQuery(a, HRS.nel[B]))

    /** @group Transformations */
    def map[C](f: B => C): Query[A, C] =
      new Query[A, C] {
        val write = outer.write
        val read  = outer.read.map(f)
        def sql = outer.sql
        def pos = outer.pos
        val logHandler = outer.logHandler
      }

    /** @group Transformations */
    def contramap[C](f: C => A): Query[C, B] =
      new Query[C, B] {
        val write = outer.write.contramap(f)
        val read  = outer.read
        def sql = outer.sql
        def pos = outer.pos
        val logHandler = outer.logHandler
      }

    /**
     * Apply an argument, yielding a residual `[[Query0]]`.
     * @group Transformations
     */
    @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
    def toQuery0(a: A): Query0[B] =
      new Query0[B] {
        def sql = outer.sql
        def pos = outer.pos
        // def toFragment = outer.toFragment(a)
        def analysis = outer.analysis
        def outputAnalysis = outer.outputAnalysis
        def streamWithChunkSize(n: Int) = outer.streamWithChunkSize(a, n)
        def accumulate[F[_]: Alternative] = outer.accumulate[F](a)
        def to[F[_]](implicit f: FactoryCompat[B, F[B]]) = outer.to[F](a)
        def unique = outer.unique(a)
        def option = outer.option(a)
        def nel = outer.nel(a)
        def map[C](f: B => C): Query0[C] = outer.map(f).toQuery0(a)
      }

  }

  object Query {

    /**
     * Construct a `Query` with the given SQL string, an optional `Pos` for diagnostic
     * purposes, and type arguments for writable input and readable output types. Note that the
     * most common way to construct a `Query` is via the `sql` interpolator.
     * @group Constructors
     */
    @silent // deprecation
    @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
    def apply[A, B](sql0: String, pos0: Option[Pos] = None, logHandler0: LogHandler = LogHandler.nop)(implicit A: Write[A], B: Read[B]): Query[A, B] =
      new Query[A, B] {
        val write = A
        val read = B
        val sql = sql0
        val pos = pos0
        val logHandler = logHandler0
      }

    /** @group Typeclass Instances */
    implicit val queryProfunctor: Profunctor[Query] =
      new Profunctor[Query] {
        def dimap[A, B, C, D](fab: Query[A,B])(f: C => A)(g: B => D): Query[C,D] =
          fab.contramap(f).map(g)
      }

    /** @group Typeclass Instances */
    implicit def queryCovariant[A]: Functor[Query[A, ?]] =
      new Functor[Query[A, ?]] {
        def map[B, C](fa: Query[A, B])(f: B => C): Query[A, C] =
          fa.map(f)
      }

    /** @group Typeclass Instances */
    implicit def queryContravariant[B]: Contravariant[Query[?, B]] =
      new Contravariant[Query[?, B]] {
        def contramap[A, C](fa: Query[A, B])(f: C => A): Query[C, B] =
          fa.contramap(f)
      }

  }

  /**
   * An abstract query closed over its input arguments and yielding values of type `B`, without a
   * specified disposition. Methods provided on `[[Query0]]` allow the query to be interpreted as a
   * stream or program in `CollectionIO`.
   */
  trait Query0[B] { outer =>

    /**
     * The SQL string.
     * @group Diagnostics
     */
    def sql: String

    /**
     * An optional `Pos` indicating the source location where this `Query` was
     * constructed. This is used only for diagnostic purposes.
     * @group Diagnostics
     */
    def pos: Option[Pos]

    /**
     * Program to construct an analysis of this query's SQL statement and asserted parameter and
     * column types.
     * @group Diagnostics
     */
    def analysis: ConnectionIO[Analysis]

    /**
     * Program to construct an analysis of this query's SQL statement and result set column types.
     * @group Diagnostics
     */
    def outputAnalysis: ConnectionIO[Analysis]

    /**
     * `Stream` with default chunk factor, with effect type
     * `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding  elements of type `B`.
     * @group Results
     */
    def stream : Stream[ConnectionIO, B] =
      streamWithChunkSize(DefaultChunkSize)

    /**
     * `Stream` with given chunk factor, with effect type
     * `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding  elements of type `B`.
     * @group Results
     */
    def streamWithChunkSize(n: Int): Stream[ConnectionIO, B]

    /**
     * Program in `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding an `F[B]`
     * accumulated via the provided `CanBuildFrom`. This is the fastest way to accumulate a
     * collection.
     * @group Results
     */
    def to[F[_]](implicit f: FactoryCompat[B, F[B]]): ConnectionIO[F[B]]

    /**
     * Program in `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding an `F[B]`
     * accumulated via `MonadPlus` append. This method is more general but less efficient than `to`.
     * @group Results
     */
    def accumulate[F[_]: Alternative]: ConnectionIO[F[B]]

    /**
     * Program in `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding a unique `B` and
     * raising an exception if the resultset does not have exactly one row. See also `option`.
     * @group Results
     */
    def unique: ConnectionIO[B]

    /**
     * Program in `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding an optional `B`
     * and raising an exception if the resultset has more than one row. See also `unique`.
     * @group Results
     */
    def option: ConnectionIO[Option[B]]

    /**
      * Program in `[[doobie.free.connection.ConnectionIO ConnectionIO]]` yielding a `NonEmptyList[B]`
      * and raising an exception if the resultset does not have at least one row. See also `unique`.
      * @group Results
      */
    def nel: ConnectionIO[NonEmptyList[B]]

    /** @group Transformations */
    def map[C](f: B => C): Query0[C]

    /**
     * Convenience method, equivalent to `stream.evalMap(f).compile.drain`.
     * @group Results
     */
    @deprecated("use .stream.evalMap(f).compile.drain", "0.6.0")
    def sink(f: B => ConnectionIO[Unit]): ConnectionIO[Unit] =
      stream.evalMap(f).compile.drain

  }

  object Query0 {

    /**
     * Construct a `Query` with the given SQL string, an optional `Pos` for diagnostic
     * purposes, with no parameters. Note that the most common way to construct a `Query` is via the
     * `sql`interpolator.
     * @group Constructors
     */
    @silent // deprecation
    @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
    def apply[A: Read](sql: String, pos: Option[Pos] = None, logHandler: LogHandler = LogHandler.nop): Query0[A] =
      Query[Unit, A](sql, pos, logHandler).toQuery0(())

    /** @group Typeclass Instances */
    implicit val queryFunctor: Functor[Query0] =
      new Functor[Query0] {
        def map[A, B](fa: Query0[A])(f: A => B) = fa map f
      }

  }

}
