// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.data.NonEmptyList
import cats.effect.kernel.Async
import cats.instances.int._
import cats.instances.list._
import cats.instances.string._
import cats.syntax.list._
import cats.syntax.applicativeError._
import cats.syntax.foldable._
import cats.syntax.show._
import doobie._
import doobie.implicits._
import doobie.util.analysis._
import doobie.util.pretty._
import doobie.util.pos.Pos
import scala.Predef.augmentString
import org.tpolecat.typename._

package testing {

  trait UnsafeRun[F[_]] {
    def unsafeRunSync[A](fa: F[A]): A
  }

  /** Common base trait for various checkers and matchers.
    */
  trait CheckerBase[M[_]] {
    // Effect type, required instances
    implicit def M: Async[M]
    implicit def U: UnsafeRun[M]
    def transactor: Transactor[M]
    def colors: Colors = Colors.Ansi
  }

  /** Common data for all query-like types. */
  final case class AnalysisArgs(
      typeName: String,
      pos: Option[Pos],
      sql: String,
      analysis: ConnectionIO[Analysis]
  ) {
    val cleanedSql = Block(
      sql.linesIterator
        .map(_.trim)
        .filterNot(_.isEmpty)
        .toList
    )

    private val location =
      pos
        .map(f => show"${f.file}:${f.line}")
        .getOrElse("(source location unknown)")

    val header: String = show"$typeName defined at $location"
  }

  /** Information from [[Analysis]], prepared for output. */
  final case class AnalysisReport(
      header: String,
      sql: Block,
      items: List[AnalysisReport.Item]
  ) {
    val succeeded: Boolean = items.forall(_.error.isEmpty)
  }

  object AnalysisReport {
    final case class Item(description: String, error: Option[Block])
  }

  /** Typeclass for query-like objects. */
  trait Analyzable[T] {
    def unpack(t: T): AnalysisArgs
  }

  object Analyzable {
    def apply[T](implicit ev: Analyzable[T]): Analyzable[T] = ev

    def unpack[T](t: T)(implicit T: Analyzable[T]): AnalysisArgs =
      T.unpack(t)

    def instance[T](
        impl: T => AnalysisArgs
    ): Analyzable[T] =
      new Analyzable[T] {
        def unpack(t: T) = impl(t)
      }

    implicit def analyzableQuery[A: TypeName, B: TypeName]: Analyzable[Query[A, B]] =
      instance { q =>
        AnalysisArgs(
          s"Query[${typeName[A]}, ${typeName[B]}]",
          q.pos,
          q.sql,
          q.analysis
        )
      }

    implicit def analyzableQuery0[A: TypeName]: Analyzable[Query0[A]] =
      instance { q =>
        AnalysisArgs(
          s"Query0[${typeName[A]}]",
          q.pos,
          q.sql,
          q.analysis
        )
      }

    implicit def analyzableUpdate[A: TypeName]: Analyzable[Update[A]] =
      instance { q =>
        AnalysisArgs(
          s"Update[${typeName[A]}]",
          q.pos,
          q.sql,
          q.analysis
        )
      }

    implicit val analyzableUpdate0: Analyzable[Update0] =
      instance { q =>
        AnalysisArgs(
          s"Update0",
          q.pos,
          q.sql,
          q.analysis
        )
      }
  }
}

/** Common utilities for query testing
  */
package object testing {

  def analyze(args: AnalysisArgs): ConnectionIO[AnalysisReport] =
    args.analysis.attempt
      .map(buildItems)
      .map { items =>
        AnalysisReport(
          args.header,
          args.cleanedSql,
          items
        )
      }

  private def alignmentErrorsToBlock(
      es: NonEmptyList[AlignmentError]
  ): Block =
    Block(es.toList.flatMap(_.msg.linesIterator))

  private def buildItems(
      input: Either[Throwable, Analysis]
  ): List[AnalysisReport.Item] = input match {
    case Left(e) =>
      List(AnalysisReport.Item(
        "SQL Compiles and TypeChecks",
        Some(Block.fromErrorMsgLines(e))
      ))
    case Right(a) =>
      AnalysisReport.Item("SQL Compiles and TypeChecks", None) ::
        (a.paramDescriptions ++ a.columnDescriptions)
          .map { case (s, es) =>
            AnalysisReport.Item(s, es.toNel.map(alignmentErrorsToBlock))
          }
  }

  /** Simple formatting for analysis results.
    */
  def formatReport(
      args: AnalysisArgs,
      report: AnalysisReport,
      colors: Colors
  ): Block = {
    val sql = args.cleanedSql
      .wrap(68)
      // SQL should use the default color
      .padLeft(colors.RESET.toString)
    val items = report.items.foldMap(formatItem(colors))
    Block.fromString(args.header)
      .above(sql)
      .above(items)
  }

  private def formatItem(colors: Colors): AnalysisReport.Item => Block = {
    case AnalysisReport.Item(desc, None) =>
      Block.fromString(s"${colors.GREEN}✓${colors.RESET} $desc")
    case AnalysisReport.Item(desc, Some(err)) =>
      Block.fromString(s"${colors.RED}✕${colors.RESET} $desc")
        // No color for error details - ScalaTest paints each line of failure
        // red by default.
        .above(err.wrap(66).padLeft("  "))
  }
}
