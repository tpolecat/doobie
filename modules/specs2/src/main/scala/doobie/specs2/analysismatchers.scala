// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.specs2

import cats.effect.{Async, IO}
import cats.instances.list._
import cats.syntax.foldable._
import doobie.syntax.connectionio._
import doobie.util.pretty._
import doobie.util.testing.{AnalysisReport, Analyzable, analyze, CheckerBase, UnsafeRun}
import org.specs2.matcher.{Expectable, Matcher, MatchResult}

object analysismatchers {

  /** Provides matcher syntax for query checking:
    *
    * {{{
    * sql"select 1".query[Int] must typecheck
    * }}}
    */
  trait AnalysisMatchers[F[_]] extends CheckerBase[F] {

    def typecheck[T](implicit analyzable: Analyzable[T]): Matcher[T] =
      new Matcher[T] {
        def apply[S <: T](t: Expectable[S]): MatchResult[S] = {
          val report = U.unsafeRunSync(
            analyze(
              analyzable.unpack(t.value)
            ).transact(transactor)
          )
          reportToMatchResult(report, t)
        }
      }

    private def reportToMatchResult[S](
        r: AnalysisReport,
        s: Expectable[S]
    ): MatchResult[S] = {
      // We aim to produce the same format the fragment version does.

      val items = r.items.foldMap(itemToBlock)

      @SuppressWarnings(Array("org.wartremover.warts.ToString"))
      val message =
        Block.fromString(r.header)
          .above(Block.fromString(""))
          .above(r.sql.wrap(70).padLeft("  "))
          .above(Block.fromString(""))
          .above(items)
          .toString

      Matcher.result(r.succeeded, message, message, s)
    }

    private def itemToBlock(item: AnalysisReport.Item): Block =
      item.error match {
        case None =>
          Block.fromString(s"+ ${item.description}")
        case Some(e) =>
          Block.fromString(s"x ${item.description}").above(
            Block.fromString(" x ").leftOf(e.wrap(70))
          )
      }
  }

  trait IOAnalysisMatchers extends AnalysisMatchers[IO] {
    import cats.effect.unsafe.implicits.global
    override implicit val M: Async[IO] = IO.asyncForIO
    override implicit val U: UnsafeRun[IO] = new UnsafeRun[IO] {
      def unsafeRunSync[A](ioa: IO[A]) = ioa.unsafeRunSync()
    }
  }
}
