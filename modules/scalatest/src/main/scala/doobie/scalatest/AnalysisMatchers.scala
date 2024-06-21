// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.scalatest

import cats.effect.{Async, IO}
import doobie.syntax.connectionio._
import doobie.util.testing._
import org.scalatest.matchers.{Matcher, MatchResult, LazyArg}
import org.scalatest.matchers.dsl.MatcherFactory2
import scala.reflect.ClassTag

/** Provides matcher syntax for query checking:
  *
  * {{{
  * sql"select 1".query[Int] must typecheck
  * }}}
  */
trait AnalysisMatchers[F[_]] extends CheckerBase[F] {

  val typecheck: MatcherFactory2[Any, Analyzable, ClassTag] =
    new MatcherFactory2[Any, Analyzable, ClassTag] {
      override def matcher[T: Analyzable: ClassTag]: Matcher[T] =
        Matcher(matchTypecheck[T](_))
    }

  private def matchTypecheck[T](t: T)(
      implicit analyzable: Analyzable[T]
  ): MatchResult = {
    val args = analyzable.unpack(t)
    val report = U.unsafeRunSync(analyze(args).transact(transactor))

    MatchResult(
      report.succeeded,
      s"${args.typeName} failed to typecheck:\n{0}",
      s"${args.typeName} did typecheck:\n{0}",
      Vector(
        // Avoid formatting if the check performed as expected
        LazyArg(()) { _ =>
          formatReport(args, report, colors).padLeft("  ").toString
        }
      )
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
