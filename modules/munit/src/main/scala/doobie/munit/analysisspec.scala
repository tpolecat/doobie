// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.munit

import cats.effect.{IO, Async}
import doobie.munit.analysisspec.Checker.ErrorItems
import doobie.syntax.connectionio._
import doobie.util.query.{Query0, Query}
import doobie.util.testing._
import org.tpolecat.typename._
import munit.{Assertions, Location}

/** Module with a mix-in trait for specifications that enables checking of doobie `Query` and `Update` values.
  *
  * {{{
  * class ExampleSuite extends FunSuite with IOChecker {
  *
  *   // The transactor to use for the tests.
  *   val transactor = Transactor.fromDriverManager[IO](...)
  *
  *   // Now just mention the queries. Arguments are not used.
  *   test("findByNameAndAge") { check(MyDaoModule.findByNameAndAge(null, 0)) }
  *   test("allWoozles") { check(MyDaoModule.allWoozles) }
  *
  * }
  * }}}
  */
object analysisspec {

  trait Checker[M[_]] extends CheckerBase[M] { this: Assertions =>

    def check[A: Analyzable](a: A)(implicit loc: Location): Unit = checkImpl(Analyzable.unpack(a))

    def checkOutput[A: TypeName](q: Query0[A])(implicit loc: Location): Unit =
      checkImpl(AnalysisArgs(
        s"Query0[${typeName[A]}]",
        q.pos,
        q.sql,
        q.outputAnalysis
      ))

    def checkOutput[A: TypeName, B: TypeName](q: Query[A, B])(implicit loc: Location): Unit =
      checkImpl(AnalysisArgs(
        s"Query[${typeName[A]}, ${typeName[B]}]",
        q.pos,
        q.sql,
        q.outputAnalysis
      ))

    private def checkImpl(args: AnalysisArgs)(implicit loc: Location): Unit = {
      val report = U.unsafeRunSync(analyze(args).transact(transactor))
      if (!report.succeeded) {
        fail(
          message = formatReport(args, report, colors)
            .padLeft("  ")
            .toString,
          cause = ErrorItems(report.items.filter(_.error.isDefined))
        )
      }
    }
  }

  object Checker {
    case class ErrorItems(errors: List[AnalysisReport.Item]) extends Exception
  }

  /** Implementation of Checker[IO] */
  trait IOChecker extends Checker[IO] {
    self: Assertions =>
    import cats.effect.unsafe.implicits.global
    override implicit val M: Async[IO] = IO.asyncForIO
    override implicit val U: UnsafeRun[IO] = new UnsafeRun[IO] {
      def unsafeRunSync[A](ioa: IO[A]) = ioa.unsafeRunSync()
    }
  }
}
