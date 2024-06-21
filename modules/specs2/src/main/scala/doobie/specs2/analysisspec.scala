// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.specs2

import cats.effect.{Async, IO}
import doobie.{Update, Update0}
import doobie.syntax.connectionio._
import doobie.util.query.{Query, Query0}
import doobie.util.testing._
import org.specs2.mutable.Specification
import org.specs2.specification.core.{Fragment, Fragments}
import org.specs2.specification.create.{FormattingFragments => Format}
import org.specs2.specification.dsl.Online._
import org.tpolecat.typename._

/** Module with a mix-in trait for specifications that enables checking of doobie `Query` and `Update` values.
  * {{{
  * // An example specification, taken from the examples project.
  * class AnalysisTestSpec extends Specification with AnalysisSpec {
  *
  *   // The transactor to use for the tests.
  *   val transactor = Transactor.fromDriverManager[IO](...)
  *
  *   // Now just mention the queries. Arguments are not used.
  *   check(MyDaoModule.findByNameAndAge(null, 0))
  *   check(MyDaoModule.allWoozles)
  *
  * }
  * }}}
  */
object analysisspec {

  trait Checker[M[_]] extends CheckerBase[M] { this: Specification =>

    def check[A: Analyzable](a: A): Fragments =
      checkImpl(Analyzable.unpack(a))

    def checkOutput[A: TypeName](q: Query0[A]): Fragments =
      checkImpl(AnalysisArgs(
        s"Query0[${typeName[A]}]",
        q.pos,
        q.sql,
        q.outputAnalysis
      ))

    def checkOutput[A: TypeName, B: TypeName](q: Query[A, B]): Fragments =
      checkImpl(AnalysisArgs(
        s"Query[${typeName[A]}, ${typeName[B]}]",
        q.pos,
        q.sql,
        q.outputAnalysis
      ))

    def checkOutput[A: TypeName](u: Update[A]): Fragments =
      checkImpl(AnalysisArgs(
        s"Update[${typeName[A]}]",
        u.pos,
        u.sql,
        u.analysis
      ))

    def checkOutput(u: Update0): Fragments =
      checkImpl(AnalysisArgs(
        "Update0",
        u.pos,
        u.sql,
        u.analysis
      ))

    private def checkImpl(args: AnalysisArgs): Fragments =
      // continuesWith is necessary to make sure the query doesn't run too early
      s"${args.header}\n\n${args.cleanedSql.padLeft("  ").toString}\n" >> ok.continueWith {
        val report = U.unsafeRunSync(analyze(args).transact(transactor))
        indentBlock(
          report.items.map { item =>
            item.description ! item.error.fold(ok) {
              err => ko(err.wrap(70).toString)
            }
          }
        )
      }

    private def indentBlock(fs: Seq[Fragment]): Fragments =
      // intersperse fragments with newlines, and indent them.
      // This differs from standard version (FragmentsDsl.fragmentsBlock()) in
      // that any failure gets properly indented, too.
      Fragments.empty
        .append(Format.t)
        .append(fs.flatMap(Seq(Format.br, _)))
        .append(Format.bt)
  }

  /** Implementation of Checker[IO] */
  trait IOChecker extends Checker[IO] { this: Specification =>
    import cats.effect.unsafe.implicits.global
    override implicit val M: Async[IO] = IO.asyncForIO
    override implicit val U: UnsafeRun[IO] = new UnsafeRun[IO] {
      def unsafeRunSync[A](ioa: IO[A]) = ioa.unsafeRunSync()
    }
  }
}
