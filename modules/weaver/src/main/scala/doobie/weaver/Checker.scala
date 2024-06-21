// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.weaver

import weaver._
import weaver.Expectations.Helpers._
import doobie.util.testing._
import doobie.syntax.connectionio._
import cats.effect.kernel.Sync
import cats.syntax.all._
import doobie.util.Colors
import doobie._
import org.tpolecat.typename._

/** Module with a mix-in trait for specifications that enables checking of doobie `Query` and `Update` values.
  *
  * {{{
  * object ExampleSuite extends IOSuite with IOChecker {
  *
  *   override type Res = Transactor[IO]
  *   override def sharedResource: Resource[IO, Res] =
  *     // The transactor to use for the tests.
  *     Resource.pure(Transactor.fromDriverManager[IO](...))
  *
  *   // Now just mention the queries. Arguments are not used.
  *   test("findByNameAndAge") { implicit transactor => check(MyDaoModule.findByNameAndAge(null, 0)) }
  *   test("allWoozles") { implicit transactor => check(MyDaoModule.allWoozles) }
  *
  * }
  * }}}
  */
trait Checker[M[_]] {
  def check[A: Analyzable](a: A)(implicit M: Sync[M], pos: SourceLocation, transactor: Transactor[M]): M[Expectations] =
    checkImpl(Analyzable.unpack(a))

  def checkOutput[A: TypeName](q: Query0[A])(implicit
      M: Sync[M],
      pos: SourceLocation,
      transactor: Transactor[M]
  ): M[Expectations] =
    checkImpl(AnalysisArgs(
      s"Query0[${typeName[A]}]",
      q.pos,
      q.sql,
      q.outputAnalysis
    ))

  def checkOutput[A: TypeName, B: TypeName](q: Query[A, B])(implicit
      M: Sync[M],
      pos: SourceLocation,
      transactor: Transactor[M]
  ): M[Expectations] =
    checkImpl(AnalysisArgs(
      s"Query[${typeName[A]}, ${typeName[B]}]",
      q.pos,
      q.sql,
      q.outputAnalysis
    ))

  private def checkImpl(args: AnalysisArgs)(implicit M: Sync[M], pos: SourceLocation, transactor: Transactor[M]) = {
    analyze(args).transact(transactor).map { report =>
      if (!report.succeeded)
        failure(formatReport(args, report, Colors.Ansi)
          .padLeft("  ")
          .toString)
      else success
    }
  }
}
