// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.scalatest

import cats.effect.{Async, IO}
import doobie.{Update, Update0}
import doobie.syntax.connectionio._
import doobie.util.query.{Query, Query0}
import doobie.util.testing._
import org.scalatest.Assertions
import org.tpolecat.typename._

/** Mix-in trait for specifications that enables checking of doobie `Query` and `Update` values. Users must provide an
  * effect type `M` as well as a `Transactor[M]` and instances. As a convenience doobie provides specializations for
  * common effect types (see other types in this package).
  *
  * {{{
  * // An example specification, taken from the examples project.
  * class ExampleSpec extends AnyFunSuite with IOChecker {
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
trait Checker[M[_]] extends CheckerBase[M] { self: Assertions =>

  def check[A: Analyzable](a: A) = checkImpl(Analyzable.unpack(a))

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def checkOutput[A: TypeName](q: Query0[A]) =
    checkImpl(AnalysisArgs(
      s"Query0[${typeName[A]}]",
      q.pos,
      q.sql,
      q.outputAnalysis
    ))

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def checkOutput[A: TypeName, B: TypeName](q: Query[A, B]) =
    checkImpl(AnalysisArgs(
      s"Query[${typeName[A]}, ${typeName[B]}]",
      q.pos,
      q.sql,
      q.outputAnalysis
    ))

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def checkOutput[A: TypeName](u: Update[A]) =
    checkImpl(AnalysisArgs(
      s"Update[${typeName[A]}]",
      u.pos,
      u.sql,
      u.analysis
    ))

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def checkOutput(u: Update0) =
    checkImpl(AnalysisArgs(
      "Update0",
      u.pos,
      u.sql,
      u.analysis
    ))

  private def checkImpl(args: AnalysisArgs) = {
    val report = U.unsafeRunSync(analyze(args).transact(transactor))
    if (!report.succeeded) {
      fail(
        formatReport(args, report, colors)
          .padLeft("  ")
          .toString
      )
    }
  }
}

/** Implementation of Checker[IO] */
trait IOChecker extends Checker[IO] { self: Assertions =>
  import cats.effect.unsafe.implicits.global
  override implicit val M: Async[IO] = IO.asyncForIO
  override implicit val U: UnsafeRun[IO] = new UnsafeRun[IO] {
    def unsafeRunSync[A](ioa: IO[A]) = ioa.unsafeRunSync()
  }
}
