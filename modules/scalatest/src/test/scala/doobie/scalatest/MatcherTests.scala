// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.scalatest

import cats.effect.{ ContextShift, IO }
import doobie._, doobie.implicits._
import org.scalatest._
import scala.concurrent.ExecutionContext

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
trait MatcherChecks[M[_]] extends FunSuite
    with MustMatchers
    with AnalysisMatchers[M] {

  implicit def contextShift: ContextShift[M]

  lazy val transactor = Transactor.fromDriverManager[M](
    "org.h2.Driver",
    "jdbc:h2:mem:queryspec;DB_CLOSE_DELAY=-1",
    "sa", ""
  )

  test("valid query should pass") {
    sql"select 1".query[Int] must typecheck
  }

  test("malformed sql should fail") {
    sql"not a valid sql".query[Int].must(not(typecheck))
  }

  test("query with mismatched type should fail") {
    sql"select 'foo'".query[Int].must(not(typecheck))
  }
}

class IOMatcherCheck extends MatcherChecks[IO] with IOChecker {
  def contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)
}
