// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package example

import cats.effect.IO
import doobie.scalatest.IOChecker
import doobie.util.transactor.Transactor
import org.scalatest._

class AnalysisTestScalaTest extends funsuite.AnyFunSuite with matchers.must.Matchers with IOChecker {

  val transactor = Transactor.fromDriverManager[IO](
    driver = "org.postgresql.Driver",
    url = "jdbc:postgresql:world",
    user = "postgres",
    password = "password",
    logHandler = None
  )

  // Commented tests fail!
//test("speakerQuery")  { check(AnalysisTest.speakerQuery(null, 0)) }
  test("speakerQuery2") { check(AnalysisTest.speakerQuery2) }
  test("arrayTest") { check(AnalysisTest.arrayTest) }
  test("pointTest") { check(AnalysisTest.pointTest) }
//test("pointTest2")    { check(AnalysisTest.pointTest2) }
  test("update") { check(AnalysisTest.update) }
  test("update2") { check(AnalysisTest.update0_2) }

}
