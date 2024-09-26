// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.refined

import cats.Show
import cats.effect.IO
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.refined.implicits.*
import doobie.util.Write
import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.*
import doobie.util.invariant.*

class RefinedSuite extends munit.FunSuite {

  import cats.effect.unsafe.implicits.global

  val xa = Transactor.fromDriverManager[IO](
    driver = "org.h2.Driver",
    url = "jdbc:h2:mem:refined;DB_CLOSE_DELAY=-1",
    user = "sa",
    password = "",
    logHandler = None
  )

  type PositiveInt = Int Refined Positive

  test("Meta should exist for refined types") {
    Meta[PositiveInt]
  }

  case class Point(x: Int, y: Int)
  object Point {
    implicit val show: Show[Point] = Show.fromToString
  }
  case class Quadrant1()
  type PointInQuadrant1 = Point Refined Quadrant1

  implicit val PointComposite: Write[Point] =
    Write[(Int, Int)].contramap((p: Point) => (p.x, p.y))

  implicit val quadrant1Validate: Validate.Plain[Point, Quadrant1] =
    Validate.fromPredicate(p => p.x >= 0 && p.y >= 0, p => show"($p is in quadrant 1)", Quadrant1())

  test("Write should exist for refined types") {
    Write[PointInQuadrant1]
  }

  test("Write should exist for Option of a refined type") {
    Write[Option[PositiveInt]]
  }

  test("Query should return a refined type when conversion is possible") {
    sql"select 123".query[PositiveInt].unique.transact(xa).void.unsafeRunSync()
  }

  test("Query should return an Option of a refined type when query returns null-value") {
    sql"select NULL".query[Option[PositiveInt]].unique.transact(xa).void.unsafeRunSync()
  }

  test("Query should return an Option of a refined type when query returns a value and converion is possible") {
    sql"select NULL".query[Option[PositiveInt]].unique.transact(xa).void.unsafeRunSync()
  }

  test("Query should save a None of a refined type") {
    val none: Option[PositiveInt] = None
    insertOptionalPositiveInt(none)
  }

  test("Query should save a Some of a refined type") {
    val somePositiveInt: Option[PositiveInt] = refineV[Positive](5).toOption
    insertOptionalPositiveInt(somePositiveInt)
  }

  def insertOptionalPositiveInt(v: Option[PositiveInt]) = {
    val queryRes = for {
      _ <- Update0(s"CREATE LOCAL TEMPORARY TABLE TEST (value INT)", None).run
      _ <- sql"INSERT INTO TEST VALUES ($v)".update.run
    } yield ()
    queryRes.transact(xa).unsafeRunSync()
  }

  test("Query should throw an SecondaryValidationFailed if value does not fit the refinement-type ") {
    secondaryValidationFailedCaught_?(
      sql"select -1".query[PositiveInt].unique.transact(xa).void.unsafeRunSync()
    )
  }

  test("Query should return a refined product-type when conversion is possible") {
    sql"select 1, 1".query[PointInQuadrant1].unique.transact(xa).void.unsafeRunSync()
  }

  test("Query should throw an SecondaryValidationFailed if object does not fit the refinement-type ") {
    secondaryValidationFailedCaught_?(
      sql"select -1, 1".query[PointInQuadrant1].unique.transact(xa).void.unsafeRunSync()
    )
  }

  private def secondaryValidationFailedCaught_?(query: => Unit) =
    intercept[SecondaryValidationFailed[?]](query)

}
