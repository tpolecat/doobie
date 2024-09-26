// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.h2

import java.util.UUID
import cats.effect.IO
import doobie.*
import doobie.implicits.*
import doobie.h2.implicits.*
import doobie.util.{Put, Get, Read}
import doobie.util.analysis.{Analysis, ColumnTypeError}
import doobie.util.arbitraries.SQLArbitraries.*
import doobie.util.arbitraries.StringArbitraries.*
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}

// Establish that we can read various types. It's not very comprehensive as a test, bit it's a start.
class h2typesspec extends munit.ScalaCheckSuite {

  import cats.effect.unsafe.implicits.global

  val xa = Transactor.fromDriverManager[IO](
    driver = "org.h2.Driver",
    url = "jdbc:h2:mem:ch3;DB_CLOSE_DELAY=-1",
    user = "sa",
    password = "",
    logHandler = None
  )

  def inOut[A: Put: Get](col: String, a: A): ConnectionIO[A] =
    for {
      _ <- Update0(s"CREATE LOCAL TEMPORARY TABLE TEST (value $col NOT NULL)", None).run
      _ <- sql"INSERT INTO TEST VALUES ($a)".update.run
      a0 <- sql"SELECT value FROM TEST".query[A].unique
    } yield (a0)

  def inOutOpt[A: Put: Get](col: String, a: Option[A]): ConnectionIO[Option[A]] =
    for {
      _ <- Update0(s"CREATE LOCAL TEMPORARY TABLE TEST (value $col)", None).run
      _ <- sql"INSERT INTO TEST VALUES ($a)".update.run
      a0 <- sql"SELECT value FROM TEST".query[Option[A]].unique
    } yield (a0)

  def testInOut[A](col: String)(implicit m: Get[A], p: Put[A], arbitrary: Arbitrary[A]) = {
    testInOutWithCustomGen(col, arbitrary.arbitrary)
  }

  def testInOutWithCustomGen[A](col: String, gen: Gen[A])(implicit m: Get[A], p: Put[A]) = {
    test(s"Mapping for $col as ${m.typeStack} - write+read $col as ${m.typeStack}") {
      forAll(gen) { (t: A) => assertEquals(inOut(col, t).transact(xa).attempt.unsafeRunSync(), Right(t)) }
    }
    test(s"Mapping for $col as ${m.typeStack} - write+read $col as Option[${m.typeStack}] (Some)") {
      forAll(gen) { (t: A) =>
        assertEquals(inOutOpt[A](col, Some(t)).transact(xa).attempt.unsafeRunSync(), Right(Some(t)))
      }
    }
    test(s"Mapping for $col as ${m.typeStack} - write+read $col as Option[${m.typeStack}] (None)") {
      assertEquals(inOutOpt[A](col, None).transact(xa).attempt.unsafeRunSync(), Right(None))
    }
  }

  def skip(col: String, msg: String = "not yet implemented") =
    test(s"Mapping for $col - $msg".ignore) {}

  testInOut[Int]("INT")
  testInOut[Boolean]("BOOLEAN")
  testInOut[Byte]("TINYINT")
  testInOut[Short]("SMALLINT")
  testInOut[Long]("BIGINT")
  testInOut[BigDecimal]("DECIMAL")

  /*
      TIME
      If fractional seconds precision is specified it should be from 0 to 9, 0 is default.
   */
  testInOut[java.sql.Time]("TIME")
  testInOut[java.time.LocalTime]("TIME(9)")

  testInOut[java.sql.Date]("DATE")
  testInOut[java.time.LocalDate]("DATE")

  /*
      TIMESTAMP
      If fractional seconds precision is specified it should be from 0 to 9, 6 is default.
   */
  testInOut[java.sql.Timestamp]("TIMESTAMP(9)")
  testInOut[java.time.LocalDateTime]("TIMESTAMP(9)")

  /*
      TIME WITH TIMEZONE
      If fractional seconds precision is specified it should be from 0 to 9, 6 is default.
   */
  testInOut[java.time.OffsetTime]("TIME(9) WITH TIME ZONE")

  /*
      TIMESTAMP WITH TIME ZONE
      If fractional seconds precision is specified it should be from 0 to 9, 6 is default.
   */
  testInOut[java.time.OffsetDateTime]("TIMESTAMP(9) WITH TIME ZONE")
  testInOut[java.time.Instant]("TIMESTAMP(9) WITH TIME ZONE")

  testInOut[java.time.ZoneId]("VARCHAR")

  testInOut[List[Byte]]("BINARY")
  skip("OTHER")
  testInOut[String]("VARCHAR")
  testInOutWithCustomGen[String]("CHAR(3)", nLongString(3))
  skip("BLOB")
  skip("CLOB")
  testInOut[UUID]("UUID")
  testInOut[List[Int]]("ARRAY")
  testInOut[List[String]]("ARRAY")
  skip("GEOMETRY")

  test("Mapping for Boolean should pass query analysis for unascribed 'true'") {
    val a = sql"select true".query[Boolean].analysis.transact(xa).unsafeRunSync()
    assertEquals(a.alignmentErrors, Nil)
  }
  test("Mapping for Boolean should pass query analysis for ascribed BIT") {
    val a = sql"select true::BIT".query[Boolean].analysis.transact(xa).unsafeRunSync()
    assertEquals(a.alignmentErrors, Nil)
  }
  test("Mapping for Boolean should pass query analysis for ascribed BOOLEAN") {
    val a = sql"select true::BOOLEAN".query[Boolean].analysis.transact(xa).unsafeRunSync()
    assertEquals(a.alignmentErrors, Nil)
  }

  test("Mapping for UUID should pass query analysis for unascribed UUID") {
    val a = sql"select random_uuid()".query[UUID].analysis.transact(xa).unsafeRunSync()
    assertEquals(a.alignmentErrors, Nil)
  }
  test("Mapping for UUID should pass query analysis for ascribed UUID") {
    val a = sql"select random_uuid()::UUID".query[UUID].analysis.transact(xa).unsafeRunSync()
    assertEquals(a.alignmentErrors, Nil)
  }

  test("Mapping for LocalDate should pass query analysis for DATE") {
    val a = analyzeDate[java.time.LocalDate]
    assertEquals(a.alignmentErrors, Nil)
  }

  test("Mapping for LocalDate should fail query analysis for TIMESTAMP") {
    val a = analyzeTimestamp[java.time.LocalDate]
    assertAnalyzeColumnError(a)
  }

  test("Mapping for LocalTime should pass query analysis for TIME") {
    val a = analyzeTime[java.time.LocalTime]
    assertEquals(a.alignmentErrors, Nil)
  }

  test("Mapping for LocalTime should fail query analysis for TIME WITH TIME ZONE") {
    val a = analyzeTimeWithTimeZone[java.time.LocalTime]
    assertAnalyzeColumnError(a)
  }

  test("Mapping for OffsetTime should pass query analysis for TIME WITH TIME ZONE") {
    val a = analyzeTimeWithTimeZone[java.time.OffsetTime]
    assertEquals(a.alignmentErrors, Nil)
  }

  test("Mapping for OffsetTime should fail query analysis for TIME") {
    val a = analyzeTime[java.time.OffsetTime]
    assertAnalyzeColumnError(a)
  }

  test("Mapping for LocalDateTime should pass query analysis for TIMESTAMP") {
    val a = analyzeTimestamp[java.time.LocalDateTime]
    assertEquals(a.alignmentErrors, Nil)
  }

  test("Mapping for LocalDateTime should fail query analysis for DATE") {
    val a = analyzeDate[java.time.LocalDateTime]
    assertAnalyzeColumnError(a)
  }

  test("Mapping for LocalDateTime should fail query analysis for TIME") {
    val a = analyzeTime[java.time.LocalDateTime]
    assertAnalyzeColumnError(a)
  }

  test("Mapping for LocalDateTime should fail query analysis for TIMESTAMP WITH TIME ZONE") {
    val a = analyzeTimestampWithTimeZone[java.time.LocalDateTime]
    assertAnalyzeColumnError(a)
  }

  test("Mapping for OffsetDateTime should pass query analysis for TIMESTAMP WITH TIME ZONE") {
    val a = analyzeTimestampWithTimeZone[java.time.OffsetDateTime]
    assertEquals(a.alignmentErrors, Nil)
  }

  test("Mapping for OffsetDateTime should fail query analysis for TIME WITH TIME ZONE") {
    val a = analyzeTimeWithTimeZone[java.time.OffsetDateTime]
    assertAnalyzeColumnError(a)
  }

  test("Mapping for OffsetDateTime should fail query analysis for TIMESTAMP") {
    val a = analyzeTimestamp[java.time.OffsetDateTime]
    assertAnalyzeColumnError(a)
  }

  private def analyzeDate[R: Read] = analyze(sql"select '2000-01-02'::DATE".query[R])
  private def analyzeTime[R: Read] = analyze(sql"select '01:02:03'::TIME".query[R])
  private def analyzeTimeWithTimeZone[R: Read] = analyze(sql"select '01:02:03+04:05'::TIME WITH TIME ZONE".query[R])
  private def analyzeTimestamp[R: Read] = analyze(sql"select '2000-01-02T01:02:03'::TIMESTAMP".query[R])
  private def analyzeTimestampWithTimeZone[R: Read] =
    analyze(sql"select '2000-01-02T01:02:03+04:05'::TIMESTAMP WITH TIME ZONE".query[R])

  private def analyze[R](q: Query0[R]) = q.analysis.transact(xa).unsafeRunSync()

  private def assertAnalyzeColumnError(result: Analysis): Unit = {
    val errorClasses = result.alignmentErrors.map(_.getClass)
    assertEquals(errorClasses, List(classOf[ColumnTypeError]))
  }

}
