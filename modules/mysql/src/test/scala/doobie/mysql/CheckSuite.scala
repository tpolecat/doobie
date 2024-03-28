// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.mysql

import java.time.OffsetTime
import java.time.{LocalDate, LocalDateTime, LocalTime, OffsetDateTime}

import doobie._
import doobie.implicits._
import doobie.mysql.implicits._
import doobie.util.analysis.{ColumnTypeError, ColumnTypeWarning}

class CheckSuite extends munit.FunSuite {
  import cats.effect.unsafe.implicits.global
  import MySQLTestTransactor.xa

  // note selecting from a table because a value cannot be cast to a timestamp
  // and casting returns a nullable column

  test("OffsetDateTime Read typechecks") {
    successRead[OffsetDateTime](sql"SELECT c_timestamp FROM test LIMIT 1")

    warnRead[OffsetDateTime](sql"SELECT '2019-02-13 22:03:21.051'")
    warnRead[OffsetDateTime](sql"SELECT c_date FROM test LIMIT 1")
    warnRead[OffsetDateTime](sql"SELECT c_time FROM test LIMIT 1")
    warnRead[OffsetDateTime](sql"SELECT c_datetime FROM test LIMIT 1")
    failedRead[OffsetDateTime](sql"SELECT c_integer FROM test LIMIT 1")
  }

  test("LocalDateTime Read typechecks") {
    successRead[LocalDateTime](sql"SELECT c_datetime FROM test LIMIT 1")

    warnRead[LocalDateTime](sql"SELECT '2019-02-13 22:03:21.051'")
    warnRead[LocalDateTime](sql"SELECT c_date FROM test LIMIT 1")
    warnRead[LocalDateTime](sql"SELECT c_time FROM test LIMIT 1")
    warnRead[LocalDateTime](sql"SELECT c_timestamp FROM test LIMIT 1")
    failedRead[LocalDateTime](sql"SELECT 123")
  }

  test("LocalDate Read typechecks") {
    successRead[LocalDate](sql"SELECT c_date FROM test LIMIT 1")

    warnRead[LocalDate](sql"SELECT '2019-02-13'")
    warnRead[LocalDate](sql"SELECT c_time FROM test LIMIT 1")
    warnRead[LocalDate](sql"SELECT c_datetime FROM test LIMIT 1")
    warnRead[LocalDate](sql"SELECT c_timestamp FROM test LIMIT 1")
    failedRead[LocalDate](sql"SELECT 123")
  }

  test("LocalTime Read typechecks") {
    successRead[LocalTime](sql"SELECT c_time FROM test LIMIT 1")

    warnRead[LocalTime](sql"SELECT c_date FROM test LIMIT 1")
    warnRead[LocalTime](sql"SELECT c_datetime FROM test LIMIT 1")
    warnRead[LocalTime](sql"SELECT c_timestamp FROM test LIMIT 1")
    warnRead[LocalTime](sql"SELECT '22:03:21'")
    failedRead[LocalTime](sql"SELECT 123")
  }

  test("OffsetTime Read typechecks") {
    successRead[OffsetTime](sql"SELECT c_timestamp FROM test LIMIT 1")

    warnRead[OffsetTime](sql"SELECT '22:03:21'")
    warnRead[OffsetTime](sql"SELECT c_date FROM test LIMIT 1")
    warnRead[OffsetTime](sql"SELECT c_time FROM test LIMIT 1")
    warnRead[OffsetTime](sql"SELECT c_datetime FROM test LIMIT 1")
    failedRead[OffsetTime](sql"SELECT 123")
  }

  private def successRead[A: Read](frag: Fragment): Unit = {
    val analysisResult = frag.query[A].analysis.transact(xa).unsafeRunSync()
    assertEquals(analysisResult.columnAlignmentErrors, Nil)

    val result = frag.query[A].unique.transact(xa).attempt.unsafeRunSync()
    assert(result.isRight)
  }

  private def warnRead[A: Read](frag: Fragment): Unit = {
    val analysisResult = frag.query[A].analysis.transact(xa).unsafeRunSync()
    val errorClasses = analysisResult.columnAlignmentErrors.map(_.getClass)
    assertEquals(errorClasses, List(classOf[ColumnTypeWarning]))

    val result = frag.query[A].unique.transact(xa).attempt.unsafeRunSync()
    assert(result.isRight)
  }

  private def failedRead[A: Read](frag: Fragment): Unit = {
    val analysisResult = frag.query[A].analysis.transact(xa).unsafeRunSync()
    val errorClasses = analysisResult.columnAlignmentErrors.map(_.getClass)
    assertEquals(errorClasses, List(classOf[ColumnTypeError]))

    val result = frag.query[A].unique.transact(xa).attempt.unsafeRunSync()
    assert(result.isLeft)
  }

}
