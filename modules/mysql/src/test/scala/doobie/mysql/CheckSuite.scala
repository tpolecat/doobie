// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.mysql

import java.time.{LocalDate, LocalDateTime, LocalTime, OffsetDateTime}
import doobie.*
import doobie.implicits.*
import doobie.mysql.implicits.*
import doobie.util.Read
import doobie.util.analysis.ColumnTypeError

class CheckSuite extends munit.FunSuite {
  import cats.effect.unsafe.implicits.global
  import MySQLTestTransactor.xa

  // note selecting from a table because a value cannot be cast to a timestamp
  // and casting returns a nullable column

  test("OffsetDateTime Read typechecks") {
    val _ = OffsetDateTime.parse("2019-02-13T22:03:21.000+08:00")
    successRead[OffsetDateTime](sql"SELECT c_timestamp FROM test LIMIT 1")

    failedRead[OffsetDateTime](sql"SELECT '2019-02-13 22:03:21.051'")
//    failedWrite[OffsetDateTime](t, "VARCHAR")
    failedRead[OffsetDateTime](sql"SELECT c_date FROM test LIMIT 1")
//    failedWrite[OffsetDateTime](t, "DATE")
    failedRead[OffsetDateTime](sql"SELECT c_time FROM test LIMIT 1")
//    failedWrite[OffsetDateTime](t, "TIME")
    failedRead[OffsetDateTime](sql"SELECT c_datetime FROM test LIMIT 1")
//    failedWrite[OffsetDateTime](t, "DATETIME")
    failedRead[OffsetDateTime](sql"SELECT c_integer FROM test LIMIT 1")
//    failedWrite[OffsetDateTime](t, "INT")
  }

  test("LocalDateTime Read typechecks") {
    successRead[LocalDateTime](sql"SELECT c_datetime FROM test LIMIT 1")

    failedRead[LocalDateTime](sql"SELECT '2019-02-13 22:03:21.051'")
    failedRead[LocalDateTime](sql"SELECT c_date FROM test LIMIT 1")
    failedRead[LocalDateTime](sql"SELECT c_time FROM test LIMIT 1")
    failedRead[LocalDateTime](sql"SELECT c_timestamp FROM test LIMIT 1")
    failedRead[LocalDateTime](sql"SELECT 123")
  }

  test("LocalDate Read typechecks") {
    successRead[LocalDate](sql"SELECT c_date FROM test LIMIT 1")

    failedRead[LocalDate](sql"SELECT '2019-02-13'")
    failedRead[LocalDate](sql"SELECT c_time FROM test LIMIT 1")
    failedRead[LocalDate](sql"SELECT c_datetime FROM test LIMIT 1")
    failedRead[LocalDate](sql"SELECT c_timestamp FROM test LIMIT 1")
    failedRead[LocalDate](sql"SELECT 123")
  }

  test("LocalTime Read typechecks") {
    successRead[LocalTime](sql"SELECT c_time FROM test LIMIT 1")

    failedRead[LocalTime](sql"SELECT c_date FROM test LIMIT 1")
    failedRead[LocalTime](sql"SELECT c_datetime FROM test LIMIT 1")
    failedRead[LocalTime](sql"SELECT c_timestamp FROM test LIMIT 1")
    failedRead[LocalTime](sql"SELECT '22:03:21'")
    failedRead[LocalTime](sql"SELECT 123")
  }

  private def successRead[A: Read](frag: Fragment): Unit = {
    val analysisResult = frag.query[A].analysis.transact(xa).unsafeRunSync()
    assertEquals(analysisResult.columnAlignmentErrors, Nil)

    val result = frag.query[A].unique.transact(xa).attempt.unsafeRunSync()
    assert(result.isRight)
  }

  private def failedRead[A: Read](frag: Fragment): Unit = {
    val analysisResult = frag.query[A].analysis.transact(xa).unsafeRunSync()
    val errorClasses = analysisResult.columnAlignmentErrors.map(_.getClass)
    assertEquals(errorClasses, List(classOf[ColumnTypeError]))
  }

//  private def failedWrite[A: Put](value: A, dbType: String): Unit = {
//    val frag = sql"SELECT $value :: " ++ Fragment.const(dbType)
//    val analysisResult = frag.update.analysis.transact(xa).unsafeRunSync()
//    val errorClasses = analysisResult.parameterAlignmentErrors.map(_.getClass)
//    assertEquals(errorClasses, List(classOf[ParameterTypeError]))
//  }
}
