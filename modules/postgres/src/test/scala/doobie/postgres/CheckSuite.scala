// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.postgres.enums.*
import doobie.util.{Put, Read}
import doobie.util.analysis.{ColumnTypeError, ParameterTypeError}

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, OffsetTime}

class CheckSuite extends munit.FunSuite {
  import cats.effect.unsafe.implicits.global
  import PostgresTestTransactor.xa

  test("pgEnumString Read and Write typechecks") {
    successRead[MyEnum](sql"select 'foo' :: myenum")
    successWrite[MyEnum](MyEnum.Foo, "myenum")
  }

  test("OffsetDateTime Read and Write typechecks") {
    val t = OffsetDateTime.parse("2019-02-13T22:03:21.000+08:00")
    successRead[OffsetDateTime](sql"SELECT '2019-02-13T22:03:21.000' :: TIMESTAMPTZ")
    successWrite[OffsetDateTime](t, "TIMESTAMPTZ")

    failedRead[OffsetDateTime](sql"SELECT '2019-02-13T22:03:21.000' :: TIMESTAMP")
    errorWrite[OffsetDateTime](t, "TIMESTAMP")

    failedRead[OffsetDateTime](sql"SELECT '2019-02-13T22:03:21.000' :: TEXT")
    failedRead[OffsetDateTime](sql"SELECT '03:21' :: TIME")
    failedRead[OffsetDateTime](sql"SELECT '03:21' :: TIMETZ")
    failedRead[OffsetDateTime](sql"SELECT '2019-02-13' :: DATE")

    errorWrite[OffsetDateTime](t, "TEXT")
    errorWrite[OffsetDateTime](t, "TIME")
    errorWrite[OffsetDateTime](t, "TIMETZ")
    errorWrite[OffsetDateTime](t, "DATE")

    failedRead[OffsetDateTime](sql"SELECT '123' :: BYTEA")
    errorWrite[OffsetDateTime](t, "BYTEA")
  }

  test("Instant Read and Write typechecks") {
    val t = Instant.parse("2019-02-13T22:03:21.000Z")
    successRead[Instant](sql"SELECT '2019-02-13T22:03:21.000' :: TIMESTAMPTZ")
    successWrite[Instant](t, "TIMESTAMPTZ")

    failedRead[Instant](sql"SELECT '2019-02-13T22:03:21.000' :: TIMESTAMP")
    errorWrite[Instant](t, "TIMESTAMP")

    failedRead[Instant](sql"SELECT '2019-02-13T22:03:21.000' :: TEXT")
    failedRead[Instant](sql"SELECT '03:21' :: TIME")
    failedRead[Instant](sql"SELECT '03:21' :: TIMETZ")
    failedRead[Instant](sql"SELECT '2019-02-13' :: DATE")

    errorWrite[Instant](t, "TEXT")
    errorWrite[Instant](t, "TIME")
    errorWrite[Instant](t, "TIMETZ")
    errorWrite[Instant](t, "DATE")

    failedRead[Instant](sql"SELECT '123' :: BYTEA")
    errorWrite[Instant](t, "BYTEA")
  }

  test("LocalDateTime Read and Write typechecks") {
    val t = LocalDateTime.parse("2019-02-13T22:03:21.051")
    successRead[LocalDateTime](sql"SELECT '2019-02-13T22:03:21.051' :: TIMESTAMP")
    successWrite[LocalDateTime](t, "TIMESTAMP")

    failedRead[LocalDateTime](sql"SELECT '2019-02-13T22:03:21.051' :: TIMESTAMPTZ")
    errorWrite[LocalDateTime](t, "TIMESTAMPTZ")

    failedRead[LocalDateTime](sql"SELECT '2019-02-13T22:03:21.051' :: TEXT")
    failedRead[LocalDateTime](sql"SELECT '03:21' :: TIME")
    failedRead[LocalDateTime](sql"SELECT '03:21' :: TIMETZ")
    failedRead[LocalDateTime](sql"SELECT '2019-02-13' :: DATE")

    errorWrite[LocalDateTime](t, "TEXT")
    errorWrite[LocalDateTime](t, "TIME")
    errorWrite[LocalDateTime](t, "TIMETZ")
    errorWrite[LocalDateTime](t, "DATE")

    failedRead[LocalDateTime](sql"SELECT '123' :: BYTEA")
    errorWrite[LocalDateTime](t, "BYTEA")
  }

  test("LocalDate Read and Write typechecks") {
    val t = LocalDate.parse("2015-02-23")
    successRead[LocalDate](sql"SELECT '2015-02-23' :: DATE")
    successWrite[LocalDate](t, "DATE")

    failedRead[LocalDate](sql"SELECT '2015-02-23T01:23:13.000' :: TIMESTAMP")
    failedRead[LocalDate](sql"SELECT '2015-02-23T01:23:13.000Z' :: TIMESTAMPTZ")
    failedRead[LocalDate](sql"SELECT '2015-02-23' :: TEXT")
    failedRead[LocalDate](sql"SELECT '03:21' :: TIME")
    failedRead[LocalDate](sql"SELECT '03:21' :: TIMETZ")

    errorWrite[LocalDate](t, "TEXT")
    errorWrite[LocalDate](t, "TIME")
    errorWrite[LocalDate](t, "TIMETZ")

    failedRead[LocalDate](sql"SELECT '123' :: BYTEA")
    errorWrite[LocalDate](t, "BYTEA")
  }

  test("LocalTime Read and Write typechecks") {
    val t = LocalTime.parse("23:13")
    successRead[LocalTime](sql"SELECT '23:13' :: TIME")
    successWrite[LocalTime](t, "TIME")

    failedRead[LocalTime](sql"SELECT '2015-02-23T01:23:13.000' :: TIMESTAMP")
    failedRead[LocalTime](sql"SELECT '2015-02-23T01:23:13.000Z' :: TIMESTAMPTZ")
    failedRead[LocalTime](sql"SELECT '2015-02-23' :: TEXT")
    failedRead[LocalTime](sql"SELECT '2015-02-23' :: DATE")
    failedRead[LocalTime](sql"SELECT '23:13' :: TIMETZ")

    errorWrite[LocalTime](t, "TEXT")
    errorWrite[LocalTime](t, "TIMETZ")
    errorWrite[LocalTime](t, "DATE")

    failedRead[LocalTime](sql"SELECT '123' :: BYTEA")
    errorWrite[LocalTime](t, "BYTEA")
  }

  test("OffsetTime Read and Write typechecks") {
    val t = OffsetTime.parse("23:13-01:00")
    successRead[OffsetTime](sql"SELECT '23:13' :: TIMETZ")
    successWrite[OffsetTime](t, "TIMETZ")

    failedRead[OffsetTime](sql"SELECT '2015-02-23T01:23:13.000' :: TIMESTAMP")
    failedRead[OffsetTime](sql"SELECT '2015-02-23T01:23:13.000Z' :: TIMESTAMPTZ")
    failedRead[OffsetTime](sql"SELECT '2015-02-23' :: TEXT")
    failedRead[OffsetTime](sql"SELECT '2015-02-23' :: DATE")
    failedRead[OffsetTime](sql"SELECT '23:13' :: TIME")

    errorWrite[OffsetTime](t, "TEXT")
    errorWrite[OffsetTime](t, "TIME")
    errorWrite[OffsetTime](t, "DATE")

    failedRead[OffsetTime](sql"SELECT '123' :: BYTEA")
    errorWrite[OffsetTime](t, "BYTEA")
  }

  private def successRead[A: Read](frag: Fragment): Unit = {
    val analysisResult = frag.query[A].analysis.transact(xa).unsafeRunSync()
    assertEquals(analysisResult.columnAlignmentErrors, Nil)

    val result = frag.query[A].unique.transact(xa).attempt.unsafeRunSync()
    assert(result.isRight)
  }

  private def successWrite[A: Put](value: A, dbType: String): Unit = {
    val frag = sql"SELECT $value :: " ++ Fragment.const(dbType)
    val analysisResult = frag.update.analysis.transact(xa).unsafeRunSync()
    assertEquals(analysisResult.parameterAlignmentErrors, Nil)
  }

  private def failedRead[A: Read](frag: Fragment): Unit = {
    val analysisResult = frag.query[A].analysis.transact(xa).unsafeRunSync()
    val errorClasses = analysisResult.columnAlignmentErrors.map(_.getClass)
    assertEquals(errorClasses, List(classOf[ColumnTypeError]))
  }

  private def errorWrite[A: Put](value: A, dbType: String): Unit = {
    val frag = sql"SELECT $value :: " ++ Fragment.const(dbType)
    val analysisResult = frag.update.analysis.transact(xa).unsafeRunSync()
    val errorClasses = analysisResult.parameterAlignmentErrors.map(_.getClass)
    assertEquals(errorClasses, List(classOf[ParameterTypeError]))
  }

}
