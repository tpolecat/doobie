// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.effect.IO
import doobie.*
import doobie.implicits.*
import org.postgresql.util.{PSQLException, PSQLState}

import java.sql.SQLException

class CatchSqlSuite extends munit.FunSuite {

  import cats.effect.unsafe.implicits.global

  val SQLSTATE_FOO = SqlState("Foo")
  val SQLSTATE_BAR = SqlState("Bar")

  test("attemptSql should do nothing on success") {
    assertEquals(IO.delay(3).attemptSql.unsafeRunSync(), Right(3))
  }

  test("attemptSql should catch SQLException") {
    val e = new SQLException
    assertEquals(IO.raiseError(e).attemptSql.unsafeRunSync(), Left(e))
  }

  test("attemptSql should ignore non-SQLException") {
    val e = new IllegalArgumentException
    intercept[IllegalArgumentException] {
      IO.raiseError(e).attemptSql.unsafeRunSync()
    }
  }

  test("attemptSomeSql should do nothing on success") {
    assertEquals(
      IO.delay(3).attemptSomeSql {
        case _: SQLException => 42
      }.unsafeRunSync(),
      Right(3))
  }

  test("attemptSomeSql should catch SQLException with matching subtype (1)") {
    val e = new SQLException("", SQLSTATE_FOO.value)
    assertEquals(
      IO.raiseError(e).attemptSomeSql {
        case _: SQLException => 42
      }.unsafeRunSync(),
      Left(42))
  }

  test("attemptSomeSql should catch SQLException with matching subtype (2)") {
    val PSQLSTATE = PSQLState.CHECK_VIOLATION
    val e = new PSQLException("", PSQLSTATE)
    assertEquals(
      IO.raiseError(e).attemptSomeSql {
        case exception: PSQLException if exception.getSQLState == PSQLSTATE.getState => 66
      }.unsafeRunSync(),
      Left(66))
  }

  test("attemptSomeSql should ignore SQLException with non-matching subtype") {
    final case class AnotherSQLException(message: String) extends SQLException(message)
    val e = AnotherSQLException("")
    intercept[AnotherSQLException] {
      IO.raiseError(e).attemptSomeSql {
        case exception: PSQLException if exception.getSQLState == "Baz" => 66
      }.unsafeRunSync()
    }
  }

  test("attemptSomeSql should ignore non-SQLException") {
    val e = new IllegalArgumentException
    intercept[IllegalArgumentException] {
      IO.raiseError(e).attemptSomeSql {
        case _: SQLException => 42
      }.unsafeRunSync()
    }
  }
  test("attemptSqlState should do nothing on success") {
    assertEquals(IO.delay(3).attemptSqlState.unsafeRunSync(), Right(3))
  }

  test("attemptSqlState should catch SQLException") {
    val e = new SQLException("", SQLSTATE_FOO.value)
    assertEquals(IO.raiseError(e).attemptSqlState.unsafeRunSync(), Left(SQLSTATE_FOO))
  }

  test("attemptSqlState should ignore non-SQLException") {
    val e = new IllegalArgumentException
    intercept[IllegalArgumentException] {
      IO.raiseError(e).attemptSqlState.unsafeRunSync()
    }
  }

  test("attemptSomeSqlState should do nothing on success") {
    assertEquals(
      IO.delay(3).attemptSomeSqlState {
        case SQLSTATE_FOO => 42
        case SQLSTATE_BAR => 66
      }.unsafeRunSync(),
      Right(3))
  }

  test("attemptSomeSqlState should catch SQLException with matching state (1)") {
    val e = new SQLException("", SQLSTATE_FOO.value)
    assertEquals(
      IO.raiseError(e).attemptSomeSqlState {
        case SQLSTATE_FOO => 42
        case SQLSTATE_BAR => 66
      }.unsafeRunSync(),
      Left(42))
  }

  test("attemptSomeSqlState should catch SQLException with matching state (2)") {
    val e = new SQLException("", SQLSTATE_BAR.value)
    assertEquals(
      IO.raiseError(e).attemptSomeSqlState {
        case SQLSTATE_FOO => 42
        case SQLSTATE_BAR => 66
      }.unsafeRunSync(),
      Left(66))
  }

  test("attemptSomeSqlState should ignore SQLException with non-matching state") {
    val e = new SQLException("", SQLSTATE_BAR.value)
    intercept[SQLException] {
      IO.raiseError(e).attemptSomeSqlState {
        case SQLSTATE_FOO => 42
      }.unsafeRunSync()
    }
  }

  test("attemptSomeSqlState should ignore non-SQLException") {
    val e = new IllegalArgumentException
    intercept[IllegalArgumentException] {
      IO.raiseError(e).attemptSomeSqlState {
        case SQLSTATE_FOO => 42
      }.unsafeRunSync()
    }
  }

  lazy val rescue = IO.delay(4)

  test("exceptSql should do nothing on success") {
    assertEquals(IO.delay(3).exceptSql(_ => rescue).unsafeRunSync(), 3)
  }

  test("exceptSql should catch SQLException") {
    val e = new SQLException("", SQLSTATE_FOO.value)
    assertEquals(IO.raiseError[Int](e).exceptSql(_ => rescue).unsafeRunSync(), 4)
  }

  test("exceptSql should ignore non-SQLException") {
    val e = new IllegalArgumentException
    intercept[IllegalArgumentException] {
      IO.raiseError[Int](e).exceptSql(_ => rescue).unsafeRunSync()
    }
  }

  test("exceptSqlState should do nothing on success") {
    assertEquals(IO.delay(3).exceptSqlState(_ => rescue).unsafeRunSync(), 3)
  }

  test("exceptSqlState should catch SQLException") {
    val e = new SQLException("", SQLSTATE_FOO.value)
    assertEquals(IO.raiseError[Int](e).exceptSqlState(_ => rescue).unsafeRunSync(), 4)
  }

  test("exceptSqlState should ignore non-SQLException") {
    val e = new IllegalArgumentException
    intercept[IllegalArgumentException] {
      IO.raiseError[Int](e).exceptSqlState(_ => rescue).unsafeRunSync()
    }
  }

  test("exceptSomeSqlState should do nothing on success") {
    assertEquals(IO.delay(3).exceptSomeSqlState { case _ => rescue }.unsafeRunSync(), 3)
  }

  test("exceptSomeSqlState should catch SQLException with some state") {
    val e = new SQLException("", SQLSTATE_FOO.value)
    assertEquals(IO.raiseError[Int](e).exceptSomeSqlState { case SQLSTATE_FOO => rescue }.unsafeRunSync(), 4)
  }

  test("exceptSomeSqlState should ignore SQLException with other state") {
    val e = new SQLException("", SQLSTATE_FOO.value)
    intercept[SQLException] {
      IO.raiseError[Int](e).exceptSomeSqlState { case SQLSTATE_BAR => rescue }.unsafeRunSync()
    }
  }

  test("exceptSomeSqlState should ignore non-SQLException") {
    val e = new IllegalArgumentException
    intercept[IllegalArgumentException] {
      IO.raiseError[Int](e).exceptSomeSqlState { case _ => rescue }.unsafeRunSync()
    }
  }

  test("onSqlException should do nothing on success") {
    var a = 1
    val _ = IO.delay(3).onSqlException(IO.delay(a += 1)).attempt.unsafeRunSync()
    assertEquals(a, 1)
  }

  test("onSqlException should perform its effect on SQLException") {
    var a = 1
    val e = new SQLException("", SQLSTATE_FOO.value)
    assertEquals(IO.raiseError[Int](e).onSqlException(IO.delay(a += 1)).attempt.unsafeRunSync(), Left(e))
    assertEquals(a, 2)
  }

  test("onSqlException should ignore its effect on non-SQLException") {
    var a = 1
    val e = new IllegalArgumentException
    assertEquals(IO.raiseError[Int](e).onSqlException(IO.delay(a += 1)).attempt.unsafeRunSync(), Left(e))
    assertEquals(a, 1)
  }

}
