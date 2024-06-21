// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.effect.IO
import doobie._
import doobie.enumerated.JdbcType

import scala.annotation.nowarn

@nowarn("msg=.*pure expression does nothing in statement position.*")
class GetSuite extends munit.FunSuite with GetSuitePlatform {

  case class X(x: Int)
  case class Q(x: String)

  case class Z(i: Int, s: String)
  object S

  test("Get should exist for primitive types") {
    Get[Int]
    Get[String]
  }

  test("Get should be auto derived for unary products") {
    import doobie.generic.auto._

    Get[X]
    Get[Q]
  }

  test("Get is not auto derived without an import") {
    compileErrors("Get[X]")
    compileErrors("Get[Q]")
  }

  test("Get can be manually derived for unary products") {
    Get.derived[X]
    Get.derived[Q]
  }

  test("Get should not be derived for non-unary products") {
    compileErrors("Get[Z]")
    compileErrors("Get[(Int, Int)]")
    compileErrors("Get[S.type]")
  }

}

final case class Foo(s: String)
final case class Bar(n: Int)

class GetDBSuite extends munit.FunSuite {
  import cats.effect.unsafe.implicits.global
  import doobie.syntax.all._

  lazy val xa = Transactor.fromDriverManager[IO](
    driver = "org.h2.Driver",
    url = "jdbc:h2:mem:queryspec;DB_CLOSE_DELAY=-1",
    user = "sa",
    password = "",
    logHandler = None
  )

  // Both of these will fail at runtime if called with a null value, we check that this is
  // avoided below.
  implicit def FooMeta: Get[Foo] = Get[String].map(s => Foo(s.toUpperCase))
  implicit def barMeta: Get[Bar] = Get[Int].temap(n => if (n == 0) Left("cannot be 0") else Right(Bar(n)))

  test("Get should not allow map to observe null on the read side (AnyRef)") {
    val x = sql"select null".query[Option[Foo]].unique.transact(xa).unsafeRunSync()
    assertEquals(x, None)
  }

  test("Get should read non-null value (AnyRef)") {
    val x = sql"select 'abc'".query[Foo].unique.transact(xa).unsafeRunSync()
    assertEquals(x, Foo("ABC"))
  }

  test("Get should error when reading a NULL into an unlifted Scala type (AnyRef)") {
    def x = sql"select null".query[Foo].unique.transact(xa).attempt.unsafeRunSync()
    assertEquals(x, Left(doobie.util.invariant.NonNullableColumnRead(1, JdbcType.Char)))
  }

  test("Get should not allow map to observe null on the read side (AnyVal)") {
    val x = sql"select null".query[Option[Bar]].unique.transact(xa).unsafeRunSync()
    assertEquals(x, None)
  }

  test("Get should read non-null value (AnyVal)") {
    val x = sql"select 1".query[Bar].unique.transact(xa).unsafeRunSync()
    assertEquals(x, Bar(1))
  }

  test("Get should error when reading a NULL into an unlifted Scala type (AnyVal)") {
    def x = sql"select null".query[Bar].unique.transact(xa).attempt.unsafeRunSync()
    assertEquals(x, Left(doobie.util.invariant.NonNullableColumnRead(1, JdbcType.Integer)))
  }

  test("Get should error when reading an incorrect value") {
    def x = sql"select 0".query[Bar].unique.transact(xa).attempt.unsafeRunSync()
    assertEquals(x, Left(doobie.util.invariant.InvalidValue[Int, Bar](0, "cannot be 0")))
  }

}
