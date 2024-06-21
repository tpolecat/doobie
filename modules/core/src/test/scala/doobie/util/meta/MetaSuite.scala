// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util.meta

import cats.effect.IO
import doobie._
import doobie.util.{Get, Put}

import scala.annotation.nowarn

case class Foo(str: String)

@nowarn("msg=.*local method foo.*")
class MetaSuite extends munit.FunSuite {

  test("Meta should exist for primitive types") {
    Meta[Int]
    Meta[String]
  }

  test("Meta should imply Get") {
    def foo[A: Meta] = Get[A]
  }

  test("Meta should imply Put") {
    def foo[A: Meta] = Put[A]
  }

}

class MetaDBSuite extends munit.FunSuite {
  import doobie.implicits._
  import cats.effect.unsafe.implicits.global

  lazy val xa = Transactor.fromDriverManager[IO](
    driver = "org.h2.Driver",
    url = "jdbc:h2:mem:queryspec;DB_CLOSE_DELAY=-1",
    user = "sa",
    password = "",
    logHandler = None
  )

  implicit def FooMeta: Meta[Foo] = Meta[String].tiemap(s => Either.cond(!s.isEmpty, Foo(s), "may not be empty"))(_.str)

  test("Meta.tiemap should accept valid values") {
    val x = sql"select 'bar'".query[Foo].unique.transact(xa).unsafeRunSync()
    assertEquals(x, Foo("bar"))
  }

  test("Meta.tiemap should reject invalid values") {
    val x = sql"select ''".query[Foo].unique.transact(xa).attempt.unsafeRunSync()
    assertEquals(x, Left(doobie.util.invariant.InvalidValue[String, Foo]("", "may not be empty")))
  }

}
