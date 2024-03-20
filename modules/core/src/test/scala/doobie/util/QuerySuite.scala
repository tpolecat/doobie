// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.effect.IO
import doobie._, doobie.implicits._
import scala.Predef._

class QuerySuite extends munit.FunSuite {

  import cats.effect.unsafe.implicits.global

  val xa = Transactor.fromDriverManager[IO](
    driver = "org.h2.Driver",
    url = "jdbc:h2:mem:queryspec;DB_CLOSE_DELAY=-1",
    user = "sa",
    password = "",
    logHandler = None
  )

  val q = Query[String, Int]("select 123 where ? = 'foo'", None)
  val pairQuery = Query[String, (String, Int)]("select 'xxx', 123 where ? = 'foo'", None)

  test("Query (non-empty) to") {
    assertEquals(q.to[List]("foo").transact(xa).unsafeRunSync(), List(123))
  }
  test("Query (non-empty) toMap") {
    assertEquals(pairQuery.toMap[String, Int]("foo").transact(xa).unsafeRunSync(), Map("xxx" -> 123))
  }
  test("Query (non-empty) unique") {
    assertEquals(q.unique("foo").transact(xa).unsafeRunSync(), 123)
  }
  test("Query (non-empty) option") {
    assertEquals(q.option("foo").transact(xa).unsafeRunSync(), Some(123))
  }
  test("Query (non-empty) map") {
    assertEquals(q.map("x" * _).to[List]("foo").transact(xa).unsafeRunSync(), List("x" * 123))
  }
  test("Query (non-empty) contramap") {
    assertEquals(q.contramap[Int](n => "foo" * n).to[List](1).transact(xa).unsafeRunSync(), List(123))
  }

  test("Query (empty) to") {
    assertEquals(q.to[List]("bar").transact(xa).unsafeRunSync(), Nil)
  }
  test("Query (empty) toMap") {
    assertEquals(pairQuery.toMap[String, Int]("bar").transact(xa).unsafeRunSync(), Map.empty[String, Int])
  }
  test("Query (empty) unique") {
    assertEquals(q.unique("bar").transact(xa).attempt.unsafeRunSync(), Left(invariant.UnexpectedEnd))
  }
  test("Query (empty) option") {
    assertEquals(q.option("bar").transact(xa).unsafeRunSync(), None)
  }
  test("Query (empty) map") {
    assertEquals(q.map("x" * _).to[List]("bar").transact(xa).unsafeRunSync(), Nil)
  }
  test("Query (empty) contramap") {
    assertEquals(q.contramap[Int](n => "bar" * n).to[List](1).transact(xa).unsafeRunSync(), Nil)
  }

  test("Query0 from Query (non-empty) to") {
    assertEquals(q.toQuery0("foo").to[List].transact(xa).unsafeRunSync(), List(123))
  }
  test("Query0 from Query (non-empty) toMap") {
    assertEquals(pairQuery.toQuery0("foo").toMap[String, Int].transact(xa).unsafeRunSync(), Map("xxx" -> 123))
  }
  test("Query0 from Query (non-empty) unique") {
    assertEquals(q.toQuery0("foo").unique.transact(xa).unsafeRunSync(), 123)
  }
  test("Query0 from Query (non-empty) option") {
    assertEquals(q.toQuery0("foo").option.transact(xa).unsafeRunSync(), Some(123))
  }
  test("Query0 from Query (non-empty) map") {
    assertEquals(q.toQuery0("foo").map(_ * 2).to[List].transact(xa).unsafeRunSync(), List(246))
  }

  test("Query0 from Query (empty) to") {
    assertEquals(q.toQuery0("bar").to[List].transact(xa).unsafeRunSync(), Nil)
  }
  test("Query0 from Query (empty) toMap") {
    assertEquals(pairQuery.toQuery0("bar").toMap[String, Int].transact(xa).unsafeRunSync(), Map.empty[String, Int])
  }
  test("Query0 from Query (empty) unique") {
    assertEquals(q.toQuery0("bar").unique.transact(xa).attempt.unsafeRunSync(), Left(invariant.UnexpectedEnd))
  }
  test("Query0 from Query (empty) option") {
    assertEquals(q.toQuery0("bar").option.transact(xa).unsafeRunSync(), None)
  }
  test("Query0 from Query (empty) map") {
    assertEquals(q.toQuery0("bar").map(_ * 2).to[List].transact(xa).unsafeRunSync(), Nil)
  }

  val q0n = Query0[Int]("select 123 where 'foo' = 'foo'", None)
  val pairQ0n = Query0[(String, Int)]("select 'xxx', 123 where 'foo' = 'foo'", None)

  test("Query0 via constructor (non-empty) to") {
    assertEquals(q0n.to[List].transact(xa).unsafeRunSync(), List(123))
  }
  test("Query0 via constructor (non-empty) toMap") {
    assertEquals(pairQ0n.toMap[String, Int].transact(xa).unsafeRunSync(), Map("xxx" -> 123))
  }
  test("Query0 via constructor (non-empty) unique") {
    assertEquals(q0n.unique.transact(xa).unsafeRunSync(), 123)
  }
  test("Query0 via constructor (non-empty) option") {
    assertEquals(q0n.option.transact(xa).unsafeRunSync(), Some(123))
  }
  test("Query0 via constructor (non-empty) map") {
    assertEquals(q0n.map(_ * 2).to[List].transact(xa).unsafeRunSync(), List(246))
  }

  val q0e = Query0[Int]("select 123 where 'bar' = 'foo'", None)
  val pairQ0e = Query0[(String, Int)]("select 'xxx', 123 where 'bar' = 'foo'", None)

  test("Query0 via constructor (empty) to") {
    assertEquals(q0e.to[List].transact(xa).unsafeRunSync(), Nil)
  }
  test("Query0 via constructor (empty) toMap") {
    assertEquals(pairQ0e.toMap[String, Int].transact(xa).unsafeRunSync(), Map.empty[String, Int])
  }
  test("Query0 via constructor (empty) unique") {
    assertEquals(q0e.unique.transact(xa).attempt.unsafeRunSync(), Left(invariant.UnexpectedEnd))
  }
  test("Query0 via constructor (empty) option") {
    assertEquals(q0e.option.transact(xa).unsafeRunSync(), None)
  }
  test("Query0 via constructor (empty) map") {
    assertEquals(q0e.map(_ * 2).to[List].transact(xa).unsafeRunSync(), Nil)
  }

  val qf = sql"select 'foo', ${1: Int}, ${Option.empty[Int]}, ${Option(42)}".query[String] // wrong!
  test("Query to Fragment and back") {
    val qfʹ = qf.toFragment.query[(String, Int, Option[Int], Option[Int])]
    assertEquals(qfʹ.unique.transact(xa).unsafeRunSync(), (("foo", 1, None, Some(42))))
  }

}
