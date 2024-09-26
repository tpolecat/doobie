// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.syntax.all.*
import cats.effect.IO
import doobie.*
import doobie.implicits.*
import doobie.testutils.VoidExtensions

class FragmentSuite extends munit.FunSuite {

  import cats.effect.unsafe.implicits.global

  val xa = Transactor.fromDriverManager[IO](
    driver = "org.h2.Driver",
    url = "jdbc:h2:mem:fragmentspec;DB_CLOSE_DELAY=-1",
    user = "sa",
    password = "",
    logHandler = None
  )

  val a = 1
  val b = "two"
  val c = true

  val fra = fr"$a"
  val frb = fr"$b"
  val frc = fr"$c"

  test("Fragment must substitute placeholders properly") {
    assertEquals(fr"foo $a $b bar".query[Unit].sql, "foo ? ? bar ")
  }

  test("Fragment must concatenate properly") {
    assertEquals((fr"foo" ++ fr"bar $a baz").query[Unit].sql, "foo bar ? baz ")
  }

  // FIXME:
//  test("Fragment must interpolate fragments properly") {
//    assertEquals(fr"foo ${fr0"bar $a baz"}".query[Unit].sql, "foo bar ? baz ")
//  }

  // https://github.com/tpolecat/doobie/issues/1186
  test("Fragment must interpolate an expression `Option(1).getOrElse(2)` properly") {
    sql"${Option(1).getOrElse(2)} ${false} ${"xx"}".void
    fr"${Option(1).getOrElse(2)}".void
    fr0"${Option(1).getOrElse(2)}".void
  }

  test("Fragment must maintain parameter indexing (in-order)") {
    val s = fr"select" ++ List(fra, frb, frc).intercalate(fr",")
    assertEquals(s.query[(Int, String, Boolean)].unique.transact(xa).unsafeRunSync(), ((a, b, c)))
  }

  test("Fragment must maintain parameter indexing (out-of-order)") {
    val s = fr"select" ++ List(frb, frc, fra).intercalate(fr",")
    assertEquals(s.query[(String, Boolean, Int)].unique.transact(xa).unsafeRunSync(), ((b, c, a)))
  }

  test("Fragment must maintain associativity (left)") {
    val s = fr"select" ++ List(fra, fr",", frb, fr",", frc).foldLeft(Fragment.empty)(_ ++ _)
    assertEquals(s.query[(Int, String, Boolean)].unique.transact(xa).unsafeRunSync(), ((a, b, c)))
  }

  test("Fragment must maintain associativity (right)") {
    val s = fr"select" ++ List(fra, fr",", frb, fr",", frc).foldRight(Fragment.empty)(_ ++ _)
    assertEquals(s.query[(Int, String, Boolean)].unique.transact(xa).unsafeRunSync(), ((a, b, c)))
  }

  test("Fragment must Add a trailing space when constructed with .const") {
    assertEquals(Fragment.const("foo").query[Int].sql, "foo ")
  }

  test("Fragment must Not add a trailing space when constructed with .const0") {
    assertEquals(Fragment.const0("foo").query[Int].sql, "foo")
  }

  test("Fragment must allow margin stripping") {
    assertEquals(
      fr"""select foo
          |  from bar
          |  where a = $a and b = $b and c = $c
          |""".stripMargin.query[Int].sql,
      "select foo\n  from bar\n  where a = ? and b = ? and c = ?\n "
    )
  }

  test("Fragment must allow margin stripping with custom margin") {
    assertEquals(
      fr"""select foo
          !  from bar
          !""".stripMargin('!').query[Int].sql,
      "select foo\n  from bar\n ")
  }

  test("Fragment must not affect margin characters in middle outside of margin position") {
    assertEquals(
      fr"""select foo || baz
          |  from bar
          |""".stripMargin.query[Int].sql,
      "select foo || baz\n  from bar\n ")
  }

  // A fragment composed of this many sub-fragments would not be stacksafe without special
  // handling, which we test below.
  val STACK_UNSAFE_SIZE = 20000

  test("Fragment must be stacksafe (left-associative)") {
    val frag =
      fr0"SELECT 1 WHERE 1 IN (" ++
        List.fill(STACK_UNSAFE_SIZE)(1).foldLeft(Fragment.empty)((f, n) => f ++ fr"$n,") ++
        fr0"1)"
    assertEquals(frag.query[Int].unique.transact(xa).unsafeRunSync(), 1)
  }

  test("Fragment must be stacksafe (right-associative)") {
    val frag =
      fr0"SELECT 1 WHERE 1 IN (" ++
        List.fill(STACK_UNSAFE_SIZE)(1).foldRight(Fragment.empty)((n, f) => f ++ fr"$n,") ++
        fr0"1)"
    assertEquals(frag.query[Int].unique.transact(xa).unsafeRunSync(), 1)
  }

}
