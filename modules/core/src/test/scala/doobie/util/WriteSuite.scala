// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import doobie.util.meta.Meta

class WriteSuite extends munit.FunSuite with WriteSuitePlatform {

  case class LenStr1(n: Int, s: String)

  case class LenStr2(n: Int, s: String)
  object LenStr2 {
    implicit val LenStrMeta: Meta[LenStr2] =
      Meta[String].timap(s => LenStr2(s.length, s))(_.s)
  }

  test("Write should exist for some fancy types") {
    import doobie.generic.auto._

    Write[Int]
    Write[(Int, Int)]
    Write[(Int, Int, String)]
    Write[(Int, (Int, String))]
  }

  test("Write is not auto derived for tuples without an import") {
    assert(compileErrors("Write[(Int, Int)]").contains("Cannot find or construct"))
    assert(compileErrors("Write[(Int, Int, String)]").contains("Cannot find or construct"))
    assert(compileErrors("Write[(Int, (Int, String))]").contains("Cannot find or construct"))
  }

  test("Write is not auto derived for case classes") {
    assert(compileErrors("Write[LenStr1]").contains("Cannot find or construct"))
  }

  test("Write can be manually derived") {
    Write.derived[LenStr1]
  }

  test("Write should exist for Unit") {
    import doobie.generic.auto._

    Write[Unit]
    assertEquals(Write[(Int, Unit)].length, 1)
  }

  test("Write should exist for option of some fancy types") {
    import doobie.generic.auto._

    Write[Option[Int]]
    Write[Option[(Int, Int)]]
    Write[Option[(Int, Int, String)]]
    Write[Option[(Int, (Int, String))]]
    Write[Option[(Int, Option[(Int, String)])]]
  }

  test("Write should exist for option of Unit") {
    import doobie.generic.auto._

    Write[Option[Unit]]
    assertEquals(Write[Option[(Int, Unit)]].length, 1)
  }

  test("Write should select multi-column instance by default") {
    import doobie.generic.auto._

    assertEquals(Write[LenStr1].length, 2)
  }

  test("Write should select 1-column instance when available") {
    assertEquals(Write[LenStr2].length, 1)
  }

}
