// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.effect.IO
import doobie.util.meta.Meta
import doobie.util.transactor.Transactor

class ReadSuite extends munit.FunSuite with ReadSuitePlatform {

  import cats.effect.unsafe.implicits.global

  case class LenStr1(n: Int, s: String)

  case class LenStr2(n: Int, s: String)
  object LenStr2 {
    implicit val LenStrMeta: Meta[LenStr2] =
      Meta[String].timap(s => LenStr2(s.length, s))(_.s)
  }

  val xa = Transactor.fromDriverManager[IO](
    driver = "org.h2.Driver",
    url = "jdbc:h2:mem:;DB_CLOSE_DELAY=-1",
    user = "sa",
    password = "",
    logHandler = None
  )

  test("Read should exist for some fancy types") {
    import doobie.generic.auto._

    Read[Int]
    Read[(Int, Int)]
    Read[(Int, Int, String)]
    Read[(Int, (Int, String))]
  }

  test("Read is not auto derived for case classes without importing auto derive import") {
    assert(compileErrors("Read[LenStr1]").contains("Cannot find or construct"))
  }

  test("Read is not auto derived for tuples without an import") {
    assert(compileErrors("Read[(Int, Int)]").contains("Cannot find or construct"))
    assert(compileErrors("Read[(Int, Int, String)]").contains("Cannot find or construct"))
    assert(compileErrors("Read[(Int, (Int, String))]").contains("Cannot find or construct"))
  }

  test("Read can be manually derived") {
    Read.derived[LenStr1]
  }

  test("Read should exist for Unit") {
    import doobie.generic.auto._

    Read[Unit]
    assertEquals(Read[(Int, Unit)].length, 1)
  }

  test("Read should exist for option of some fancy types") {
    import doobie.generic.auto._

    Read[Option[Int]]
    Read[Option[(Int, Int)]]
    Read[Option[(Int, Int, String)]]
    Read[Option[(Int, (Int, String))]]
    Read[Option[(Int, Option[(Int, String)])]]
  }

  test("Read should exist for option of Unit") {
    import doobie.generic.auto._

    Read[Option[Unit]]
    assertEquals(Read[Option[(Int, Unit)]].length, 1)
  }

  test("Read should select multi-column instance by default") {
    import doobie.generic.auto._

    assertEquals(Read[LenStr1].length, 2)
  }

  test("Read should select 1-column instance when available") {
    assertEquals(Read[LenStr2].length, 1)
  }

  test(".product should product the correct ordering of gets") {
    import cats.syntax.all._

    val readInt = Read[Int]
    val readString = Read[String]

    val p = readInt.product(readString)

    assertEquals(p.gets, (readInt.gets ++ readString.gets))
  }

  test("Read should select correct columns when combined with `ap`") {
    import cats.syntax.all._
    import doobie.implicits._

    val r = Read[Int]

    val c = (r, r, r, r, r).tupled

    val q = sql"SELECT 1, 2, 3, 4, 5".query(c).to[List]

    val o = q.transact(xa).unsafeRunSync()

    assertEquals(o, List((1, 2, 3, 4, 5)))
  }

  test("Read should select correct columns when combined with `product`") {
    import cats.syntax.all._
    import doobie.implicits._

    val r = Read[Int].product(Read[Int].product(Read[Int]))

    val q = sql"SELECT 1, 2, 3".query(r).to[List]
    val o = q.transact(xa).unsafeRunSync()

    assertEquals(o, List((1, (2, 3))))
  }

}
