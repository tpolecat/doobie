// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

/*
// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.effect.IO
import doobie.Transactor
import TestTypes.*
import doobie.syntax.*

import scala.util.Using
import java.sql.{DriverManager, ResultSet}
import FixmeSuite.*
import munit.Location

class FixmeSuite extends munit.FunSuite {
  import Rr.*

  val rrLenStr1: Rr[LenStr1] = Comp[LenStr1](
    List(ret[Int], ret[String]),
    items => {
      val x = new Go(items)
      LenStr1(x(0), x(1))
    })

  val orr: Rr[Option[LenStr1]] = rrLenStr1.toOpt

  val rrStr2: Rr[Str2] = Comp[Str2](
    List(ret[Int], rrLenStr1),
    items => {
      val x = new Go(items)
      Str2(x(0), x(1))
    }
  )

  val rrStrO2: Rr[StrO2] = Comp[StrO2](
    List(ret[Int], rrLenStr1.toOpt),
    items => {
      val x = new Go(items)
      StrO2(x(0), x(1))
    }
  )

  val rrSimpleCaseClass: Rr[SimpleCaseClass] = Comp[SimpleCaseClass](
    List(roet[Int], ret[String], roet[String]),
    items => {
      val x = new Go(items)
      SimpleCaseClass(x(0), x(1), x(2))
    }
  )

  val rrOptSimpleCaseClass: Rr[Option[SimpleCaseClass]] = rrSimpleCaseClass.toOpt

  test("select") {
    runQuery("select 1, 'a'") { rs =>
      assertEquals(rrLenStr1.unsafeGet(rs, 1), LenStr1(1, "a"))
      assertEquals(orr.unsafeGet(rs, 1), Some(LenStr1(1, "a")))
    }
  }

  test("select nest") {
    runQuery("select 3, 1, 'a'") { rs =>
      assertEquals(rrStr2.unsafeGet(rs, 1), Str2(3, LenStr1(1, "a")))
      assertEquals(rrStrO2.unsafeGet(rs, 1), StrO2(3, Some(LenStr1(1, "a"))))
    }

    runQuery("select 3, null, 'a'") { rs =>
      assertEquals(rrStrO2.unsafeGet(rs, 1), StrO2(3, None))
    }

    runQuery("select 3, null, 'a'") { rs =>
      assertEquals(rrStrO2.unsafeGet(rs, 1), StrO2(3, None))
    }

    assertResult("select 1, 's', 2", rrSimpleCaseClass, SimpleCaseClass(Some(1), "s", Some("2")))
    assertResult("select null, 's', 2", rrOptSimpleCaseClass, Some(SimpleCaseClass(None, "s", Some("2"))))
    assertResult("select 1, 's', null", rrOptSimpleCaseClass, Some(SimpleCaseClass(Some(1), "s", None)))
    assertResult("select 1, null, 2", rrOptSimpleCaseClass, None)
  }

  private def assertResult[A](sql: String, rr: Rr[A], expected: A)(implicit loc: Location): Unit = {
    runQuery(sql) { rs =>
      assertEquals(rr.unsafeGet(rs, 1), expected)
    }
  }

  private def runQuery(sql: String)(f: ResultSet => Unit): Unit = {
    Class.forName("org.h2.Driver")
    Using.resource(DriverManager.getConnection("jdbc:h2:mem:queryspec;DB_CLOSE_DELAY=-1", "sa", "")) { conn =>
      Using.resource(conn.prepareStatement(sql)) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          rs.next()
          f(rs)
        }
      }
    }
  }

  class Go(l: List[Any]) {
    def apply[A](i: Int): A = l(i).asInstanceOf[A]
  }
}

object FixmeSuite {
  case class Str2(i: Int, ls: LenStr1)
  case class StrO2(i: Int, ls: Option[LenStr1])
}
*/
