// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package org.typelevel.doobie.util

import org.typelevel.doobie.{ConnectionIO, Query, Transactor, Update}
import org.typelevel.doobie.util.TestTypes.*
import cats.effect.IO
import org.typelevel.doobie.testutils.VoidExtensions
import org.typelevel.doobie.syntax.all.*
import org.typelevel.doobie.util.analysis.{Analysis, ParameterMisalignment, ParameterTypeError}
import munit.Location

import scala.annotation.nowarn

class WriteSuite extends munit.CatsEffectSuite with WriteSuitePlatform {

  val xa: Transactor[IO] = Transactor.fromDriverManager[IO](
    driver = "org.h2.Driver",
    url = "jdbc:h2:mem:;DB_CLOSE_DELAY=-1",
    user = "sa",
    password = "",
    logHandler = None
  )

  test("Write is available for tuples without an import when all elements have a Write instance") {
    Write[(Int, Int)].void
    Write[(Int, Int, String)].void
    Write[(Int, (Int, String))].void

    Write[Option[(Int, Int)]].void
    Write[Option[(Int, Option[(String, Int)])]].void

    // But shouldn't automatically derive anything that doesn't already have a Read instance
    assert(compileErrors("Write[(Int, TrivialCaseClass)]").contains("Cannot find or construct"))
  }

  test("Write is still auto derived for tuples when import is present (no ambiguous implicits) ") {
    import org.typelevel.doobie.implicits.*
    Write[(Int, Int)].void
    Write[(Int, Int, String)].void
    Write[(Int, (Int, String))].void

    Write[Option[(Int, Int)]].void
    Write[Option[(Int, Option[(String, Int)])]].void

    Write[(ComplexCaseClass, Int)].void
    Write[(Int, ComplexCaseClass)].void
  }

  test("Write is not auto derived for case classes") {
    assert(compileErrors("Write[TrivialCaseClass]").contains("Cannot find or construct"))
  }

  test("Semiauto derivation selects custom Write instances when available") {
    implicit val i0: Write[HasCustomReadWrite0] = Write.derived[HasCustomReadWrite0]
    implicit val i1: Write[HasCustomReadWrite1] = Write.derived[HasCustomReadWrite1]
    implicit val iOpt0: Write[HasOptCustomReadWrite0] = Write.derived[HasOptCustomReadWrite0]
    implicit val iOpt1: Write[HasOptCustomReadWrite1] = Write.derived[HasOptCustomReadWrite1]

    IO {
      assertEquals(i0.length, 2)
    } *>
      writeAndCheckTuple2(HasCustomReadWrite0(CustomReadWrite("x"), "y"), ("x_W", "y")) *>
      IO {
        assertEquals(i1.length, 2)
      } *>
      writeAndCheckTuple2(HasCustomReadWrite1("x", CustomReadWrite("y")), ("x", "y_W")) *>
      IO {
        assertEquals(iOpt0.length, 2)
      } *>
      writeAndCheckTuple2(HasOptCustomReadWrite0(Some(CustomReadWrite("x")), "y"), ("x_W", "y")) *>
      IO {
        assertEquals(iOpt1.length, 2)
      } *>
      writeAndCheckTuple2(HasOptCustomReadWrite1("x", Some(CustomReadWrite("y"))), ("x", "y_W"))
  }

  test("Semiauto derivation selects custom Put instances to use for Write when available") {
    implicit val i0: Write[HasCustomGetPut0] = Write.derived[HasCustomGetPut0]
    implicit val i1: Write[HasCustomGetPut1] = Write.derived[HasCustomGetPut1]
    implicit val iOpt0: Write[HasOptCustomGetPut0] = Write.derived[HasOptCustomGetPut0]
    implicit val iOpt1: Write[HasOptCustomGetPut1] = Write.derived[HasOptCustomGetPut1]

    IO { assertEquals(i0.length, 2) } *>
      writeAndCheckTuple2(HasCustomGetPut0(CustomGetPut("x"), "y"), ("x_P", "y")) *>
      IO { assertEquals(i1.length, 2) } *>
      writeAndCheckTuple2(HasCustomGetPut1("x", CustomGetPut("y")), ("x", "y_P")) *>
      IO { assertEquals(iOpt0.length, 2) } *>
      writeAndCheckTuple2(HasOptCustomGetPut0(Some(CustomGetPut("x")), "y"), ("x_P", "y")) *>
      IO { assertEquals(iOpt1.length, 2) } *>
      writeAndCheckTuple2(HasOptCustomGetPut1("x", Some(CustomGetPut("y"))), ("x", "y_P"))
  }

  test("Automatic derivation selects custom Write instances when available") {
    import org.typelevel.doobie.implicits.*

    writeAndCheckTuple2(HasCustomReadWrite0(CustomReadWrite("x"), "y"), ("x_W", "y")) *>
      writeAndCheckTuple2(HasCustomReadWrite1("x", CustomReadWrite("y")), ("x", "y_W")) *>
      writeAndCheckTuple2(HasOptCustomReadWrite0(Some(CustomReadWrite("x")), "y"), ("x_W", "y")) *>
      writeAndCheckTuple2(HasOptCustomReadWrite1("x", Some(CustomReadWrite("y"))), ("x", "y_W"))
  }

  test("Automatic derivation selects custom Put instances to use for Write when available") {
    import org.typelevel.doobie.implicits.*
    writeAndCheckTuple2(HasCustomGetPut0(CustomGetPut("x"), "y"), ("x_P", "y")) *>
      writeAndCheckTuple2(HasCustomGetPut1("x", CustomGetPut("y")), ("x", "y_P")) *>
      writeAndCheckTuple2(HasOptCustomGetPut0(Some(CustomGetPut("x")), "y"), ("x_P", "y")) *>
      writeAndCheckTuple2(HasOptCustomGetPut1("x", Some(CustomGetPut("y"))), ("x", "y_P"))
  }

  test("Write should not be derivable for case objects or empty case classes") {
    val expectedDeriveError =
      ScalaBinaryVersion.currentVersion match {
        case ScalaBinaryVersion.S2_12 => "could not find implicit"
        case ScalaBinaryVersion.S2_13 => "Cannot derive"
        case ScalaBinaryVersion.S3    => "Cannot derive"
      }
    assert(compileErrors("Write.derived[CaseObj.type]").contains(expectedDeriveError))
    assert(compileErrors("Write.derived[ZeroFieldCaseClass]").contains(expectedDeriveError))

    val expectedErrorWithAutoDerivationEnabled = ScalaBinaryVersion.currentVersion match {
      case ScalaBinaryVersion.S2_12 => "Cannot find or construct"
      case ScalaBinaryVersion.S2_13 => "Cannot find or construct"
      case ScalaBinaryVersion.S3    => "Cannot derive"
    }
    import org.typelevel.doobie.implicits.*
    assert(compileErrors("Write[CaseObj.type]").contains(expectedErrorWithAutoDerivationEnabled))
    assert(compileErrors("Write[Option[CaseObj.type]]").contains(expectedErrorWithAutoDerivationEnabled))
    assert(compileErrors("Write[Option[ZeroFieldCaseClass]]").contains(expectedErrorWithAutoDerivationEnabled))
    assert(compileErrors("Write.derived[CaseObj.type]").contains(expectedDeriveError))
    assert(compileErrors("Write.derived[ZeroFieldCaseClass]").contains(expectedDeriveError))
  }: @nowarn("msg=.*(u|U)nused import.*")

  case class Boo(i: Int, s: CaseObj.type)

  test("Write should exist for Unit/Option[Unit]") {
    assertEquals(Write[Unit].length, 0)
    assertEquals(Write[Option[Unit]].length, 0)
    assertEquals(Write[(Int, Unit)].length, 1)
  }

  test("Write should correctly set parameters for Option instances ") {
    import org.typelevel.doobie.implicits.*
    (for {
      _ <- sql"create temp table t1 (a int, b int)".update.run
      _ <- Update[Option[(Int, Int)]]("insert into t1 (a, b) values (?, ?)").run(Some((1, 2)))
      _ <- Update[Option[(Option[Int], Int)]]("insert into t1 (a, b) values (?, ?)").run(Some((None, 4)))
      _ <- Update[Option[(Int, Option[Int])]]("insert into t1 (a, b) values (?, ?)").run(Some((5, None)))
      _ <- Update[Option[(Option[Int], Int)]]("insert into t1 (a, b) values (?, ?)").run(None)
      _ <- Update[Option[(Int, Option[Int])]]("insert into t1 (a, b) values (?, ?)").run(None)
      res <- sql"select a, b from t1 order by a asc nulls last".query[(Option[Int], Option[Int])].to[List]
    } yield {
      assertEquals(
        res,
        List(
          (Some(1), Some(2)),
          (Some(5), None),
          (None, Some(4)),
          (None, None),
          (None, None)
        ))
    })
      .transact(xa)

  }

  test("Write should yield correct error when Some(null) inserted") {
    interceptMessageIO[RuntimeException](
      "Expected non-nullable param at 2. Use Option to describe nullable values."
    )(testNullPut(("a", Some(null))))
  }

  test("Write should yield correct error when null inserted into non-nullable field") {
    interceptMessageIO[RuntimeException]("Expected non-nullable param at 1. Use Option to describe nullable values.") {
      testNullPut((null, Some("b")))
    }
  }

  test(".contramap correctly transformers the input value") {
    import org.typelevel.doobie.implicits.*
    implicit val w: Write[WrappedSimpleCaseClass] = Write[SimpleCaseClass].contramap(v =>
      v.sc.copy(
        s = "custom"
      ))

    writeAndCheckTuple3(WrappedSimpleCaseClass(SimpleCaseClass(Some(1), "s1", Some("s2"))), (1, "custom", "s2"))
  }

  test("Write typechecking should work for tuples") {
    val createTable = sql"create temp table tab(c1 int, c2 varchar not null, c3 double)".update.run
    val createAllNullableTable = sql"create temp table tab(c1 int, c2 varchar, c3 double)".update.run
    val insertSql = "INSERT INTO tab VALUES (?,?,?)"

    assertSuccessTypecheckWrite(
      createTable.flatMap(_ => Update[(Option[Int], String, Double)](insertSql).analysis)) *>
      assertSuccessTypecheckWrite(
        createTable.flatMap(_ => Update[((Option[Int], String), Double)](insertSql).analysis)) *>
      assertSuccessTypecheckWrite(
        createTable.flatMap(_ => Update[(Option[Int], String, Option[Double])](insertSql).analysis)) *>
      assertSuccessTypecheckWrite(
        createAllNullableTable.flatMap(_ =>
          Update[(Option[Int], Option[String], Option[Double])](insertSql).analysis)) *>
      assertSuccessTypecheckWrite(
        createAllNullableTable.flatMap(_ => Update[Option[(Option[Int], String, Double)]](insertSql).analysis)) *>
      assertSuccessTypecheckWrite(
        createAllNullableTable.flatMap(_ => Update[Option[(Int, Option[(String, Double)])]](insertSql).analysis)) *>
      assertMisalignedTypecheckWrite(createTable.flatMap(_ => Update[(Option[Int], String)](insertSql).analysis)) *>
      assertMisalignedTypecheckWrite(createTable.flatMap(_ =>
        Update[(Option[Int], String, Double, Int)](insertSql).analysis)) *>
      assertTypeErrorTypecheckWrite(
        sql"create temp table tab(c1 binary not null, c2 varchar not null, c3 int)".update.run.flatMap(_ =>
          Update[(Int, String, Option[Int])](insertSql).analysis)
      )
  }

  test("Write typechecking should work for case classes") {
    implicit val wscc: Write[SimpleCaseClass] = Write.derived[SimpleCaseClass]
    implicit val wccc: Write[ComplexCaseClass] = Write.derived[ComplexCaseClass]
    implicit val wwscc: Write[WrappedSimpleCaseClass] =
      wscc.contramap(_.sc) // Testing contramap doesn't break typechecking

    val createTable: ConnectionIO[Long] = sql"create temp table tab(c1 int, c2 varchar not null, c3 varchar)".update.run

    val insertSimpleSql = "INSERT INTO tab VALUES (?,?,?)"
    val insertComplexSql = "INSERT INTO tab VALUES (?,?,?,?,?,?,?,?)"

    assertSuccessTypecheckWrite(createTable.flatMap(_ => Update[SimpleCaseClass](insertSimpleSql).analysis)) *>
      assertSuccessTypecheckWrite(createTable.flatMap(_ => Update[WrappedSimpleCaseClass](insertSimpleSql).analysis)) *>
      // This shouldn't pass but JDBC driver (at least for h2) doesn't tell us when a parameter should be not-nullable
      assertSuccessTypecheckWrite(
        createTable.flatMap(_ => Update[Option[SimpleCaseClass]](insertSimpleSql).analysis)) *>
      assertSuccessTypecheckWrite(createTable.flatMap(_ =>
        Update[Option[WrappedSimpleCaseClass]](insertSimpleSql).analysis)) *>
      assertSuccessTypecheckWrite(
        sql"create temp table tab(c1 int, c2 varchar, c3 varchar, c4 int, c5 varchar, c6 varchar, c7 int, c8 varchar not null)"
          .update.run
          .flatMap(_ => Update[ComplexCaseClass](insertComplexSql).analysis)
      ) *>
      assertTypeErrorTypecheckWrite(
        sql"create temp table tab(c1 int, c2 varchar, c3 varchar, c4 BINARY, c5 varchar, c6 varchar, c7 int, c8 varchar not null)"
          .update.run
          .flatMap(_ => Update[ComplexCaseClass](insertComplexSql).analysis)
      )
  }

  test("Derivation for big case class works") {
    Write.derived[Big30CaseClass].void

    import org.typelevel.doobie.implicits.*
    Write[Big30CaseClass].void
  }

  private def assertSuccessTypecheckWrite(connio: ConnectionIO[Analysis])(implicit loc: Location): IO[Unit] = {
    connio.transact(xa).map(_.parameterAlignmentErrors).assertEquals(Nil)
  }

  private def assertMisalignedTypecheckWrite(connio: ConnectionIO[Analysis])(implicit loc: Location): IO[Unit] = {
    connio.transact(xa).map(_.parameterAlignmentErrors.map(_.getClass)).assertEquals(List(
      classOf[ParameterMisalignment]))
  }

  private def assertTypeErrorTypecheckWrite(connio: ConnectionIO[Analysis])(implicit loc: Location): IO[Unit] = {
    connio.transact(xa).map(_.parameterAlignmentErrors.map(_.getClass)).assertEquals(List(classOf[ParameterTypeError]))
  }

  private def writeAndCheckTuple2[A: Write, Tup <: (?, ?): Read](in: A, expectedOut: Tup)(implicit
      loc: Location
  ): IO[Unit] =
    Query[A, Tup]("SELECT ?, ?").unique(in).transact(xa).assertEquals(expectedOut)

  private def writeAndCheckTuple3[A: Write, Tup <: (?, ?, ?): Read](in: A, expectedOut: Tup)(implicit
      loc: Location
  ): IO[Unit] =
    Query[A, Tup]("SELECT ?, ?, ?").unique(in).transact(xa).assertEquals(expectedOut)

  private def testNullPut(input: (String, Option[String])): IO[Long] = {
    import org.typelevel.doobie.implicits.*

    (for {
      _ <- sql"create temp table t0 (a text, b text null)".update.run
      n <- Update[(String, Option[String])]("insert into t0 (a, b) values (?, ?)").run(input)
    } yield n)
      .transact(xa)
  }

}
