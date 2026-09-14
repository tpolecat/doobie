// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package org.typelevel.doobie.util

import org.typelevel.doobie.implicits.*
import cats.syntax.all.*
import cats.effect.IO
import org.typelevel.doobie.{Transactor, Update}
import org.typelevel.doobie.free.preparedstatement as IFPS
import munit.CatsEffectSuite

class UpdateSuite extends CatsEffectSuite {
  val xa: Transactor[IO] = Transactor.fromDriverManager[IO](
    driver = "org.h2.Driver",
    url = "jdbc:h2:mem:;DB_CLOSE_DELAY=-1",
    user = "sa",
    password = "",
    logHandler = None
  )

  test("Update runAlteringExecution") {
    var didRun = false
    (for {
      _ <- sql"create temp table t1 (a int)".update.run
      res <- Update[Int]("insert into t1 (a) values (?)").runAlteringExecution(
        1,
        pe => pe.copy(exec = IFPS.delay { didRun = true } *> pe.exec))
    } yield {
      assertEquals(res, 1L)
    })
      .transact(xa)
      .flatMap { _ =>
        IO(assert(didRun))
      }
  }

  test("Update updateManyAlteringExecution") {
    var didRun = false
    (for {
      _ <- sql"create temp table t1 (a int)".update.run
      res <- Update[Int]("insert into t1 (a) values (?)").updateManyAlteringExecution(
        List(2, 4, 6, 8),
        pe => pe.copy(exec = IFPS.delay { didRun = true } *> pe.exec))
    } yield {
      assertEquals(res, 4L)
    })
      .transact(xa)
      .flatMap { _ =>
        IO(assert(didRun))
      }
  }

  test("Update withUniqueGeneratedKeysAlteringExecution") {
    var didRun = false
    (for {
      _ <- sql"create temp table t1 (a int, b int)".update.run
      res <- Update[(Int, Int)]("insert into t1 (a, b) values (?, ?)")
        .withUniqueGeneratedKeysAlteringExecution[(Int, Int)]("a", "b")(
          (5, 6),
          pe => pe.copy(exec = IFPS.delay { didRun = true } *> pe.exec)
        )
    } yield {
      assertEquals(res, (5, 6))
    })
      .transact(xa)
      .flatMap { _ =>
        IO(assert(didRun))
      }
  }

  test("Update0 runAlteringExecution") {
    var didRun = false
    (for {
      _ <- sql"create temp table t1 (a int)".update.run
      res <- Update[Int]("insert into t1 (a) values (?)").toUpdate0(1).runAlteringExecution(pe =>
        pe.copy(exec = IFPS.delay { didRun = true } *> pe.exec))
    } yield {
      assertEquals(res, 1L)
    })
      .transact(xa)
      .flatMap { _ =>
        IO(assert(didRun))
      }
  }

  test("Update0 withUniqueGeneratedKeysAlteringExecution") {
    var didRun = false
    (for {
      _ <- sql"create temp table t1 (a int, b int)".update.run
      res <- Update[(Int, Int)]("insert into t1 (a, b) values (?, ?)")
        .toUpdate0((5, 6))
        .withUniqueGeneratedKeysAlteringExecution[(Int, Int)]("a", "b")(pe =>
          pe.copy(exec = IFPS.delay { didRun = true } *> pe.exec))
    } yield {
      assertEquals(res, (5, 6))
    })
      .transact(xa)
      .flatMap { _ =>
        IO(assert(didRun))
      }
  }

}
