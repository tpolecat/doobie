// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.syntax.all._
import cats.effect.{IO, IOLocal}
import doobie._
import doobie.implicits._
import doobie.util.log.{LogEvent, ProcessingFailure, Success}

class LogSuite extends munit.FunSuite {

  import cats.effect.unsafe.implicits.global

  val ioLocal: IOLocal[LogEvent] =
    IOLocal[LogEvent](null).unsafeRunSync()

  val xa = Transactor.fromDriverManager[IO](
    "org.h2.Driver",
    "jdbc:h2:mem:queryspec;DB_CLOSE_DELAY=-1",
    "sa",
    "",
    logHandler = Some(ev => ioLocal.set(ev))
  )

  def eventForCIO[A](cio: ConnectionIO[A]): LogEvent =
    cio.transact(xa).attempt.flatMap(_ => ioLocal.get).unsafeRunSync()

  def eventForUniqueQuery[A: Write](sql: String, arg: A): LogEvent = {
    eventForCIO(Query[A, Unit](sql, None).unique(arg))
  }

  def eventForUniqueUpdate[A: Write](sql: String, arg: A): LogEvent = {
    val cio = sql"create table if not exists foo (bar integer)".update.run *>
      Update[A](sql, None).run(arg)
    eventForCIO(cio)
  }

  test("[Query] default handler") {
    sql"select 1".query[Int]
  }

  test("[Query] implicit handler") {
    eventForCIO(sql"select 1".query[Int].unique) match {
      case Success(_, _, _, _, _) => ()
      case a                      => fail(s"no match: $a")
    }
  }

  test("[Query] explicit handler") {
    eventForCIO(sql"select 1".query[Int].unique) match {
      case Success(_, _, _, _, _) => ()
      case a                      => fail(s"no match: $a")
    }
  }

  test("[Query] zero-arg success") {
    val Sql = "select 1"
    eventForUniqueQuery(Sql, ()) match {
      case Success(Sql, Nil, _, _, _) => ()
      case a                          => fail(s"no match: $a")
    }
  }

  test("[Query] zero-arg execution failure".ignore) {
    ()
  }

  test("[Query] n-arg execution failure".ignore) {
    ()
  }

  test("[Query] zero-arg processing failure") {
    val Sql = "select 1 where 1 = 2"
    eventForUniqueQuery(Sql, ()) match {
      case ProcessingFailure(Sql, Nil, _, _, _, _) => ()
      case a                                       => fail(s"no match: $a")
    }
  }

  test("[Update] default handler") {
    sql"drop table if exists barf".update
  }

  test("[Update] implicit handler") {
    val cio = sql"drop table if exists barf".update.run
    eventForCIO(cio) match {
      case Success(_, _, _, _, _) => ()
      case a                      => fail(s"no match: $a")
    }
  }

  test("[Update] explicit handler") {
    val cio = sql"drop table if exists barf".update.run
    eventForCIO(cio) match {
      case Success(_, _, _, _, _) => ()
      case a                      => fail(s"no match: $a")
    }
  }

  test("[Update] zero-arg success") {
    val Sql = "update foo set bar = 42"
    eventForUniqueUpdate(Sql, ()) match {
      case Success(Sql, Nil, _, _, _) => ()
      case a                          => fail(s"no match: $a")
    }
  }

  test("[Update] zero-arg execution failure".ignore) {
    ()
  }

  test("[Update] n-arg execution failure".ignore) {
    ()
  }

}
