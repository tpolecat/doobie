// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.implicits._
import cats.effect.{ ContextShift, IO }
import com.github.ghik.silencer.silent
import doobie._, doobie.implicits._
import doobie.util.log.{ LogEvent, Success, ProcessingFailure }
import org.specs2.mutable.Specification
import scala.concurrent.ExecutionContext
import shapeless._

@silent // deprecation
@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.DefaultArguments", "org.wartremover.warts.Var"))
object logspec extends Specification {

  implicit def contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  val xa = Transactor.fromDriverManager[IO](
    "org.h2.Driver",
    "jdbc:h2:mem:queryspec;DB_CLOSE_DELAY=-1",
    "sa", ""
  )

  def eventForUniqueQuery[A: Write](sql: String, arg: A = HNil : HNil): LogEvent = {
    var result  = null : LogEvent
    val handler = LogHandler(result = _)
    val cio     = Query[A, HNil](sql, None, handler).unique(arg)
    cio.transact(xa).attempt.unsafeRunSync
    result
  }

  def eventForUniqueUpdate[A: Write](sql: String, arg: A = HNil : HNil): LogEvent = {
    var result  = null : LogEvent
    val handler = LogHandler(result = _)
    val cio     = sql"create table if not exists foo (bar integer)".update.run *>
                  Update[A](sql, None, handler).run(arg)
    cio.transact(xa).attempt.unsafeRunSync
    result
  }

  "query" >> {

    "default handler" in {
      sql"select 1".query[Int]
      true // compilation test only
    }

    "implicit handler" in {
      var result  = null : LogEvent
      implicit val handler: LogHandler = LogHandler(result = _)
      val cio = sql"select 1".query[Int].unique
      cio.transact(xa).attempt.unsafeRunSync
      result must beLike {
        case Success(_, _, _, _) => ok
      }
    }

    "implicit handler" in {
      var result  = null : LogEvent
      val handler = LogHandler(result = _)
      val cio = sql"select 1".queryWithLogHandler[Int](handler).unique
      cio.transact(xa).attempt.unsafeRunSync
      result must beLike {
        case Success(_, _, _, _) => ok
      }
    }

    "zero-arg success" in {
      val Sql = "select 1"
      eventForUniqueQuery(Sql) must beLike {
        case Success(Sql, Nil, _, _) => ok
      }
    }

    "n-arg success" in {
      val Sql = "select 1 where ? = ?"
      val Arg = 1 :: 1 :: HNil
      eventForUniqueQuery(Sql, Arg) must beLike {
        case Success(Sql, List(1, 1), _, _) => ok
      }
    }

    "zero-arg execution failure" in {
      pending
    }

    "n-arg execution failure" in {
      pending
    }

    "zero-arg processing failure" in {
      val Sql = "select 1 where 1 = 2"
      eventForUniqueQuery(Sql) must beLike {
        case ProcessingFailure(Sql, Nil, _, _, _) => ok
      }
    }

    "n-arg processing failure" in {
      val Sql = "select 1 where ? = ?"
      val Arg = 1 :: 2 :: HNil
      eventForUniqueQuery(Sql, Arg) must beLike {
        case ProcessingFailure(Sql, List(1, 2), _, _, _) => ok
      }
    }

  }

  "update" >> {

    "default handler" in {
      sql"drop table if exists barf".update
      true // compilation test only
    }

    "implicit handler" in {
      var result  = null : LogEvent
      implicit val handler: LogHandler = LogHandler(result = _)
      val cio = sql"drop table if exists barf".update.run
      cio.transact(xa).attempt.unsafeRunSync
      result must beLike {
        case Success(_, _, _, _) => ok
      }
    }

    "implicit handler" in {
      var result  = null : LogEvent
      val handler = LogHandler(result = _)
      val cio = sql"drop table if exists barf".updateWithLogHandler(handler).run
      cio.transact(xa).attempt.unsafeRunSync
      result must beLike {
        case Success(_, _, _, _) => ok
      }
    }

    "zero-arg success" in {
      val Sql = "update foo set bar = 42"
      eventForUniqueUpdate(Sql) must beLike {
        case Success(Sql, Nil, _, _) => ok
      }
    }

    "n-arg success" in {
      val Sql = "update foo set bar = ?"
      val Arg = 42 :: HNil
      eventForUniqueUpdate(Sql, Arg) must beLike {
        case Success(Sql, List(42), _, _) => ok
      }
    }

    "zero-arg execution failure" in {
      pending
    }

    "n-arg execution failure" in {
      pending
    }

  }

}
