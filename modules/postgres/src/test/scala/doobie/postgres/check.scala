// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import cats.effect.{ ContextShift, IO }
import doobie._, doobie.implicits._
import doobie.postgres.implicits._
import org.specs2.mutable.Specification
import scala.concurrent.ExecutionContext

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
object pgcheck extends Specification {

  implicit def contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  val xa = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:world",
    "postgres", ""
  )

  // create type myenum as enum ('foo', 'bar') <-- part of setup
  sealed trait MyEnum
  case object Foo extends MyEnum
  case object Bar extends MyEnum
  object MyEnum {
    implicit val MyEnumMeta: Meta[MyEnum] =
      pgEnumString("myenum", {
        case "foo" => Foo
        case "bar" => Bar
      }, {
        case Foo => "foo"
        case Bar => "bar"
      })
  }

  "pgEnumString" should {

    "check ok for read" in {
      val a = sql"select 'foo' :: myenum".query[MyEnum].analysis.transact(xa).unsafeRunSync
      a.columnTypeErrors must_== Nil
    }

    "check ok for write" in {
      val a = sql"select ${Foo : MyEnum} :: myenum".query[MyEnum].analysis.transact(xa).unsafeRunSync
      a.parameterTypeErrors must_== Nil
    }

  }

}
