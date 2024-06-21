// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import doobie._
import doobie.implicits._
import doobie.postgres.implicits._
import doobie.postgres.enums._
import doobie.util.invariant._

class ReadErrorSuite extends munit.FunSuite {
  import cats.effect.unsafe.implicits.global
  import PostgresTestTransactor.xa

  implicit val MyEnumMetaOpt: Meta[MyEnum] = pgEnumStringOpt(
    "myenum",
    {
      case "foo" => Some(MyEnum.Foo)
      case "bar" => Some(MyEnum.Bar)
      case _     => None
    },
    {
      case MyEnum.Foo => "foo"
      case MyEnum.Bar => "bar"
    })
  implicit val MyScalaEnumMeta: Meta[MyScalaEnum.Value] = pgEnum(MyScalaEnum, "myenum")
  implicit val MyJavaEnumMeta: Meta[MyJavaEnum] = pgJavaEnum[MyJavaEnum]("myenum")

  test("pgEnumStringOpt") {
    val r = sql"select 'invalid'".query[MyEnum].unique.transact(xa).attempt.unsafeRunSync()
    assertEquals(r, Left(InvalidEnum[MyEnum]("invalid")))
  }

  test("pgEnum") {
    val r = sql"select 'invalid' :: myenum".query[MyScalaEnum.Value].unique.transact(xa).attempt.unsafeRunSync()
    assertEquals(r, Left(InvalidEnum[MyScalaEnum.Value]("invalid")))
  }

  test("pgJavaEnum") {
    val r = sql"select 'invalid' :: myenum".query[MyJavaEnum].unique.transact(xa).attempt.unsafeRunSync()
    assertEquals(r, Left(InvalidEnum[MyJavaEnum]("invalid")))
  }

}
