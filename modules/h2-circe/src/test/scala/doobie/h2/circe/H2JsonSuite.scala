// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.h2.circe

import cats.effect.IO
import doobie.*
import doobie.implicits.*
import doobie.util.{Get, Put, Read, Write}
import io.circe.{Decoder, Encoder, Json}

class H2JsonSuite extends munit.FunSuite {

  import cats.effect.unsafe.implicits.global

  val xa = Transactor.fromDriverManager[IO](
    driver = "org.h2.Driver",
    url = "jdbc:h2:mem:testdb",
    user = "sa",
    password = "",
    logHandler = None
  )

  def inOut[A: Write: Read](col: String, a: A) =
    for {
      _ <- Update0(s"CREATE TEMPORARY TABLE TEST (value $col)", None).run
      a0 <- Update[A](s"INSERT INTO TEST VALUES (?)", None).withUniqueGeneratedKeys[A]("value")(a)
    } yield a0

  def testInOut[A](col: String, a: A, t: Transactor[IO])(implicit m: Get[A], p: Put[A]) = {
    test(s"Mapping for $col as ${m.typeStack} - write+read $col as ${m.typeStack}") {
      assertEquals(inOut(col, a).transact(t).attempt.unsafeRunSync(), Right(a))
    }
    test(s"Mapping for $col as ${m.typeStack} - write+read $col as Option[${m.typeStack}] (Some)") {
      assertEquals(inOut[Option[A]](col, Some(a)).transact(t).attempt.unsafeRunSync(), Right(Some(a)))
    }
    test(s"Mapping for $col as ${m.typeStack} - write+read $col as Option[${m.typeStack}] (None)") {
      assertEquals(inOut[Option[A]](col, None).transact(t).attempt.unsafeRunSync(), Right(None))
    }
  }

  {
    import doobie.h2.circe.json.implicits.*
    testInOut("json", Json.obj("something" -> Json.fromString("Yellow")), xa)
  }

  // Explicit Type Checks

  test("json should check ok for read") {
    import doobie.h2.circe.json.implicits.*

    val a = sql"SELECT '{}' FORMAT JSON".query[Json].analysis.transact(xa).unsafeRunSync()
    assertEquals(a.columnTypeErrors, Nil)
  }
  test("json should check ok for write") {
    import doobie.h2.circe.json.implicits.*

    val a = sql"SELECT ${Json.obj()} FORMAT JSON".query[Json].analysis.transact(xa).unsafeRunSync()
    assertEquals(a.parameterTypeErrors, Nil)
  }

  // Encoder / Decoders
  private case class Foo(x: Json)
  private object Foo {
    import doobie.h2.circe.json.implicits.*
    implicit val fooEncoder: Encoder[Foo] = Encoder[Json].contramap(_.x)
    implicit val fooDecoder: Decoder[Foo] = Decoder[Json].map(Foo(_))
    implicit val fooGet: Get[Foo] = h2DecoderGetT[Foo]
    implicit val fooPut: Put[Foo] = h2EncoderPutT[Foo]
  }

  test("fooGet should check ok for read") {
    val a = sql"SELECT '{}' FORMAT JSON".query[Foo].analysis.transact(xa).unsafeRunSync()
    assertEquals(a.columnTypeErrors, Nil)
  }
  test("fooPut check ok for write") {
    val a = sql"SELECT ${Foo(Json.obj())} FORMAT JSON".query[Foo].analysis.transact(xa).unsafeRunSync()
    assertEquals(a.parameterTypeErrors, Nil)
  }

}
