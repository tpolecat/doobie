// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import cats.effect.{ ContextShift, IO }
import cats.implicits._
import doobie._, doobie.implicits._
import doobie.postgres.implicits._
import fs2._
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import scala.concurrent.ExecutionContext


class textspec extends Specification with ScalaCheck {

  implicit def contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  val xa = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:world",
    "postgres", ""
  )

  implicit val byteListInstance: Text[List[Byte]] =
    Text[Array[Byte]].contramap(_.toArray)

  val create: ConnectionIO[Unit] =
    sql"""| CREATE TEMPORARY TABLE test (
          |  id serial,  -- just for ordering
          |   a text,    -- String
          |   b int2,    -- Short
          |   c int4,    -- Int
          |   d int8,    -- Long
          |   e float4,  -- Float
          |   f float8,  -- Double
          |   g numeric, -- BigDecimal
          |   h boolean, -- Boolean
          |   i bytea,   -- List[Byte]
          |   j _text,   -- List[String]
          |   k _int4    -- List[Int]
          | ) ON COMMIT DELETE ROWS
          |""".stripMargin.update.run.void

  val insert: Fragment[Any] =
    sql"""| COPY test (a, b, c, d, e, f, g, h, i, j, k)
          | FROM STDIN
          |""".stripMargin

  val selectAll: ConnectionIO[List[Row]] =
    sql"SELECT a, b, c, d, e, f, g, h, i, j, k FROM test ORDER BY id ASC".query[Row].to[List]

  // A test type to insert, all optional so we can check NULL
  final case class Row(
    a: Option[String],
    b: Option[Short],
    c: Option[Int],
    d: Option[Long],
    e: Option[Float],
    f: Option[Double],
    g: Option[BigDecimal],
    h: Option[Boolean],
    i: Option[List[Byte]],
    j: Option[List[String]],
    k: Option[List[Int]]
  )

  // filter chars pg can't cope with
  def filter(s: String): String =
    s.replace("\u0000", "") // NUL
     .toList
     .map { c => if (Character.isSpaceChar(c)) ' ' else c } // high space
     .filterNot(c => c >= 0x0E && c <= 0x1F) // low ctrl
     .mkString

  val genRow: Gen[Row] =
    for {
      a <- arbitrary[Option[String]].map(_.map(filter))
      b <- arbitrary[Option[Short]]
      c <- arbitrary[Option[Int]]
      d <- arbitrary[Option[Long]]
      e <- arbitrary[Option[Float]]
      f <- arbitrary[Option[Double]]
      g <- arbitrary[Option[BigDecimal]]
      h <- arbitrary[Option[Boolean]]
      i <- arbitrary[Option[List[Byte]]]
      j <- arbitrary[Option[List[String]]].map(_.map(_.map(filter)))
      k <- arbitrary[Option[List[Int]]]
    } yield Row(a, b, c, d, e, f, g, h, i, j, k)

  val genRows: Gen[List[Row]] =
    Gen.choose(0,50).flatMap(Gen.listOfN(_, genRow))

  "copyIn" should {
    "correctly insert batches of rows" in forAll(genRows) { rs =>
      val rsʹ = (create *> insert.copyIn(rs) *> selectAll).transact(xa).unsafeRunSync
      rs must_=== rsʹ
    }

    "correctly insert batches of rows via Stream" in forAll(genRows) { rs =>
      val rsʹ = (create *> insert.copyIn(Stream.emits[IO, Row](rs), 100) *> selectAll).transact(xa).unsafeRunSync
      rs must_=== rsʹ
    }
  }

}
