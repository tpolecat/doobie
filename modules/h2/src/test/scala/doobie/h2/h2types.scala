// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.h2

import cats.effect.{ ContextShift, IO }
import com.github.ghik.silencer.silent
import doobie._, doobie.implicits._
import doobie.h2.implicits._
import java.util.UUID
import org.specs2.mutable.Specification
import scala.concurrent.ExecutionContext

// Establish that we can read various types. It's not very comprehensive as a test, bit it's a start.
@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
object h2typesspec extends Specification {

  implicit def contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  val xa = Transactor.fromDriverManager[IO](
    "org.h2.Driver",
    "jdbc:h2:mem:ch3;DB_CLOSE_DELAY=-1",
    "sa", ""
  )

  def inOut[A: Put: Get](col: String, a: A): ConnectionIO[A] =
    for {
      _  <- Update0(s"CREATE LOCAL TEMPORARY TABLE TEST (value $col)", None).run
      _  <- sql"INSERT INTO TEST VALUES ($a)".update.run
      a0 <- sql"SELECT value FROM TEST".query[A].unique
    } yield (a0)

  def inOutOpt[A: Put: Get](col: String, a: Option[A]): ConnectionIO[Option[A]] =
    for {
      _  <- Update0(s"CREATE LOCAL TEMPORARY TABLE TEST (value $col)", None).run
      _  <- sql"INSERT INTO TEST VALUES ($a)".update.run
      a0 <- sql"SELECT value FROM TEST".query[Option[A]].unique
    } yield (a0)

  def testInOut[A](col: String, a: A)(implicit m: Get[A], p: Put[A]) =
    s"Mapping for $col as ${m.typeStack}" >> {
      s"write+read $col as ${m.typeStack}" in {
        inOut(col, a).transact(xa).attempt.unsafeRunSync must_== Right(a)
      }
      s"write+read $col as Option[${m.typeStack}] (Some)" in {
        inOutOpt[A](col, Some(a)).transact(xa).attempt.unsafeRunSync must_== Right(Some(a))
      }
      s"write+read $col as Option[${m.typeStack}] (None)" in {
        inOutOpt[A](col, None).transact(xa).attempt.unsafeRunSync must_== Right(None)
      }
    }

  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  def skip(col: String, msg: String = "not yet implemented") =
    s"Mapping for $col" >> {
      "PENDING:" in pending(msg)
    }

  testInOut[Int]("INT", 123)
  testInOut[Boolean]("BOOLEAN", true)
  testInOut[Byte]("TINYINT",  123)
  testInOut[Short]("SMALLINT", 123)
  testInOut[Long]("BIGINT", 123)
  testInOut[BigDecimal]("DECIMAL", 123.45)
  testInOut[java.sql.Time]("TIME", new java.sql.Time(3,4,5): @silent)
  testInOut[java.sql.Date]("DATE", new java.sql.Date(4,5,6): @silent)
  testInOut[java.time.LocalDate]("DATE", java.time.LocalDate.of(4,5,6))
  testInOut[java.sql.Timestamp]("TIMESTAMP", new java.sql.Timestamp(System.currentTimeMillis))
  testInOut[java.time.Instant]("TIMESTAMP", java.time.Instant.now)
  testInOut[List[Byte]]("BINARY", BigInt("DEADBEEF",16).toByteArray.toList)
  skip("OTHER")
  testInOut[String]("VARCHAR", "abc")
  testInOut[String]("CHAR(3)", "abc")
  skip("BLOB")
  skip("CLOB")
  testInOut[UUID]("UUID", UUID.randomUUID)
  testInOut[List[Int]]("ARRAY", List(1,2,3))
  testInOut[List[String]]("ARRAY", List("foo", "bar"))
  skip("GEOMETRY")

  "Mapping for Boolean" should {
    "pass query analysis for unascribed 'true'" in {
      val a = sql"select true".query[Boolean].analysis.transact(xa).unsafeRunSync
      a.alignmentErrors must_== Nil
    }
    "pass query analysis for ascribed BIT" in {
      val a = sql"select true::BIT".query[Boolean].analysis.transact(xa).unsafeRunSync
      a.alignmentErrors must_== Nil
    }
    "pass query analysis for ascribed BOOLEAN" in {
      val a = sql"select true::BIT".query[Boolean].analysis.transact(xa).unsafeRunSync
      a.alignmentErrors must_== Nil
    }
  }

  "Mapping for UUID" should {
    "pass query analysis for unascribed UUID" in {
      val a = sql"select random_uuid()".query[UUID].analysis.transact(xa).unsafeRunSync
      a.alignmentErrors must_== Nil
    }
    "pass query analysis for ascribed UUID" in {
      val a = sql"select random_uuid()::UUID".query[UUID].analysis.transact(xa).unsafeRunSync
      a.alignmentErrors must_== Nil
    }
  }
}
