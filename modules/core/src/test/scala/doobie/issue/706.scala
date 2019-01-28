// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.issue

import cats._
import cats.implicits._
import cats.effect.{ ContextShift, IO }
import doobie._, doobie.implicits._
import org.scalacheck.Prop.forAll
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import scala.concurrent.ExecutionContext
import scala.Predef._

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
object `706` extends Specification with ScalaCheck {

  implicit def contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  val xa = Transactor.fromDriverManager[IO](
    "org.h2.Driver",
    "jdbc:h2:mem:issue-706;DB_CLOSE_DELAY=-1",
    "sa", ""
  )

  val setup: ConnectionIO[Unit] =
    sql"CREATE TABLE IF NOT EXISTS test (value INTEGER)".update.run.void

  def insert[F[_]: Foldable, A: Write](as: F[A]): ConnectionIO[Int] =
    Update[A]("INSERT INTO test VALUES (?)").updateMany(as)

  "updateMany" should {

    "work correctly for valid inputs" ! forAll { (ns: List[Int]) =>
      val prog = setup *> insert(ns)
      prog.transact(xa).unsafeRunSync must_== ns.length
    }

    // TODO: add a case for invalid inputs if we can find one that doesn't cause an
    // exception to be thrown.

  }

}
