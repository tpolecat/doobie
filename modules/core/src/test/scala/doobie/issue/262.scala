// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.issue

import cats.effect.{ Async, Blocker, ContextShift, IO }
import doobie._, doobie.implicits._
import org.specs2.mutable.Specification
import scala.concurrent.ExecutionContext
import Predef._
import io.chrisdavenport.log4cats.MessageLogger
import doobie.util.DefaultMessageLogger


class `262` extends Specification {

  implicit def contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  // an interpreter that returns null when we ask for statement metadata
  object Interp extends KleisliInterpreter[IO] {
    val asyncM = Async[IO]
    val blocker = Blocker.liftExecutionContext(ExecutionContext.global)
    val contextShiftM = contextShift
    val loggerM: MessageLogger[IO] = DefaultMessageLogger[IO]

    val M = implicitly[Async[IO]]

    override lazy val PreparedStatementInterpreter =
      new PreparedStatementInterpreter {
        override def getMetaData = primitive(_ => null, "getMetaData")
      }

  }

  val baseXa = Transactor.fromDriverManager[IO](
    "org.h2.Driver",
    "jdbc:h2:mem:queryspec;DB_CLOSE_DELAY=-1",
    "sa", ""
  )

  // A transactor that uses our interpreter above
  val xa: Transactor[IO] =
    Transactor.interpret.set(baseXa, Interp.ConnectionInterpreter)

  "getColumnJdbcMeta" should {
    "handle null metadata" in {
      val prog = HC.prepareStatement("select 1")(HPS.getColumnJdbcMeta)
      prog.transact(xa).unsafeRunSync() must_== Nil
    }
  }

}
