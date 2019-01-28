// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.effect.{ Async, ContextShift, IO }
import cats.implicits._
import doobie._, doobie.implicits._
import org.specs2.mutable.Specification
import scala.concurrent.ExecutionContext

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.Var"))
object strategyspec extends Specification {

  implicit def contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  val baseXa = Transactor.fromDriverManager[IO](
    "org.h2.Driver",
    "jdbc:h2:mem:queryspec;DB_CLOSE_DELAY=-1",
    "sa", ""
  )

  // an instrumented interpreter
  class Interp extends KleisliInterpreter[IO] {

    val asyncM = Async[IO]
    val blockingContext = ExecutionContext.global
    val contextShiftM = contextShift

    object Connection {
      var autoCommit: Option[Boolean] = None
      var close:      Option[Unit]    = None
      var commit:     Option[Unit]    = None
      var rollback:   Option[Unit]    = None
    }

    object PreparedStatement {
      var close: Option[Unit] = None
    }

    object ResultSet {
      var close: Option[Unit] = None
    }

    override lazy val ConnectionInterpreter = new ConnectionInterpreter {
      override val close = delay(() => Connection.close = Some(())) *> super.close
      override val rollback = delay(() => Connection.rollback = Some(())) *> super.rollback
      override val commit = delay(() => Connection.commit = Some(())) *> super.commit
      override def setAutoCommit(b: Boolean) = delay(() => Connection.autoCommit = Option(b)) *> super.setAutoCommit(b)
    }

    override lazy val PreparedStatementInterpreter = new PreparedStatementInterpreter {
      override val close = delay(() => PreparedStatement.close = Some(())) *> super.close
    }

    override lazy val ResultSetInterpreter = new ResultSetInterpreter {
      override val close = delay(() => ResultSet.close = Some(())) *> super.close
    }

  }

  def xa(i: KleisliInterpreter[IO]) =
    Transactor.interpreter.set(baseXa, i.ConnectionInterpreter)

  "Connection configuration and safety" >> {

    "Connection.autoCommit should be set to false" in {
      val i = new Interp
      sql"select 1".query[Int].unique.transact(xa(i)).unsafeRunSync
      i.Connection.autoCommit must_== Some(false)
    }

    "Connection.commit should be called on success" in {
      val i = new Interp
      sql"select 1".query[Int].unique.transact(xa(i)).unsafeRunSync
      i.Connection.commit must_== Some(())
    }

    "Connection.commit should NOT be called on failure" in {
      val i = new Interp
      sql"abc".query[Int].unique.transact(xa(i)).attempt.unsafeRunSync.toOption must_== None
      i.Connection.commit must_== None
    }

    "Connection.rollback should NOT be called on success" in {
      val i = new Interp
      sql"select 1".query[Int].unique.transact(xa(i)).unsafeRunSync
      i.Connection.rollback must_== None
    }

    "Connection.rollback should be called on failure" in {
      val i = new Interp
      sql"abc".query[Int].unique.transact(xa(i)).attempt.unsafeRunSync.toOption must_== None
      i.Connection.rollback must_== Some(())
    }

    "Connection.close should be called on success" in {
      val i = new Interp
      sql"select 1".query[Int].unique.transact(xa(i)).unsafeRunSync
      i.Connection.close must_== Some(())
    }

    "Connection.close should be called on failure" in {
      val i = new Interp
      sql"abc".query[Int].unique.transact(xa(i)).attempt.unsafeRunSync.toOption must_== None
      i.Connection.close must_== Some(())
    }

  }

  "Connection configuration and safety (streaming)" >> {

    "Connection.autoCommit should be set to false" in {
      val i = new Interp
      sql"select 1".query[Int].stream.compile.toList.transact(xa(i)).unsafeRunSync
      i.Connection.autoCommit must_== Some(false)
    }

    "Connection.commit should be called on success" in {
      val i = new Interp
      sql"select 1".query[Int].stream.compile.toList.transact(xa(i)).unsafeRunSync
      i.Connection.commit must_== Some(())
    }

    "Connection.commit should NOT be called on failure" in {
      val i = new Interp
      sql"abc".query[Int].stream.compile.toList.transact(xa(i)).attempt.unsafeRunSync.toOption must_== None
      i.Connection.commit must_== None
    }

    "Connection.rollback should NOT be called on success" in {
      val i = new Interp
      sql"select 1".query[Int].stream.compile.toList.transact(xa(i)).unsafeRunSync
      i.Connection.rollback must_== None
    }

    "Connection.rollback should be called on failure" in {
      val i = new Interp
      sql"abc".query[Int].stream.compile.toList.transact(xa(i)).attempt.unsafeRunSync.toOption must_== None
      i.Connection.rollback must_== Some(())
    }

    "Connection.close should be called on success" in {
      val i = new Interp
      sql"select 1".query[Int].stream.compile.toList.transact(xa(i)).unsafeRunSync
      i.Connection.close must_== Some(())
    }

    "Connection.close should be called on failure" in {
      val i = new Interp
      sql"abc".query[Int].stream.compile.toList.transact(xa(i)).attempt.unsafeRunSync.toOption must_== None
      i.Connection.close must_== Some(())
    }

  }

  "PreparedStatement safety" >> {

    "PreparedStatement.close should be called on success" in {
      val i = new Interp
      sql"select 1".query[Int].unique.transact(xa(i)).unsafeRunSync
      i.PreparedStatement.close must_== Some(())
    }

    "PreparedStatement.close should be called on failure" in {
      val i = new Interp
      sql"select 'x'".query[Int].unique.transact(xa(i)).attempt.unsafeRunSync.toOption must_== None
      i.PreparedStatement.close must_== Some(())
    }

  }

  "PreparedStatement safety (streaming)" >> {

    "PreparedStatement.close should be called on success" in {
      val i = new Interp
      sql"select 1".query[Int].stream.compile.toList.transact(xa(i)).unsafeRunSync
      i.PreparedStatement.close must_== Some(())
    }

    "PreparedStatement.close should be called on failure" in {
      val i = new Interp
      sql"select 'x'".query[Int].stream.compile.toList.transact(xa(i)).attempt.unsafeRunSync.toOption must_== None
      i.PreparedStatement.close must_== Some(())
    }

  }

  "ResultSet safety" >> {

    "ResultSet.close should be called on success" in {
      val i = new Interp
      sql"select 1".query[Int].unique.transact(xa(i)).unsafeRunSync
      i.ResultSet.close must_== Some(())
    }

    "ResultSet.close should be called on failure" in {
      val i = new Interp
      sql"select 'x'".query[Int].unique.transact(xa(i)).attempt.unsafeRunSync.toOption must_== None
      i.ResultSet.close must_== Some(())
    }

  }

  "ResultSet safety (streaming)" >> {

    "ResultSet.close should be called on success" in {
      val i = new Interp
      sql"select 1".query[Int].stream.compile.toList.transact(xa(i)).unsafeRunSync
      i.ResultSet.close must_== Some(())
    }

    "ResultSet.close should be called on failure" in {
      val i = new Interp
      sql"select 'x'".query[Int].stream.compile.toList.transact(xa(i)).attempt.unsafeRunSync.toOption must_== None
      i.ResultSet.close must_== Some(())
    }

  }

}
