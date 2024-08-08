// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.effect.IO
import doobie.Transactor
import doobie.*
import doobie.implicits.*
import cats.syntax.all.*

import scala.concurrent.duration.DurationInt

class QueryCancellationSuite extends munit.FunSuite {
  import cats.effect.unsafe.implicits.global

  val xa = Transactor.fromDriverManager[IO](
    driver = "org.h2.Driver",
    url = "jdbc:h2:mem:queryspec;DB_CLOSE_DELAY=-1",
    user = "sa",
    password = "",
    logHandler = None
  )

  test("Query cancel") {
    val scenario = WeakAsync.liftIO[ConnectionIO].use { elevator =>
      for {
        _ <- sql"CREATE TABLE IF NOT EXISTS example_table ( id INT)".update.run.transact(xa)
        _ <- sql"TRUNCATE TABLE example_table".update.run.transact(xa)
        _ <- sql"INSERT INTO example_table (id) VALUES (1)".update.run.transact(xa)
        _ <- {
          sql"select * from example_table for update".query[Int].unique >> elevator.liftIO(IO.never)
        }.transact(xa).start

        insertWithLockFiber <- {
          for {
            _ <- IO.sleep(100.milli)
            insertFiber <- sql"UPDATE example_table SET id = 2".update.run.transact(xa).start
            _ <- IO.sleep(100.milli)
            _ <- insertFiber.cancel
          } yield ()
        }.start

        _ <- IO.race(insertWithLockFiber.join, IO.sleep(5.seconds) >> IO(fail("Cancellation is blocked")))
        result <- sql"SELECT * FROM example_table".query[Int].to[List].transact(xa)
      } yield assertEquals(result, List(1))
    }
    scenario.unsafeRunSync()
  }
}
