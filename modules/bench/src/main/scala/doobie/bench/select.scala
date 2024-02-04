// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.bench

import cats.effect.IO
import doobie._, doobie.implicits._
import java.sql.DriverManager
import org.openjdk.jmh.annotations._

object shared {

  @State(Scope.Benchmark)
  val xa = Transactor.fromDriverManager[IO](
    driver = "org.postgresql.Driver",
    url = "jdbc:postgresql:world",
    user = "postgres",
    password = "password",
    logHandler = None)
}

class bench {
  import shared._
  import cats.effect.unsafe.implicits.global

  // Baseline hand-written JDBC code
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.While"))
  def jdbcBench(n: Int): Int = {
    Class.forName("org.postgresql.Driver")
    val co = DriverManager.getConnection("jdbc:postgresql:world", "postgres", "password")
    try {
      co.setAutoCommit(false)
      val ps = co.prepareStatement("select a.name, b.name, co.name from country a, country b, country co limit ?")
      try {
        ps.setInt(1, n)
        val rs = ps.executeQuery
        try {
          val accum = List.newBuilder[(String, String, String)]
          while (rs.next) {
            val a = rs.getString(1); rs.wasNull
            val b = rs.getString(2); rs.wasNull
            val c = rs.getString(3); rs.wasNull
            accum += ((a, b, c))
          }
          accum.result().length
        } finally rs.close
      } finally ps.close
    } finally {
      co.commit()
      co.close()
    }
  }

  // Reading via .stream, which adds a fair amount of overhead
  def doobieBenchP(n: Int): Int =
    sql"select a.name, b.name, c.name from country a, country b, country c limit $n"
      .query[(String, String, String)]
      .stream
      .compile.toList
      .transact(xa)
      .map(_.length)
      .unsafeRunSync()

  // Reading via .list, which uses a lower-level collector
  def doobieBench(n: Int): Int =
    sql"select a.name, b.name, c.name from country a, country b, country c limit $n"
      .query[(String, String, String)]
      .to[List]
      .transact(xa)
      .map(_.length)
      .unsafeRunSync()

  // Reading via .vector, which uses a lower-level collector
  def doobieBenchV(n: Int): Int =
    sql"select a.name, b.name, c.name from country a, country b, country c limit $n"
      .query[(String, String, String)]
      .to[Vector]
      .transact(xa)
      .map(_.length)
      .unsafeRunSync()

  @Benchmark
  @OperationsPerInvocation(1000)
  def list_accum_1000_jdbc: Int = jdbcBench(1000)

  @Benchmark
  @OperationsPerInvocation(1000)
  def list_accum_1000: Int = doobieBench(1000)

  @Benchmark
  @OperationsPerInvocation(1000)
  def vector_accum_1000: Int = doobieBenchV(1000)

  @Benchmark
  @OperationsPerInvocation(1000)
  def stream_accum_1000: Int = doobieBenchP(1000)

}
