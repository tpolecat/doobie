// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package example

import cats.effect.{IO, IOApp, ExitCode}
import cats.syntax.all._
import doobie._
import doobie.implicits._
import fs2.Stream
import fs2.Stream.{eval, bracket}
import java.sql.{PreparedStatement, ResultSet}
import doobie.util.stream.repeatEvalChunks

/** From a user question on Gitter, how can we have an equivalent to `Stream[A]` that constructs a stream of untyped
  * maps.
  */
object GenericStream extends IOApp {

  type Row = Map[String, Any]

  // This escapes to raw JDBC for efficiency.
  @SuppressWarnings(Array(
    "org.wartremover.warts.Var",
    "org.wartremover.warts.While",
    "org.wartremover.warts.NonUnitStatements"
  ))
  def getNextChunkGeneric(chunkSize: Int): ResultSetIO[Seq[Row]] =
    FRS.raw { rs =>
      val md = rs.getMetaData
      val ks = (1 to md.getColumnCount).map(md.getColumnLabel).toList
      var n = chunkSize
      val b = Vector.newBuilder[Row]
      while (n > 0 && rs.next) {
        val mb = Map.newBuilder[String, Any]
        ks.foreach(k => mb += (k -> rs.getObject(k)))
        b += mb.result()
        n -= 1
      }
      b.result()
    }

  def liftProcessGeneric(
      chunkSize: Int,
      create: ConnectionIO[PreparedStatement],
      prep: PreparedStatementIO[Unit],
      exec: PreparedStatementIO[ResultSet]
  ): Stream[ConnectionIO, Row] = {

    def prepared(ps: PreparedStatement): Stream[ConnectionIO, PreparedStatement] =
      eval[ConnectionIO, PreparedStatement] {
        val fs = FPS.setFetchSize(chunkSize)
        FC.embed(ps, fs *> prep).map(_ => ps)
      }

    def unrolled(rs: ResultSet): Stream[ConnectionIO, Row] =
      repeatEvalChunks(FC.embed(rs, getNextChunkGeneric(chunkSize)))

    val preparedStatement: Stream[ConnectionIO, PreparedStatement] =
      bracket(create)(FC.embed(_, FPS.close)).flatMap(prepared)

    def results(ps: PreparedStatement): Stream[ConnectionIO, Row] =
      bracket(FC.embed(ps, exec))(FC.embed(_, FRS.close)).flatMap(unrolled)

    preparedStatement.flatMap(results)

  }

  def processGeneric(sql: String, prep: PreparedStatementIO[Unit], chunkSize: Int): Stream[ConnectionIO, Row] =
    liftProcessGeneric(chunkSize, FC.prepareStatement(sql), prep, FPS.executeQuery)

  val xa = Transactor.fromDriverManager[IO](
    driver = "org.postgresql.Driver",
    url = "jdbc:postgresql:world",
    user = "postgres",
    password = "password",
    logHandler = None
  )

  def run(args: List[String]): IO[ExitCode] =
    args match {
      case sql :: Nil => processGeneric(sql, ().pure[PreparedStatementIO], 100).transact(xa).evalMap(m =>
          IO(Console.println(m))).compile.drain.as(ExitCode.Success)
      case _ => IO(Console.println("expected on arg, a query")).as(ExitCode.Error)
    }

  // > runMain example.GenericStream "select * from city limit 10"
  // Map(name -> Kabul, population -> 1780000, id -> 1, district -> Kabol, countrycode -> AFG)
  // Map(name -> Qandahar, population -> 237500, id -> 2, district -> Qandahar, countrycode -> AFG)
  // Map(name -> Herat, population -> 186800, id -> 3, district -> Herat, countrycode -> AFG)
  // Map(name -> Mazar-e-Sharif, population -> 127800, id -> 4, district -> Balkh, countrycode -> AFG)
  // Map(name -> Amsterdam, population -> 731200, id -> 5, district -> Noord-Holland, countrycode -> NLD)
  // Map(name -> Rotterdam, population -> 593321, id -> 6, district -> Zuid-Holland, countrycode -> NLD)
  // Map(name -> Haag, population -> 440900, id -> 7, district -> Zuid-Holland, countrycode -> NLD)
  // Map(name -> Utrecht, population -> 234323, id -> 8, district -> Utrecht, countrycode -> NLD)
  // Map(name -> Eindhoven, population -> 201843, id -> 9, district -> Noord-Brabant, countrycode -> NLD)
  // Map(name -> Tilburg, population -> 193238, id -> 10, district -> Noord-Brabant, countrycode -> NLD)

}
