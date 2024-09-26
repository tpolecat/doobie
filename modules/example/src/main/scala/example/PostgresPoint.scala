// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package example

import cats.effect.{IO, IOApp}
import doobie.{Meta, Transactor}
import doobie.util.Write
import doobie.implicits.*
import doobie.postgres.implicits.*
import org.postgresql.geometric.PGpoint

object PostgresPoint extends IOApp.Simple {

  val xa = Transactor.fromDriverManager[IO](
    driver = "org.postgresql.Driver",
    url = "jdbc:postgresql:world",
    user = "postgres",
    password = "password",
    logHandler = None
  )

  // A custom Point type with a Meta instance xmapped from the PostgreSQL native type (which
  // would be weird to use directly in a data model). Note that the presence of this `Meta`
  // instance precludes mapping `Point` to two columns. If you want two mappings you need two types.
  final case class Point(x: Double, y: Double)
  object Point {
    implicit val PointType: Meta[Point] =
      Meta[PGpoint].timap(p => new Point(p.x, p.y))(p => new PGpoint(p.x, p.y))
  }

  val q = sql"select '(1, 2)'::point".query[Point]

  def run: IO[Unit] =
    for {
      a <- q.to[List].transact(xa)

      // Point is now a perfectly cromulent input/output type
      _ <- IO(println(a)) // List(Point(1.0,2.0))

      // Just to be clear; the Write instance has width 1, not 2
      _ <- IO(println(Write[Point].length)) // 1

    } yield ()

}
