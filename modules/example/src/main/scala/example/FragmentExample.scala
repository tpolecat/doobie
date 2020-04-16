// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package example

import cats.effect.{ IO, IOApp, ExitCode }
import cats.implicits._
import doobie._, doobie.implicits._

object FragmentExample extends IOApp {

  // Import some convenience constructors.
  import Fragments.{ in, whereAndOpt }

  // Country Info
  final case class Info(name: String, code: String, population: Int)

  // Construct a Query0 with some optional filter conditions and a configurable LIMIT.
  def select(name: Option[String], pop: Option[Int], codes: List[String], limit: Long) = {

    // Three Option[Fragment] filter conditions.
    val f1 = name.map(s => fr"name LIKE $s")
    val f2 = pop.map(n => fr"population > $n")
    val f3 = codes.toNel.map(cs => in(fr"code", cs))

    // Our final query
    val q: Fragment[Any] =
      fr"SELECT name, code, population FROM country" ++
      whereAndOpt(f1, f2, f3)                        ++
      fr"LIMIT $limit"

    // Construct a Query0
    q.query[Info]

  }

  // Our world database
  val xa = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:world",
    "postgres", ""
  )

  // Some quick examples.
  val prog = List(
    select(None, None, Nil, 10),
    select(Some("U%"), None, Nil, 10),
    select(None, Some(100000000), Nil, 10),
    select(Some("U%"), None, List("USA", "GBR", "FRA"), 10),
    select(Some("U%"), Some(100000000), List("USA", "GBR", "FRA"), 10)
  ).traverse { q =>
    val y = xa.yolo
    import y._
    q.check *> q.quick
  }

  def run(args: List[String]): IO[ExitCode] =
    prog.as(ExitCode.Success)

}
