// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package example

import cats.Show
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import doobie.{ConnectionIO, Query, Query0, Transactor, Update}
import doobie.implicits.*
import doobie.util.{Read, Write}
import fs2.Stream
import shapeless.*
import shapeless.ops.record.*
import shapeless.ops.hlist.*

/** A super-simple ORM for super-simple data types. We assume auto-generated keys, represented externally, and columns
  * map 1:1 with fields and have the same names.
  */
object Orm extends IOApp {

  // to silence unused warnings
  def void[A](a: A): Unit = (a, ())._2

  trait Dao[A] {
    type Key
    def insert(a: A): ConnectionIO[Key]
    def find(k: Key): ConnectionIO[Option[A]]
    def findAll: Stream[ConnectionIO, A]
    def update(k: Key, a: A): ConnectionIO[Int]
    def delete(k: Key): ConnectionIO[Int]
  }
  object Dao {

    type Aux[A, K] = Dao[A] { type Key = K }

    def apply[A](implicit ev: Dao[A]): Aux[A, ev.Key] = ev

    object derive {
      def apply[A, K] = new Partial[A, K]
      class Partial[A, K] {
        def apply[R <: HList, S <: HList](table: String, keyCol: String)(
            implicit
            ev: LabelledGeneric.Aux[A, R],
            ra: Read[A],
            wa: Write[A],
            ks: Keys.Aux[R, S],
            tl: ToList[S, Symbol],
            rk: Read[K],
            wk: Write[K]
        ): Aux[A, K] =
          new Dao[A] {
            void(ev)

            type Key = K
            val cols = ks.apply().toList.map(_.name)

            def insert(a: A): ConnectionIO[Key] =
              Update[A](s"""
                INSERT INTO $table (${cols.mkString(", ")})
                VALUES (${cols.as("?").mkString(", ")})
              """).withUniqueGeneratedKeys[Key](keyCol)(a)

            def find(key: Key): ConnectionIO[Option[A]] =
              Query[Key, A](s"""
                SELECT ${cols.mkString(", ")}
                FROM $table
                WHERE $keyCol = ?
              """).option(key)

            def findAll: Stream[ConnectionIO, A] =
              Query0[A](s"""
                SELECT ${cols.mkString(", ")}
                FROM $table
              """).stream

            def update(k: Key, a: A): ConnectionIO[Int] =
              Update[(A, Key)](s"""
                UPDATE $table
                SET ${cols.map(_ + " = ?").mkString(", ")}
                WHERE $keyCol = ?
              """).run((a, k))

            def delete(k: Key): ConnectionIO[Int] = {
              Update[Key](s"""
                DELETE FROM $table
                WHERE $keyCol = ?
              """).run(k)
            }

          }
      }
    }

  }

  // a little usage example

  final case class Neighbor(name: String, age: Int)
  object Neighbor {
    implicit val dao: Dao.Aux[Neighbor, Int] =
      Dao.derive[Neighbor, Int]("neighbor", "id")

    implicit val show: Show[Neighbor] = Show.fromToString
  }

  val ddl: ConnectionIO[Unit] =
    sql"""
      CREATE TABLE IF NOT EXISTS neighbor (
        id   SERIAL,
        name VARCHAR NOT NULL,
        age  INT     NOT NULL
      )
    """.update.run.void

  val prog: ConnectionIO[String] = {
    val dn = Dao[Neighbor]
    import dn._
    for {
      _ <- ddl
      ka <- insert(Neighbor("Alice", 42))
      kb <- insert(Neighbor("Bob", 42))
      oa <- find(ka)
      _ <- delete(kb)
    } yield show"Did some stuff. Keys were $ka and $kb. Selected value was ${oa.fold("<nothing>")(_.show)}."
  }

  val xa = Transactor.fromDriverManager[IO](
    driver = "org.postgresql.Driver",
    url = "jdbc:postgresql:world",
    user = "postgres",
    password = "password",
    logHandler = None
  )

  def run(args: List[String]): IO[ExitCode] =
    for {
      a <- prog.transact(xa)
      _ <- IO(println(a))
    } yield ExitCode.Success

}
