// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import cats.effect.{ ContextShift, IO, Sync }
import doobie._, doobie.implicits._
import java.io.ByteArrayOutputStream
import org.specs2.mutable.Specification
import scala.concurrent.ExecutionContext

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
object pgcopyspec extends Specification {

  implicit def contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  val xa = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:world",
    "postgres", ""
  )

  "copy out" should {

    "read csv in utf-8 and match expectations" in  {

      val query = """
        copy (select code, name, population
              from country
              where name like 'U%'
              order by code)
        to stdout (encoding 'utf-8', format csv)"""

      val fixture = """
        |ARE,United Arab Emirates,2441000
        |GBR,United Kingdom,59623400
        |UGA,Uganda,21778000
        |UKR,Ukraine,50456000
        |UMI,United States Minor Outlying Islands,0
        |URY,Uruguay,3337000
        |USA,United States,278357000
        |UZB,Uzbekistan,24318000
        |
      """.trim.stripMargin

      val prog: ConnectionIO[String] =
        for {
          out <- Sync[ConnectionIO].delay(new ByteArrayOutputStream)
          _   <- PHC.pgGetCopyAPI(PFCM.copyOut(query, out))
        } yield new String(out.toByteArray, "UTF-8")

      prog.transact(xa).unsafeRunSync must_== fixture

    }

  }

}
