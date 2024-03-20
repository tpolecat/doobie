// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.h2.circe

import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all._
import doobie.enumerated.JdbcType
import io.circe._
import io.circe.jawn._
import io.circe.syntax._
import doobie.util._

import java.nio.charset.StandardCharsets.UTF_8

object Instances {

  private implicit val byteArrayShow: Show[Array[Byte]] = Show.show(new String(_, UTF_8))

  trait JsonInstances {
    implicit val jsonPut: Put[Json] =
      Put.Advanced.one[Array[Byte]](
        JdbcType.VarChar,
        NonEmptyList.of("JSON"),
        (ps, n, a) => ps.setObject(n, a),
        (rs, n, a) => rs.updateObject(n, a)
      )
        .tcontramap { a =>
          a.noSpaces.getBytes(UTF_8)
        }

    implicit val jsonGet: Get[Json] =
      Get.Advanced.other[Array[Byte]](
        NonEmptyList.of("JSON")
      ).temap(a =>
        parse(a.show).leftMap(_.show))

    def h2EncoderPutT[A: Encoder]: Put[A] =
      Put[Json].tcontramap(_.asJson)

    def h2EncoderPut[A: Encoder]: Put[A] =
      Put[Json].contramap(_.asJson)

    def h2DecoderGetT[A: Decoder]: Get[A] =
      Get[Json].temap(json => json.as[A].leftMap(_.show))

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def h2DecoderGet[A: Decoder]: Get[A] =
      Get[Json].map(json => json.as[A].fold(throw _, identity))
  }

}
