// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres.circe

import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all._
import io.circe._
import io.circe.jawn._
import io.circe.syntax._
import doobie.util._
import org.postgresql.util.PGobject

object Instances {

  private implicit val showPGobject: Show[PGobject] = Show.show(_.getValue.take(250))

  trait JsonbInstances {
    implicit val jsonbPut: Put[Json] =
      Put.Advanced.other[PGobject](
        NonEmptyList.of("jsonb")
      ).tcontramap { a =>
        val o = new PGobject
        o.setType("jsonb")
        o.setValue(a.noSpaces)
        o
      }

    implicit val jsonbGet: Get[Json] =
      Get.Advanced.other[PGobject](
        NonEmptyList.of("jsonb")
      ).temap(a =>
        parse(a.getValue).leftMap(_.show))

    def pgEncoderPutT[A: Encoder]: Put[A] =
      Put[Json].tcontramap(_.asJson)

    def pgEncoderPut[A: Encoder]: Put[A] =
      Put[Json].contramap(_.asJson)

    def pgDecoderGetT[A: Decoder]: Get[A] =
      Get[Json].temap(json => json.as[A].leftMap(_.show))

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def pgDecoderGet[A: Decoder]: Get[A] =
      Get[Json].map(json => json.as[A].fold(throw _, identity))
  }

  trait JsonInstances {
    implicit val jsonPut: Put[Json] =
      Put.Advanced.other[PGobject](
        NonEmptyList.of("json")
      ).tcontramap { a =>
        val o = new PGobject
        o.setType("json")
        o.setValue(a.noSpaces)
        o
      }

    implicit val jsonGet: Get[Json] =
      Get.Advanced.other[PGobject](
        NonEmptyList.of("json")
      ).temap(a =>
        parse(a.getValue).leftMap(_.show))

    def pgEncoderPutT[A: Encoder]: Put[A] =
      Put[Json].tcontramap(_.asJson)

    def pgEncoderPut[A: Encoder]: Put[A] =
      Put[Json].contramap(_.asJson)

    def pgDecoderGetT[A: Decoder]: Get[A] =
      Get[Json].temap(json => json.as[A].leftMap(_.show))

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def pgDecoderGet[A: Decoder]: Get[A] =
      Get[Json].map(json => json.as[A].fold(throw _, identity))

  }

}
