// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie
package util

import com.github.ghik.silencer.silent
import shapeless._, shapeless.record._
import org.specs2.mutable.Specification

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
object readspec extends Specification {

  final case class Woozle(a: (String, Int), b: Int :: String :: HNil, c: Boolean)

  final case class LenStr1(n: Int, s: String)

  final case class LenStr2(n: Int, s: String)
  object LenStr2 {
    implicit val LenStrMeta: Meta[LenStr2] =
      Meta[String].timap(s => LenStr2(s.length, s))(_.s)
  }

  "Read" should {

    "exist for some fancy types" in {
      Read[Int]
      Read[(Int, Int)]
      Read[(Int, Int, String)]
      Read[(Int, (Int, String))]
      Read[Woozle]

      // https://github.com/tpolecat/doobie/pull/126 was reverted because these
      // derivations were failing with SOE
      Read[(Woozle, String)]
      Read[(Int, Woozle :: Woozle :: String :: HNil)]

      true
    }

    "exist for Unit" in {
      Read[Unit]
      Read[(Int, Unit)].length must_== 1
    }

    "exist for shapeless record types" in {

      @silent // scalac doesn't know this type is used
      type DL = (Double, Long)
      type A  = Record.`'foo -> Int, 'bar -> String, 'baz -> DL, 'quz -> Woozle`.T

      Read[A]
      Read[(A, A)]

      true
    }

    "exist for option of some fancy types" in {
      Read[Option[Int]]
      Read[Option[(Int, Int)]]
      Read[Option[(Int, Int, String)]]
      Read[Option[(Int, (Int, String))]]
      Read[Option[(Int, Option[(Int, String)])]]
      Read[Option[Woozle]]
      Read[Option[(Woozle, String)]]
      Read[Option[(Int, Woozle :: Woozle :: String :: HNil)]]
      true
    }

    "exist for option of Unit" in {
      Read[Option[Unit]]
      Read[Option[(Int, Unit)]].length must_== 1
    }

    // This doesn't work because of the issue with tagged element as HList members.
    // "exist for option of shapeless record types" in {
    //
    //   type DL = (Double, Long)
    //   type A  = Record.`'foo -> Int, 'bar -> String, 'baz -> DL, 'quz -> Woozle`.T
    //
    //   Read[Option[A]]
    //   Read[Option[(A, A)]]
    //   Read[Option[(A, Option[A])]]
    //
    //   true
    // }

    "select multi-column instance by default" in {
      Read[LenStr1].length must_== 2
    }

    "select 1-column instance when available" in {
      Read[LenStr2].length must_== 1
    }

    // "work for products of ludicrous size (128)" in {
    //   Read[
    //     Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    //     Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    //     Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    //     Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    //     Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    //     Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    //     Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    //     Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    //     Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    //     Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    //     Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    //     Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    //     Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    //     Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    //     Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int ::
    //     Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: HNil]
    //   true
    // }

    // "work for specific example from #217" in {
    //   trait LocalDateTime
    //   implicit val MetaLocalDateTime: Get[LocalDateTime] = null
    //   Read[
    //     Option[String] :: Option[String] :: Option[String] :: Option[LocalDateTime] ::
    //     Option[String] :: Option[String] :: Option[String] :: Option[String] ::
    //     Option[String] :: Option[String] :: Option[String] :: Option[String] ::
    //     Option[String] :: Option[String] :: Option[String] :: Option[String] ::
    //     Option[String] :: Option[String] :: Option[String] :: Option[String] ::
    //     Option[String] :: Option[String] :: Option[LocalDateTime] :: Option[LocalDateTime] ::
    //     Option[LocalDateTime] :: Option[LocalDateTime] :: Option[String] :: Option[String] ::
    //     Option[String] :: Option[String] :: Option[String] :: Option[String] ::
    //     Option[String] :: Option[String] :: Option[String] :: Option[String] ::
    //     Option[String] :: Option[String] :: Option[String] :: Option[String] ::
    //     Option[String] :: Option[String] :: Option[String] :: Option[String] ::
    //     Option[String] :: Option[String] :: Option[String] :: Option[BigDecimal] ::
    //     Option[String] :: Option[BigDecimal] :: Option[BigDecimal] ::
    //     Option[String] :: Option[String] :: Option[String] :: Option[String] ::
    //     Option[String] :: Option[String] :: Option[String] :: Option[String] ::
    //     Option[String] :: Option[String] :: Option[BigDecimal] :: Option[BigDecimal] ::
    //     Option[String] :: Option[String] :: Option[String] :: Option[String] ::
    //     Option[String] :: Option[String] :: Option[String] :: Option[String] ::
    //     Option[String] :: Option[String] :: Option[BigDecimal] :: Option[BigDecimal] ::
    //     Option[String] :: HNil]
    //   true
    // }

  }

}
