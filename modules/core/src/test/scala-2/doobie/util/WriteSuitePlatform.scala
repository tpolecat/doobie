// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import shapeless._
import shapeless.record._

import scala.annotation.nowarn

trait WriteSuitePlatform { self: munit.FunSuite =>
  import doobie.generic.auto._

  test("Write should exist for shapeless record types") {
    type DL = (Double, Long)
    type A = Record.`'foo -> Int, 'bar -> String, 'baz -> DL, 'quz -> Woozle`.T
    Write[A]
    Write[(A, A)]
  }: @nowarn("msg=.*DL is never used.*")

  case class Woozle(a: (String, Int), b: Int :: String :: HNil, c: Boolean)

  test("Write should exist for some fancy types") {
    Write[Woozle]
    Write[(Woozle, String)]
    Write[(Int, Woozle :: Woozle :: String :: HNil)]
  }

  test("Write should exist for option of some fancy types") {
    Write[Option[Woozle]]
    Write[Option[(Woozle, String)]]
    Write[Option[(Int, Woozle :: Woozle :: String :: HNil)]]
  }

}
