// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util.arbitraries

import scala.math.Ordering.Implicits._

import org.scalacheck.Gen

object GenHelpers {

  /** [[Gen.chooseNum]] but allows `T` to have an `Ordering` instead of `Numeric`.
    */
  def chooseT[T](minT: T, maxT: T, specials: T*)(implicit ord: Ordering[T], c: Gen.Choose[T]): Gen[T] = {
    val basics = List(minT, maxT)
    val basicsAndSpecials = for {
      t <- specials ++ basics if t >= minT && t <= maxT
    } yield (1, Gen.const(t))
    val other = (basicsAndSpecials.length, c.choose(minT, maxT))
    val allGens = basicsAndSpecials :+ other
    Gen.frequency(allGens: _*)
  }

}
