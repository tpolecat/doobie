// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util.arbitraries

import org.scalacheck.{Arbitrary, Gen}

object StringArbitraries {

  implicit val arbitraryString: Arbitrary[String] = Arbitrary(Gen.alphaNumStr)

  def nLongString(n: Int): Gen[String] = Gen.listOfN(n, Gen.alphaChar).map(_.mkString(""))

}
