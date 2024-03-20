// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util.arbitraries

import java.sql.{Time, Timestamp, Date}
import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Arbitrary.arbitrary

import scala.annotation.nowarn

@nowarn("msg=.*deprecated.*")
object SQLArbitraries {

  implicit val arbitraryTime: Arbitrary[Time] = Arbitrary {
    for {
      h <- Gen.chooseNum(0, 23)
      m <- Gen.chooseNum(0, 59)
      s <- Gen.chooseNum(0, 59)
    } yield new Time(h, m, s)
  }

  implicit val arbitraryDate: Arbitrary[Date] = Arbitrary {
    for {
      y <- Gen.chooseNum(0, 8099)
      m <- Gen.chooseNum(0, 11)
      d <- Gen.chooseNum(1, 31)
    } yield new Date(y, m, d)
  }

  implicit val arbitraryTimestamp: Arbitrary[Timestamp] = Arbitrary {
    for {
      d <- arbitrary[Date]
      t <- arbitrary[Time]
      n <- Gen.chooseNum(0, 999999999)
    } yield new Timestamp(d.getYear, d.getMonth, d.getDate, t.getHours, t.getMinutes, t.getSeconds, n)
  }

}
