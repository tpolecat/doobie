// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.mysql.util.arbitraries

import java.sql.Date
import java.sql.Time
import java.sql.Timestamp

import org.scalacheck.Arbitrary

object SQLArbitraries {

  implicit val arbitraryTime: Arbitrary[Time] = Arbitrary {
    TimeArbitraries.arbitraryLocalTime.arbitrary.map(Time.valueOf(_))
  }

  implicit val arbitraryDate: Arbitrary[Date] = Arbitrary {
    TimeArbitraries.arbitraryLocalDate.arbitrary.map(Date.valueOf(_))
  }

  implicit val arbitraryTimestamp: Arbitrary[Timestamp] = Arbitrary {
    TimeArbitraries.arbitraryLocalDateTime.arbitrary.map(Timestamp.valueOf(_))
  }

}
