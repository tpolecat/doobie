// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.mysql.util.arbitraries

import java.time._

import doobie.util.arbitraries.GenHelpers
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

// https://dev.mysql.com/doc/refman/5.7/en/datetime.html
object TimeArbitraries {

  // max resolution is 1 microsecond
  private def micros(nanos: Long) = Math.floorDiv(nanos, 1000)

  // for Scala 2.12
  private implicit val orderingLocalDate: Ordering[LocalDate] = new Ordering[LocalDate] {
    override def compare(x: LocalDate, y: LocalDate): Int = x compareTo y
  }

  // 1000-01-01 to 9999-12-31
  implicit val arbitraryLocalDate: Arbitrary[LocalDate] = Arbitrary {
    GenHelpers.chooseT(LocalDate.of(1000, 1, 1), LocalDate.of(9999, 12, 31), LocalDate.of(1970, 1, 1))
  }

  // 00:00:00.000000 to 23:59:59.999999
  implicit val arbitraryLocalTime: Arbitrary[LocalTime] = Arbitrary {
    val min = micros(LocalTime.MIN.toNanoOfDay)
    val max = micros(LocalTime.MAX.toNanoOfDay)
    val noon = micros(LocalTime.NOON.toNanoOfDay)
    Gen.chooseNum(min, max, noon).map(micros => LocalTime.ofNanoOfDay(micros * 1000))
  }

  // '1000-01-01 00:00:00.000000' to '9999-12-31 23:59:59.999999'
  implicit val arbitraryLocalDateTime: Arbitrary[LocalDateTime] = Arbitrary {
    for {
      date <- arbitraryLocalDate.arbitrary
      time <- arbitraryLocalTime.arbitrary
    } yield LocalDateTime.of(date, time)
  }

  // '1970-01-01 00:00:01.000000' to '2038-01-19 03:14:07.999999
  implicit val arbitraryInstant: Arbitrary[Instant] = Arbitrary {
    val min = 1 * 1000000L + 0
    val max = 2147483647 * 1000000L + 999999

    Gen.chooseNum(min, max).map { micros =>
      Instant.ofEpochSecond(micros / 1000000, micros % 1000000 * 1000)
    }
  }

  implicit val arbitraryOffsetDateTime: Arbitrary[OffsetDateTime] = Arbitrary {
    for {
      instant <- arbitraryInstant.arbitrary
      offset <- Arbitrary.arbitrary[ZoneOffset]
    } yield instant.atOffset(offset)
  }
}
