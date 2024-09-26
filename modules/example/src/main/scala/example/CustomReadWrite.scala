// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package example

import doobie.*
import doobie.implicits.*
import doobie.util.{Read, Write}

import java.sql.Date

object CustomReadWrite {

  final case class PosixTime(time: Long)

  // Create our base Meta by invariant mapping an existing one.
  implicit val LongPosixTimeScalaType: Meta[PosixTime] =
    Meta[Date].timap(d => PosixTime(d.getTime))(t => new Date(t.time))

  // What we just defined
  val m = Meta[PosixTime]

  // Free derived read/writes containing atomic types
  val c1 = Read[(PosixTime, Int, String)]
  val c2 = Write[(Option[PosixTime], Int, String)]

  // You can now use PosixTime as a column or parameter type (both demonstrated here)
  def query(lpt: PosixTime): Query0[(String, PosixTime)] =
    sql"SELECT NAME, DATE FROM FOO WHERE DATE > $lpt".query[(String, PosixTime)]

}
