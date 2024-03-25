// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import doobie.Meta
import doobie.postgres.types.Range
import org.postgresql.util.PGobject

import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.ChronoField
import java.time.{LocalDate, LocalDateTime, OffsetDateTime}

trait RangeInstances {

  private val date2DateTimeFormatter =
    new DateTimeFormatterBuilder()
      .append(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
      .optionalStart()
      .appendFraction(ChronoField.NANO_OF_SECOND, 0, 6, true)
      .optionalEnd()
      .toFormatter()


  private val date2TzDateTimeFormatter =
    new DateTimeFormatterBuilder()
      .append(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
      .optionalStart()
      .appendFraction(ChronoField.NANO_OF_SECOND, 0, 6, true)
      .optionalEnd()
      .appendOffset("+HH:mm", "+00")
      .toFormatter()

  implicit val Int4RangeType: Meta[Range[Int]]       = rangeMeta("int4range")(_.toString, java.lang.Integer.parseInt)
  implicit val Int8RangeType: Meta[Range[Long]]      = rangeMeta("int8range")(_.toString, java.lang.Long.parseLong)
  implicit val NumRangeType: Meta[Range[BigDecimal]] = rangeMeta("numrange")(_.toString, BigDecimal.exact)

  implicit val DateRangeType: Meta[Range[LocalDate]]      = rangeMeta("daterange")(
    encode = toEndless[LocalDate](LocalDate.MIN, LocalDate.MAX, _.format(DateTimeFormatter.ISO_LOCAL_DATE)),
    decode = fromEndless(LocalDate.MIN, LocalDate.MAX, LocalDate.parse(_, DateTimeFormatter.ISO_LOCAL_DATE)))

  implicit val TsRangeType: Meta[Range[LocalDateTime]]    = rangeMeta("tsrange")(
    encode = toEndless[LocalDateTime](LocalDateTime.MIN, LocalDateTime.MAX, _.format(date2DateTimeFormatter)),
    decode = fromEndless(LocalDateTime.MIN, LocalDateTime.MAX, LocalDateTime.parse(_, date2DateTimeFormatter)))

  implicit val TstzRangeType: Meta[Range[OffsetDateTime]] = rangeMeta("tstzrange")(
    encode = toEndless[OffsetDateTime](OffsetDateTime.MIN, OffsetDateTime.MAX, _.format(date2TzDateTimeFormatter)),
    decode = fromEndless(OffsetDateTime.MIN, OffsetDateTime.MAX, OffsetDateTime.parse(_, date2TzDateTimeFormatter)))

  def rangeMeta[T](sqlRangeType: String)(encode: T => String, decode: String => T): Meta[Range[T]] =
    Meta.Advanced.other[PGobject](sqlRangeType).timap[Range[T]](
      o => Range.decode[T](o.getValue)(decode).toOption.orNull)(
      a => Option(a).map { a =>
        val o = new PGobject
        o.setType(sqlRangeType)
        o.setValue(Range.encode[T](a)(encode))
        o
      }.orNull
    )

  private def toEndless[T](min: T, max: T, encode: T => String): T => String = {
    case `min`  => "-infinity"
    case `max`  => "infinity"
    case finite => encode(finite)
  }

  private def fromEndless[T](min: T, max: T, decode: String => T): String => T = {
    case "-infinity" => min
    case "infinity"  => max
    case finite      => decode(finite)
  }
}
