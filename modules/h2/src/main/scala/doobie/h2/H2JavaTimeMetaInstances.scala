// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.h2

import doobie.enumerated.JdbcType._
import doobie.util.meta.Meta
import doobie.util.meta.MetaConstructors.Basic

/** Instances for Java time classes that follow the JDBC specification.
  */
trait H2JavaTimeMetaInstances {
  import Predef.classOf

  implicit val JavaOffsetDateTimeMeta: Meta[java.time.OffsetDateTime] =
    Basic.oneObject(
      jdbcType = TimestampWithTimezone,
      checkedVendorType = Some("TIMESTAMP WITH TIME ZONE"),
      clazz = classOf[java.time.OffsetDateTime]
    )

  implicit val JavaLocalDateMeta: Meta[java.time.LocalDate] =
    Basic.oneObject(jdbcType = Date, checkedVendorType = None, clazz = classOf[java.time.LocalDate])

  implicit val JavaLocalTimeMeta: Meta[java.time.LocalTime] =
    Basic.oneObject(jdbcType = Time, checkedVendorType = Some("TIME"), clazz = classOf[java.time.LocalTime])

  implicit val JavaLocalDateTimeMeta: Meta[java.time.LocalDateTime] =
    Basic.oneObject(
      jdbcType = Timestamp,
      checkedVendorType = Some("TIMESTAMP"),
      clazz = classOf[java.time.LocalDateTime])

  implicit val JavaOffsetTimeMeta: Meta[java.time.OffsetTime] =
    Basic.oneObject(
      jdbcType = TimeWithTimezone,
      checkedVendorType = Some("TIME WITH TIME ZONE"),
      clazz = classOf[java.time.OffsetTime]
    )

  implicit val JavaInstantMeta: Meta[java.time.Instant] =
    JavaOffsetDateTimeMeta.timap(_.toInstant)(_.atOffset(java.time.ZoneOffset.UTC))

  implicit val JavaTimeZoneId: Meta[java.time.ZoneId] =
    doobie.implicits.javatimedrivernative.JavaTimeZoneId

}
