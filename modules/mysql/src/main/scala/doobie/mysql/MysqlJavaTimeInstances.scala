// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.mysql

import doobie.Meta
import doobie.enumerated.JdbcType
import doobie.util.meta.MetaConstructors.Basic

import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.OffsetDateTime
import java.time.ZoneOffset

/** Instances for JSR-310 date time types.
  *
  * Note that to ensure instants are preserved you may need to use one of the solutions described in
  * [[https://docs.oracle.com/cd/E17952_01/connector-j-8.0-en/connector-j-time-instants.html]].
  *
  * OffsetTime instance is not supported as there is no semantically equivalent type on the MySQL side.
  */
trait MysqlJavaTimeInstances {

  implicit val JavaTimeOffsetDateTimeMeta: Meta[OffsetDateTime] =
    Basic.oneObject(
      JdbcType.Timestamp,
      Some("TIMESTAMP"),
      classOf[OffsetDateTime]
    )

  implicit val JavaTimeInstantMeta: Meta[Instant] =
    JavaTimeOffsetDateTimeMeta.timap(_.toInstant)(OffsetDateTime.ofInstant(_, ZoneOffset.UTC))

  implicit val JavaTimeLocalDateTimeMeta: Meta[LocalDateTime] =
    Basic.oneObject(
      jdbcType = JdbcType.Timestamp,
      Some("DATETIME"),
      clazz = classOf[LocalDateTime]
    )

  implicit val JavaTimeLocalDateMeta: Meta[LocalDate] =
    Basic.oneObject(
      jdbcType = JdbcType.Date,
      checkedVendorType = None,
      clazz = classOf[LocalDate]
    )

  implicit val JavaTimeLocalTimeMeta: Meta[LocalTime] =
    Basic.oneObject(
      jdbcType = JdbcType.Time,
      checkedVendorType = None,
      clazz = classOf[LocalTime]
    )

  implicit val JavaTimeZoneId: Meta[java.time.ZoneId] =
    doobie.implicits.javatimedrivernative.JavaTimeZoneId

}
