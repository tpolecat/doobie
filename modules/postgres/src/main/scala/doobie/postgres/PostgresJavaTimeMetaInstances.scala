// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import doobie.enumerated.JdbcType._
import doobie.util.meta.Meta
import doobie.util.meta.MetaConstructors.Basic

/** Instances for Java time classes that follow the JDBC specification.
  */
trait PostgresJavaTimeMetaInstances {
  import Predef.classOf

  /** @group Instances */
  implicit val JavaOffsetDateTimeMeta: Meta[java.time.OffsetDateTime] =
    Basic.oneObject(
      jdbcType = Timestamp,
      checkedVendorType = Some("timestamptz"),
      clazz = classOf[java.time.OffsetDateTime]
    )

  /** @group Instances */
  implicit val JavaLocalDateMeta: Meta[java.time.LocalDate] =
    Basic.oneObject(Date, None, classOf[java.time.LocalDate])

  /** @group Instances */
  implicit val JavaLocalTimeMeta: Meta[java.time.LocalTime] =
    Basic.oneObject(Time, Some("time"), classOf[java.time.LocalTime])

  /** @group Instances */
  implicit val JavaLocalDateTimeMeta: Meta[java.time.LocalDateTime] =
    Basic.oneObject(Timestamp, Some("timestamp"), classOf[java.time.LocalDateTime])

  /** @group Instances */
  implicit val JavaOffsetTimeMeta: Meta[java.time.OffsetTime] =
    Basic.oneObject(
      Time,
      Some("timetz"),
      classOf[java.time.OffsetTime]
    )

  // extra instances not in the spec

  /** @group Instances */
  implicit val JavaInstantMeta: Meta[java.time.Instant] =
    JavaOffsetDateTimeMeta.timap(_.toInstant)(_.atOffset(java.time.ZoneOffset.UTC))

  /** @group Instances */
  implicit val JavaTimeZoneId: Meta[java.time.ZoneId] = {
    def parse(str: String) =
      try {
        Right(java.time.ZoneId.of(str))
      } catch {
        case e: java.time.DateTimeException => Left(e.getMessage)
      }

    Meta[String].tiemap(parse(_))(_.getId)
  }

}
