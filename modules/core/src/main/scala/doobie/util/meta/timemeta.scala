// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util.meta

import doobie.enumerated.JdbcType._
import doobie.util.meta.MetaConstructors.Basic

/**
 * Instances for Java time classes that follow the JDBC specification.
 */
trait TimeMetaInstances {
  import Predef.classOf

  /** @group Instances */
  implicit val JavaOffsetDateTimeMeta: Meta[java.time.OffsetDateTime] =
    Basic.oneObject(TimestampWithTimezone, Nil, classOf[java.time.OffsetDateTime])

  /** @group Instances */
  implicit val JavaLocalDateMeta: Meta[java.time.LocalDate] =
    Basic.oneObject(Date, Nil, classOf[java.time.LocalDate])

  /** @group Instances */
  implicit val JavaLocalTimeMeta: Meta[java.time.LocalTime] =
    Basic.oneObject(Time, Nil, classOf[java.time.LocalTime])

  /** @group Instances */
  implicit val JavaLocalDateTimeMeta: Meta[java.time.LocalDateTime] =
    Basic.oneObject(Timestamp, Nil, classOf[java.time.LocalDateTime])

  /** @group Instances */
  implicit val JavaOffsetTimeMeta: Meta[java.time.OffsetTime] =
    Basic.oneObject(TimeWithTimezone, Nil, classOf[java.time.OffsetTime])

  // extra instances not in the spec

  /** @group Instances */
  implicit val JavaInstantMeta: Meta[java.time.Instant] =
    JavaOffsetDateTimeMeta.imap(_.toInstant)(_.atOffset(java.time.ZoneOffset.UTC))

  /** @group Instances */
  implicit val JavaTimeZoneId: Meta[java.time.ZoneId] = {
    def parse(str: String) = try {
      Right(java.time.ZoneId.of(str))
    } catch {
      case e: java.time.DateTimeException => Left(e.getMessage)
    }

    Meta[String].tiemap(parse(_))(_.getId)
  }

}
