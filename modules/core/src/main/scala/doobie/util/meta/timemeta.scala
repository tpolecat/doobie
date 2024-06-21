// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util.meta

import doobie.enumerated.JdbcType._
import doobie.util.meta.MetaConstructors.Basic

/** Basic instances for Java time classes that follow the JDBC specification. These instances lack more precise type
  * checking (by checking against the vendor type name for columns and parameters) so this should only be used if you're
  * not using one of the databases which doobie has more precise Meta instances for. (e.g. PostgreSQL / MySQL)
  */
trait TimeMetaInstances {
  import Predef.classOf

  /** @group Instances */
  implicit val JavaOffsetDateTimeMeta: Meta[java.time.OffsetDateTime] =
    Basic.oneObject(
      jdbcType = Timestamp,
      checkedVendorType = None,
      clazz = classOf[java.time.OffsetDateTime]
    )

  /** @group Instances */
  implicit val JavaLocalDateMeta: Meta[java.time.LocalDate] =
    Basic.oneObject(jdbcType = Date, checkedVendorType = None, clazz = classOf[java.time.LocalDate])

  /** @group Instances */
  implicit val JavaLocalTimeMeta: Meta[java.time.LocalTime] =
    Basic.oneObject(jdbcType = Time, checkedVendorType = None, clazz = classOf[java.time.LocalTime])

  /** @group Instances */
  implicit val JavaLocalDateTimeMeta: Meta[java.time.LocalDateTime] =
    Basic.oneObject(jdbcType = Timestamp, checkedVendorType = None, clazz = classOf[java.time.LocalDateTime])

  // extra instances not in the spec

  /** @group Instances */
  implicit val JavaInstantMeta: Meta[java.time.Instant] =
    JavaOffsetDateTimeMeta.imap(_.toInstant)(_.atOffset(java.time.ZoneOffset.UTC))

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
