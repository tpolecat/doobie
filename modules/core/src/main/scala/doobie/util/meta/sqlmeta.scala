// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util.meta

import doobie.enumerated.JdbcType._
import MetaConstructors.Basic

trait SqlMetaInstances {

  /** @group Instances */
  implicit val DateMeta: Meta[java.sql.Date] =
    Basic.one[java.sql.Date](
      Date,
      List(Char, VarChar, LongVarChar, Timestamp),
      _.getDate(_),
      _.setDate(_, _),
      _.updateDate(_, _))

  /** @group Instances */
  implicit val TimeMeta: Meta[java.sql.Time] =
    Basic.one[java.sql.Time](
      Time,
      List(Char, VarChar, LongVarChar, Timestamp),
      _.getTime(_),
      _.setTime(_, _),
      _.updateTime(_, _))

  /** @group Instances */
  implicit val TimestampMeta: Meta[java.sql.Timestamp] =
    Basic.one[java.sql.Timestamp](
      Timestamp,
      List(Char, VarChar, LongVarChar, Date, Time),
      _.getTimestamp(_),
      _.setTimestamp(_, _),
      _.updateTimestamp(_, _))

  /** @group Instances */
  implicit val JavaUtilDateMeta: Meta[java.util.Date] =
    DateMeta.imap[java.util.Date](a => a)(d => new java.sql.Date(d.getTime))

}
