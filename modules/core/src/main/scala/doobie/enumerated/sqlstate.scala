// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.enumerated

/** A simple newtype wrapper around JDBC SqlState values, which are always vendor-specific. Vendor modules like
  * '''doobie-postgres''' can provide modules of defined values.
  * @group Types
  */
final case class SqlState(val value: String) extends AnyVal
