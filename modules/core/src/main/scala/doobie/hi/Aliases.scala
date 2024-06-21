// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.hi

trait Modules {

  /** @group Module Aliases - High level (safer) API */
  val HC = doobie.hi.connection

  /** @group Module Aliases - High level (safer) API */
  val HS = doobie.hi.statement

  /** @group Module Aliases - High level (safer) API */
  val HPS = doobie.hi.preparedstatement

  /** @group Module Aliases - High level (safer) API */
  val HRS = doobie.hi.resultset
}
