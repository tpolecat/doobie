// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.hi

trait Modules {

  /** @group Module Aliases - Hi API */
  lazy val HC = doobie.hi.connection

  /** @group Module Aliases - Hi API */
  lazy val HS = doobie.hi.statement

  /** @group Module Aliases - Hi API */
  lazy val HPS = doobie.hi.preparedstatement

  /** @group Module Aliases - Hi API */
  lazy val HRS = doobie.hi.resultset
}
