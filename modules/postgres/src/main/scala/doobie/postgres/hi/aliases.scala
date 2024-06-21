// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres.hi

trait Modules {
  val PHPC = pgconnection
  val PHC = connection
  val PHLO = largeobject
  val PHLOM = largeobjectmanager
  val PHLOS = lostreaming
}
