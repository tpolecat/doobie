// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

package object hi
    extends Modules
    with free.Modules
    with free.Types {
  object implicits extends free.Instances
}
