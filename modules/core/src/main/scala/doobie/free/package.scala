// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie

/** The `free` module gives you access to the low-level API that directly corresponds to the JDBC methods. Because this
  * is the low level API, care needs to be taken to close resources, just like you would if using the JDBC API directly.
  * When possible it is recommended to use the high level API (e.g. doobie.HC, doobie.HPS) as they handle
  * resource-safety for you.
  */
package object free
    extends Types
    with Modules {
  object implicits extends Instances
}
