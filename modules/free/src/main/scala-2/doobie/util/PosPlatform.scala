// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import doobie.util.pos.Pos
import scala.reflect.macros.blackbox

trait PosPlatform {

  /** A `Pos` can be forged on demand. */
  implicit def instance: Pos =
    macro PosPlatform.PosMacros.instance_impl

}

object PosPlatform {

  class PosMacros(val c: blackbox.Context) {
    import c.universe._
    def instance_impl: Tree = {
      val file = c.enclosingPosition.source.path
      val line = c.enclosingPosition.line
      q"_root_.doobie.util.pos.Pos($file, $line)"
    }
  }

}
