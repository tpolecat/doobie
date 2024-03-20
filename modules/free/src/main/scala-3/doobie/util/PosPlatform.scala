// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import doobie.util.pos.Pos
import scala.quoted.{Expr, Quotes}

trait PosPlatform {

  implicit inline def instance: Pos =
    ${ PosPlatform.originImpl }

}

object PosPlatform {

  def originImpl(using ctx: Quotes): Expr[Pos] = {
    val rootPosition = ctx.reflect.Position.ofMacroExpansion
    val file = Expr(rootPosition.sourceFile.path)
    val line = Expr(rootPosition.startLine + 1)
    '{ Pos($file, $line) }
  }

}
