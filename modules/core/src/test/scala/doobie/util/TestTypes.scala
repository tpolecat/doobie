// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import doobie.util.meta.Meta

object TestTypes {
  case class LenStr1(n: Int, s: String)

  case class LenStr2(n: Int, s: String)
  object LenStr2 {
    implicit val LenStrMeta: Meta[LenStr2] =
      Meta[String].timap(s => LenStr2(s.length, s))(_.s)
  }

  case object CaseObj

  case class SimpleCaseClass(i: Option[Int], s: String, os: Option[String])
  case class ComplexCaseClass(sc: SimpleCaseClass, osc: Option[SimpleCaseClass], i: Option[Int], s: String)

}
