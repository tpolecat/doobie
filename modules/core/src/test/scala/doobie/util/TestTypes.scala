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

  case class HasCustomReadWrite0(c: CustomReadWrite, s: String)
  case class HasCustomReadWrite1(s: String, c: CustomReadWrite)
  case class HasOptCustomReadWrite0(c: Option[CustomReadWrite], s: String)
  case class HasOptCustomReadWrite1(s: String, c: Option[CustomReadWrite])

  case class HasCustomMeta0(c: CustomMeta, s: String)
  case class HasCustomMeta1(s: String, c: CustomMeta)
  case class HasOptCustomMeta0(c: Option[CustomMeta], s: String)
  case class HasOptCustomMeta1(s: String, c: Option[CustomMeta])

  case class CustomReadWrite(i: Int, s: String)

  object CustomReadWrite {
    implicit val write: Write[CustomReadWrite] = Write.fromPut[String].contramap(_.s)
    implicit val read: Read[CustomReadWrite] = Read.fromGet[String].map(CustomReadWrite(0, _))
  }

  case class CustomMeta(i: Int, notUsed: Int)

  object CustomMeta {
    implicit val put: Put[CustomMeta] = Put[Int].contramap(_.i)
    implicit val get: Get[CustomMeta] = Get[Int].tmap(CustomMeta(_, 0))
  }

}
