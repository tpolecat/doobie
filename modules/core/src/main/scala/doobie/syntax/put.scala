// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.syntax

import doobie.util.Put
import doobie.util.fragment.Fragment
import doobie.util.fragment.Elem.{Arg, Opt}
import doobie.util.pos.Pos

final class PutOps[A : Put](a: A) {
  def fr(implicit pos: Pos): Fragment[Any] = mkFragment("? ", pos)
  def fr0(implicit pos: Pos): Fragment[Any] = mkFragment("?", pos)

  private def mkFragment(sql: String, pos: Pos):Fragment[Any] = Fragment(sql, List(Arg(a, Put[A])), Some(pos))
}

final class OptionPutOps[A : Put](oa: Option[A]) {
  def fr(implicit pos: Pos): Fragment[Any] = mkFragment("? ", pos)
  def fr0(implicit pos: Pos): Fragment[Any] = mkFragment("?", pos)

  private def mkFragment(sql: String, pos: Pos):Fragment[Any] = Fragment(sql, List(Opt(oa, Put[A])), Some(pos))
}

trait ToPutOps {
  implicit def toPutOps[A : Put](a: A): PutOps[A] =
    new PutOps(a)
  implicit def toOptionPutOps[A : Put](a: Option[A]): OptionPutOps[A] =
    new OptionPutOps(a)
}

object put extends ToPutOps
