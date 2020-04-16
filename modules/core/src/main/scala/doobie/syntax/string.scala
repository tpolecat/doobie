// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.syntax

import cats.implicits._

import doobie.syntax.SqlInterpolator.SingleFragment
import doobie.util.Put
import doobie.util.fragment.{Elem, Fragment}
import doobie.util.pos.Pos
import doobie.util.lens.@>

/**
 * String interpolator for SQL literals. An expression of the form `sql".. $a ... $b ..."` with
 * interpolated values of type `A` and `B` (which must have instances of `Put`)
 * yields a value of type `[[Fragment]]`.
 */
final class SqlInterpolator(private val sc: StringContext) extends AnyVal {

  private def mkFragment[R](parts: List[SingleFragment[R]], token: Boolean, pos: Pos): Fragment[R] = {
    val last = if (token) Fragment[Any](" ", Nil, None) else Fragment.empty

    sc.parts.toList
      .map(sql => SingleFragment(Fragment(sql, Nil, Some(pos))))
      .zipAll(parts, SingleFragment.empty, SingleFragment(last))
      .flatMap { case (a, b) => List(a.fr, b.fr) }
      .combineAll
  }

  /**
   * Interpolator for a statement fragment that can contain interpolated values. When inserted
   * into the final SQL statement this fragment will be followed by a space. This is normally
   * what you want, and it makes it easier to concatenate fragments because you don't need to
   * think about intervening whitespace. If you do not want this behavior, use `fr0`.
   */
  def fr[R](a: SingleFragment[R]*)(implicit pos: Pos):Fragment[R] = mkFragment(a.toList, true, pos)

  /** Alternative name for the `fr0` interpolator. */
  def sql[R](a: SingleFragment[R]*)(implicit pos: Pos): Fragment[R] = mkFragment(a.toList, false, pos)

  /**
   * Interpolator for a statement fragment that can contain interpolated values. Unlike `fr` no
   * attempt is made to be helpful with respect to whitespace.
   */
  def fr0[R](a: SingleFragment[R]*)(implicit pos: Pos): Fragment[R] = mkFragment(a.toList, false, pos)

}

object SqlInterpolator {
  final case class SingleFragment[-R](fr: Fragment[R]) extends AnyVal
  object SingleFragment {
    val empty: SingleFragment[Any] = SingleFragment(Fragment.empty)

    def fromElem[R](elem: Elem[R]):SingleFragment[R] = SingleFragment(Fragment("?", elem :: Nil, None))

    implicit def fromPut[A](a: A)(implicit put: Put[A]): SingleFragment[Any] = fromElem(Elem.Arg(a, put))
    implicit def fromPutOption[A](a: Option[A])(implicit put: Put[A]): SingleFragment[Any] = fromElem(Elem.Opt(a, put))

    implicit def fromFragment[R](fr: Fragment[R]): SingleFragment[R] = SingleFragment(fr)

    implicit def fromLensPut[R, A](lens: R @> A)(implicit put: Put[A]): SingleFragment[R] = SingleFragment.fromElem(Elem.FunArg(lens.get, put))
    implicit def fromLensPutOption[R, A](lens: R @> Option[A])(implicit put: Put[A]): SingleFragment[R] = SingleFragment.fromElem(Elem.FunOpt(lens.get, put))
  }
}

trait ToSqlInterpolator {
  implicit def toSqlInterpolator(sc: StringContext): SqlInterpolator =
    new SqlInterpolator(sc)
}

object string extends ToSqlInterpolator
