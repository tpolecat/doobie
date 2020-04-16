// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie
package util

import doobie.implicits._
import cats.Reducible
import cats.implicits._

/** Module of `Fragment` constructors. */
object fragments {

  /** Returns `f IN (fs0, fs1, ...)`. */
  def in[F[_]: Reducible, R, A: util.Put](f: Fragment[R], fs: F[A]): Fragment[R] =
    fs.toList.map[Fragment[R]](a => fr0"$a").foldSmash1(f ++ fr0"IN (", fr",", fr")")

  /** Returns `f NOT IN (fs0, fs1, ...)`. */
  def notIn[F[_]: Reducible, R, A: util.Put](f: Fragment[R], fs: F[A]): Fragment[R] =
    fs.toList.map[Fragment[R]](a => fr0"$a").foldSmash1(f ++ fr0"NOT IN (", fr",", fr")")

  /** Returns `(f1) AND (f2) AND ... (fn)`. */
  def and[R](fs: Fragment[R]*): Fragment[R] =
    fs.toList.map(parentheses[R]).intercalate(fr"AND")

  /** Returns `(f1) AND (f2) AND ... (fn)` for all defined fragments. */
  def andOpt[R](fs: Option[Fragment[R]]*): Fragment[R] =
    and(fs.toList.unite: _*)

  /** Returns `(f1) OR (f2) OR ... (fn)`. */
  def or[R](fs: Fragment[R]*): Fragment[R] =
    fs.toList.map(parentheses[R]).intercalate(fr"OR")

  /** Returns `(f1) OR (f2) OR ... (fn)` for all defined fragments. */
  def orOpt[R](fs: Option[Fragment[R]]*): Fragment[R] =
    or(fs.toList.unite: _*)

  /** Returns `WHERE (f1) AND (f2) AND ... (fn)` or the empty fragment if `fs` is empty. */
  def whereAnd[R](fs: Fragment[R]*): Fragment[R] =
    if (fs.isEmpty) Fragment.empty else fr"WHERE" ++ and(fs: _*)

  /** Returns `WHERE (f1) AND (f2) AND ... (fn)` for defined `f`, if any, otherwise the empty fragment. */
  def whereAndOpt[R](fs: Option[Fragment[R]]*): Fragment[R] =
    whereAnd(fs.toList.unite: _*)

  /** Returns `WHERE (f1) OR (f2) OR ... (fn)` or the empty fragment if `fs` is empty. */
  def whereOr[R](fs: Fragment[R]*): Fragment[R] =
    if (fs.isEmpty) Fragment.empty else fr"WHERE" ++ or(fs: _*)

  /** Returns `WHERE (f1) OR (f2) OR ... (fn)` for defined `f`, if any, otherwise the empty fragment. */
  def whereOrOpt[R](fs: Option[Fragment[R]]*): Fragment[R] =
    whereOr(fs.toList.unite: _*)

  /** Returns `SET f1, f2, ... fn` or the empty fragment if `fs` is empty. */
  def set[R](fs: Fragment[R]*): Fragment[R] =
    if (fs.isEmpty) Fragment.empty else fr"SET" ++ fs.toList.intercalate(fr",")

  /** Returns `SET f1, f2, ... fn` for defined `f`, if any, otherwise the empty fragment. */
  def setOpt[R](fs: Option[Fragment[R]]*): Fragment[R] =
    set(fs.toList.unite: _*)

  /** Returns `(f)`. */
  def parentheses[R](f: Fragment[R]): Fragment[R] = fr0"(" ++ f ++ fr")"

  /** Returns `?,?,...,?` for the values in `a`. */
  def values[A](a: A)(implicit w: util.Write[A]): Fragment[Any] =
    w.toFragment(a)

}
