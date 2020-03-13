// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie
package util

import cats.Reducible
import cats.implicits._
import doobie.implicits._

/** Module of `Fragment` constructors. */
object fragments {

  /** Returns `(f IN (fs0, fs1, ...))`, or nothing for empty `fs`. */
  def in(f: Fragment, fs: Fragment*): Option[Fragment] =
    if (fs.isEmpty) None
    else
      Some(parentheses(f ++ fr"IN" ++ parentheses(comma(fs: _*))))

  /** Returns `(f IN (fs0, fs1, ...))`. */
  def inNe[F[_]: Reducible](f: Fragment, fs: F[Fragment]): Fragment =
    parentheses(f ++ fr"IN" ++ parentheses(comma(fs.toList: _*)))

  /** Returns `(f IN (fs0, fs1, ...))` for all defined fragments, or nothing for empty `fs`. */
  def inOpt(f: Fragment, fs: Option[Fragment]*): Option[Fragment] =
    in(f, fs.toList.unite: _*)

  /** Returns `(f NOT IN (fs0, fs1, ...))`, or nothing for empty `fs`. */
  def notIn(f: Fragment, fs: Fragment*): Option[Fragment] =
    if (fs.isEmpty) None
    else
      Some(parentheses(f ++ fr"NOT IN" ++ parentheses(comma(fs: _*))))

  /** Returns `(f NOT IN (fs0, fs1, ...))`. */
  def notInNe[F[_]: Reducible](f: Fragment, fs: F[Fragment]): Fragment =
    parentheses(f ++ fr"NOT IN" ++ parentheses(comma(fs.toList: _*)))

  /** Returns `(f NOT IN (fs0, fs1, ...))` for all defined fragments, or nothing for empty `fs`. */
  def notInOpt(f: Fragment, fs: Option[Fragment]*): Option[Fragment] =
    notIn(f, fs.toList.unite: _*)

  /** Returns `(f1 AND f2 AND ... fn)`, or nothing for empty `fs`. */
  def and(fs: Fragment*): Option[Fragment] =
    if (fs.isEmpty) None else Some(parentheses(fs.toList.intercalate(fr"AND")))

  /** Returns `(f1 AND f2 AND ... fn)`. */
  def andNe[F[_]: Reducible](fs: F[Fragment]): Fragment =
    parentheses(fs.intercalate(fr"AND"))

  /** Returns `(f1 AND f2 AND ... fn)` for all defined fragments, or nothing for empty `fs`. */
  def andOpt(fs: Option[Fragment]*): Option[Fragment] =
    and(fs.toList.unite: _*)

  /** Returns `(f1 OR f2 OR ... fn)`, or nothing for empty `fs`. */
  def or(fs: Fragment*): Option[Fragment] =
    if (fs.isEmpty) None else Some(parentheses(fs.toList.intercalate(fr"OR")))

  /** Returns `(f1 OR f2 OR ... fn)`. */
  def orNe[F[_]: Reducible](fs: F[Fragment]): Fragment =
    parentheses(fs.intercalate(fr"OR"))

  /** Returns `(f1 OR f2 OR ... fn)` for all defined fragments, or nothing for empty `fs`. */
  def orOpt(fs: Option[Fragment]*): Option[Fragment] =
    or(fs.toList.unite: _*)

  /** Returns `WHERE (f1 AND f2 AND ... fn)` or the empty fragment if `fs` is empty. */
  def whereAnd(fs: Fragment*): Fragment =
    and(fs: _*).fold(Fragment.empty)(fr"WHERE" ++ _)

  /** Returns `WHERE (f1 AND f2 AND ... fn)` for defined `f`, if any, otherwise the empty fragment. */
  def whereAndOpt(fs: Option[Fragment]*): Fragment =
    whereAnd(fs.toList.unite: _*)

  /** Returns `WHERE (f1 OR f2 OR ... fn)` or the empty fragment if `fs` is empty. */
  def whereOr(fs: Fragment*): Fragment =
    or(fs: _*).fold(Fragment.empty)(fr"WHERE" ++ _)

  /** Returns `WHERE (f1 OR f2 OR ... fn)` for defined `f`, if any, otherwise the empty fragment. */
  def whereOrOpt(fs: Option[Fragment]*): Fragment =
    whereOr(fs.toList.unite: _*)

  /** Returns `SET f1, f2, ... fn` or the empty fragment if `fs` is empty. */
  def set(fs: Fragment*): Fragment =
    if (fs.isEmpty) Fragment.empty else fr"SET" ++ comma(fs: _*)

  /** Returns `SET f1, f2, ... fn` for defined `f`, if any, otherwise the empty fragment. */
  def setOpt(fs: Option[Fragment]*): Fragment =
    set(fs.toList.unite: _*)

  /** Returns `(f)`. */
  def parentheses(f: Fragment): Fragment = fr0"(" ++ f ++ fr")"

  /** Returns `?,?,...,?` for the values in `a`. */
  def values[A](a: A)(implicit w: util.Write[A]): Fragment =
    w.toFragment(a)

  /** Returns `f1, f2, ... fn` or the empty fragment if `fs` is empty. */
  def comma(fs: Fragment*): Fragment =
    fs.toList.intercalate(fr",")

  /** Returns `ORDER BY f1, f2, ... fn` or the empty fragment if `fs` is empty. */
  def orderBy(fs: Fragment*): Fragment =
    if (fs.isEmpty) Fragment.empty else fr"ORDER BY" ++ comma(fs: _*)

  /** Returns `ORDER BY f1, f2, ... fn` for defined `f`, if any, otherwise the empty fragment. */
  def orderByOpt(fs: Option[Fragment]*): Fragment =
    orderBy(fs.toList.unite: _*)
}
