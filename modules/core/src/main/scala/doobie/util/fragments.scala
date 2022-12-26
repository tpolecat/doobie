// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie
package util

import doobie.implicits._
import cats.Reducible
import cats.syntax.all._

/** Module of `Fragment` constructors. */
object fragments {

  /** Returns `VALUES (fs0), (fs1), ...`. */
  def values[F[_]: Reducible, A](fs: F[A])(implicit w: util.Write[A]): Fragment =
    fs.toList.map(a => fr0"(${w.toFragment(a)})").foldSmash1(fr0"VALUES ", fr",", fr"")

  /** Returns `f IN (fs0, fs1, ...)`. */
  def in[F[_]: Reducible, A: util.Put](f: Fragment, fs: F[A]): Fragment =
    fs.toList.map(a => fr0"$a").foldSmash1(f ++ fr0"IN (", fr",", fr")")

  /** Returns `f IN ((fs0-A, fs0-B), (fs1-A, fs1-B), ...)`. */
  def in[F[_]: Reducible, A: util.Put, B: util.Put](f: Fragment, fs: F[(A,B)]): Fragment =
    fs.toList.map { case (a,b) => fr0"($a,$b)" }.foldSmash1(f ++ fr0"IN (", fr",", fr")")

  /** Returns `f IN ((fs0-A, fs0-B, fs0-C), (fs1-A, fs1-B, fs1-C), ...)`. */
  def in[F[_]: Reducible, A: util.Put, B: util.Put, C: util.Put](f: Fragment, fs: F[(A,B,C)]): Fragment =
    fs.toList.map { case (a,b,c) => fr0"($a,$b,$c)" }.foldSmash1(f ++ fr0"IN (", fr",", fr")")

  /** Returns `f NOT IN (fs0, fs1, ...)`. */
  def notIn[F[_]: Reducible, A: util.Put](f: Fragment, fs: F[A]): Fragment =
    fs.toList.map(a => fr0"$a").foldSmash1(f ++ fr0"NOT IN (", fr",", fr")")

  /** Returns `(f1) AND (f2) AND ... (fn)`. */
  def and(fs: Fragment*): Fragment =
    fs.toList.map(parentheses).intercalate(fr"AND")

  /** Returns `(f1) AND (f2) AND ... (fn)` for all defined fragments. */
  def andOpt(fs: Option[Fragment]*): Fragment =
    and(fs.toList.unite: _*)

  /** Returns `(f1) OR (f2) OR ... (fn)`. */
  def or(fs: Fragment*): Fragment =
    fs.toList.map(parentheses).intercalate(fr"OR")

  /** Returns `(f1) OR (f2) OR ... (fn)` for all defined fragments. */
  def orOpt(fs: Option[Fragment]*): Fragment =
    or(fs.toList.unite: _*)

  /** Returns `WHERE (f1) AND (f2) AND ... (fn)` or the empty fragment if `fs` is empty. */
  def whereAnd(fs: Fragment*): Fragment =
    if (fs.isEmpty) Fragment.empty else fr"WHERE" ++ and(fs: _*)

  /** Returns `WHERE (f1) AND (f2) AND ... (fn)` for defined `f`, if any, otherwise the empty fragment. */
  def whereAndOpt(fs: Option[Fragment]*): Fragment =
    whereAnd(fs.toList.unite: _*)

  /** Returns `WHERE (f1) OR (f2) OR ... (fn)` or the empty fragment if `fs` is empty. */
  def whereOr(fs: Fragment*): Fragment =
    if (fs.isEmpty) Fragment.empty else fr"WHERE" ++ or(fs: _*)

  /** Returns `WHERE (f1) OR (f2) OR ... (fn)` for defined `f`, if any, otherwise the empty fragment. */
  def whereOrOpt(fs: Option[Fragment]*): Fragment =
    whereOr(fs.toList.unite: _*)

  /** Returns `SET f1, f2, ... fn` or the empty fragment if `fs` is empty. */
  def set(fs: Fragment*): Fragment =
    if (fs.isEmpty) Fragment.empty else fr"SET" ++ fs.toList.intercalate(fr",")

  /** Returns `SET f1, f2, ... fn` for defined `f`, if any, otherwise the empty fragment. */
  def setOpt(fs: Option[Fragment]*): Fragment =
    set(fs.toList.unite: _*)

  /** Returns `(f)`. */
  def parentheses(f: Fragment): Fragment = fr0"(" ++ f ++ fr")"

  /** Returns `?,?,...,?` for the values in `a`. */
  def values[A](a: A)(implicit w: util.Write[A]): Fragment =
    w.toFragment(a)

}
