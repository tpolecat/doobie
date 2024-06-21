// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie
package util

import cats.data.NonEmptyList
import cats.syntax.all._
import cats.{Foldable, Functor, Reducible}
import doobie.implicits._

/** Module of `Fragment` constructors. */
object fragments {

  /** Returns `VALUES (fs0), (fs1), ...`. */
  def values[F[_]: Reducible, A](fs: F[A])(implicit w: util.Write[A]): Fragment =
    fr"VALUES" ++ comma(fs.toNonEmptyList.map(f => parentheses(values(f))))

  /** Returns `(f IN (fs0, fs1, ...))`. */
  def in[A: util.Put](f: Fragment, fs0: A, fs1: A, fs: A*): Fragment =
    in(f, NonEmptyList(fs0, fs1 :: fs.toList))

  /** Returns `(f IN (fs0, fs1, ...))`, or `false` for empty `fs`. */
  def in[F[_]: Reducible: Functor, A: util.Put](f: Fragment, fs: F[A]): Fragment =
    parentheses(f ++ fr" IN" ++ parentheses(comma(fs.map(a => fr"$a"))))

  def inOpt[F[_]: Foldable, A: util.Put](f: Fragment, fs: F[A]): Option[Fragment] =
    NonEmptyList.fromFoldable(fs).map(nel => in(f, nel))

  /** Returns `(f IN ((fs0-A, fs0-B), (fs1-A, fs1-B), ...))`, or `false` for empty `fs`. */
  def in[F[_]: Reducible: Functor, A: util.Put, B: util.Put](f: Fragment, fs: F[(A, B)]): Fragment =
    parentheses(f ++ fr" IN" ++ parentheses(comma(fs.map { case (a, b) => fr0"($a,$b)" })))

  /** Returns `(f NOT IN (fs0, fs1, ...))`. */
  def notIn[A: util.Put](f: Fragment, fs0: A, fs1: A, fs: A*): Fragment =
    notIn(f, NonEmptyList(fs0, fs1 :: fs.toList))

  /** Returns `(f NOT IN (fs0, fs1, ...))`, or `true` for empty `fs`. */
  def notIn[F[_]: Reducible: Functor, A: util.Put](f: Fragment, fs: F[A]): Fragment = {
    parentheses(f ++ fr" NOT IN" ++ parentheses(comma(fs.map(a => fr"$a"))))
  }

  def notInOpt[F[_]: Foldable, A: util.Put](f: Fragment, fs: F[A]): Option[Fragment] = {
    NonEmptyList.fromFoldable(fs).map(nel => notIn(f, nel))
  }

  /** Returns `(f1 AND f2 AND ... fn)`. */
  def and(f1: Fragment, f2: Fragment, fs: Fragment*): Fragment =
    and(NonEmptyList(f1, f2 :: fs.toList))

  /** Returns `(f1 AND f2 AND ... fn)` for a non-empty collection.
    * @param withParen
    *   If this is false, does not wrap the resulting expression with parenthesis
    */
  def and[F[_]: Reducible](fs: F[Fragment], withParen: Boolean = true): Fragment = {
    val expr = fs.reduceLeftTo(f => parentheses0(f))((f1, f2) => f1 ++ fr0" AND " ++ parentheses0(f2))
    if (withParen) parentheses(expr) else expr
  }

  /** Returns `(f1 AND f2 AND ... fn)` for all defined fragments, returning None if there are no defined fragments */
  def andOpt(opt1: Option[Fragment], opt2: Option[Fragment], opts: Option[Fragment]*): Option[Fragment] = {
    andOpt((opt1 :: opt2 :: opts.toList).flatten)
  }

  /** Returns `(f1 AND f2 AND ... fn)`, or None if the collection is empty. */
  def andOpt[F[_]: Foldable](fs: F[Fragment], withParen: Boolean = true): Option[Fragment] = {
    NonEmptyList.fromFoldable(fs).map(nel => and(nel, withParen))
  }

  /** Similar to andOpt, but defaults to TRUE if passed an empty collection */
  def andFallbackTrue[F[_]: Foldable](fs: F[Fragment]): Fragment = {
    andOpt(fs).getOrElse(fr"TRUE")
  }

  /** Returns `(f1 OR f2 OR ... fn)`. */
  def or(f1: Fragment, f2: Fragment, fs: Fragment*): Fragment =
    or(NonEmptyList(f1, f2 :: fs.toList))

  /** Returns `(f1 OR f2 OR ... fn)` for a non-empty collection.
    *
    * @param withParen
    *   If this is false, does not wrap the resulting expression with parenthesis
    */
  def or[F[_]: Reducible](fs: F[Fragment], withParen: Boolean = true): Fragment = {
    val expr = fs.reduceLeftTo(f => parentheses0(f))((f1, f2) => f1 ++ fr0" OR " ++ parentheses0(f2))
    if (withParen) parentheses(expr) else expr
  }

  /** Returns `(f1 OR f2 OR ... fn)` for all defined fragments, returning None if there are no defined fragments */
  def orOpt(opt1: Option[Fragment], opt2: Option[Fragment], opts: Option[Fragment]*): Option[Fragment] = {
    orOpt((opt1 :: opt2 :: opts.toList).flatten)
  }

  /** Returns `(f1 OR f2 OR ... fn)`, or None if the collection is empty. */
  def orOpt[F[_]: Foldable](fs: F[Fragment], withParen: Boolean = true): Option[Fragment] = {
    NonEmptyList.fromFoldable(fs).map(nel => or(nel, withParen))
  }

  /** Similar to orOpt, but defaults to FALSE if passed an empty collection */
  def orFallbackFalse[F[_]: Foldable](fs: F[Fragment]): Fragment = {
    orOpt(fs).getOrElse(fr"FALSE")
  }

  /** Returns `WHERE f1 AND f2 AND ... fn`. */
  def whereAnd(f1: Fragment, fs: Fragment*): Fragment =
    whereAnd(NonEmptyList(f1, fs.toList))

  /** Returns `WHERE f1 AND f2 AND ... fn` or the empty fragment if `fs` is empty. */
  def whereAnd[F[_]: Reducible](fs: F[Fragment]): Fragment =
    fr"WHERE" ++ and(fs, withParen = false)

  /** Returns `WHERE f1 AND f2 AND ... fn` for defined `f`, if any, otherwise the empty fragment. */
  def whereAndOpt(f1: Option[Fragment], f2: Option[Fragment], fs: Option[Fragment]*): Fragment = {
    whereAndOpt((f1 :: f2 :: fs.toList).flatten)
  }

  /** Returns `WHERE f1 AND f2 AND ... fn` if collection is not empty. If collection is empty returns an empty fragment.
    */
  def whereAndOpt[F[_]: Foldable](fs: F[Fragment]): Fragment = {
    NonEmptyList.fromFoldable(fs) match {
      case Some(nel) => whereAnd(nel)
      case None      => Fragment.empty
    }
  }

  /** Returns `WHERE f1 OR f2 OR ... fn`. */
  def whereOr(f1: Fragment, fs: Fragment*): Fragment =
    whereOr(NonEmptyList(f1, fs.toList))

  /** Returns `WHERE f1 OR f2 OR ... fn` or the empty fragment if `fs` is empty. */
  def whereOr[F[_]: Reducible](fs: F[Fragment]): Fragment =
    fr"WHERE" ++ or(fs, withParen = false)

  /** Returns `WHERE f1 OR f2 OR ... fn` for defined `f`, if any, otherwise the empty fragment. */
  def whereOrOpt(f1: Option[Fragment], f2: Option[Fragment], fs: Option[Fragment]*): Fragment = {
    whereOrOpt((f1 :: f2 :: fs.toList).flatten)
  }

  /** Returns `WHERE f1 OR f2 OR ... fn` if collection is not empty. If collection is empty returns an empty fragment.
    */
  def whereOrOpt[F[_]: Foldable](fs: F[Fragment]): Fragment = {
    NonEmptyList.fromFoldable(fs) match {
      case Some(nel) => whereOr(nel)
      case None      => Fragment.empty
    }
  }

  /** Returns `SET f1, f2, ... fn`. */
  def set(f1: Fragment, fs: Fragment*): Fragment =
    set(NonEmptyList(f1, fs.toList))

  /** Returns `SET f1, f2, ... fn`. */
  def set[F[_]: Reducible](fs: F[Fragment]): Fragment =
    fr"SET" ++ comma(fs)

  /** Returns `(f) `. */
  def parentheses(f: Fragment): Fragment = fr0"(" ++ f ++ fr")"

  /** Returns `(f)`. */
  def parentheses0(f: Fragment): Fragment = fr0"(" ++ f ++ fr0")"

  /** Returns `?,?,...,?` for the values in `a`. */
  def values[A](a: A)(implicit w: util.Write[A]): Fragment =
    w.toFragment(a)

  /** Returns `f1, f2, ... fn`. */
  def comma(f1: Fragment, f2: Fragment, fs: Fragment*): Fragment =
    comma(NonEmptyList(f1, f2 :: fs.toList))

  /** Returns `f1, f2, ... fn`. */
  def comma[F[_]: Reducible](fs: F[Fragment]): Fragment =
    fs.nonEmptyIntercalate(fr",")

  /** Returns `ORDER BY f1, f2, ... fn`. */
  def orderBy(f1: Fragment, fs: Fragment*): Fragment =
    orderBy(NonEmptyList(f1, fs.toList))

  def orderBy[F[_]: Reducible](fs: F[Fragment]): Fragment =
    fr"ORDER BY" ++ comma(fs)

  /** Returns `ORDER BY f1, f2, ... fn` or the empty fragment if `fs` is empty. */
  def orderByOpt[F[_]: Foldable](fs: F[Fragment]): Fragment =
    NonEmptyList.fromFoldable(fs) match {
      case Some(nel) => orderBy(nel)
      case None      => Fragment.empty
    }

  /** Returns `ORDER BY f1, f2, ... fn` for defined `f`, if any, otherwise the empty fragment. */
  def orderByOpt(f1: Option[Fragment], f2: Option[Fragment], fs: Option[Fragment]*): Fragment =
    orderByOpt((f1 :: f2 :: fs.toList).flatten)
}
