// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import scala.deriving.Mirror
import doobie.util.shapeless.OrElse

trait WritePlatform:

  // Trivial write for empty tuple.
  given writeEmpty: MkWrite[EmptyTuple] =
    new MkWrite(Nil, _ => Nil, (_, _, _) => (), (_, _, _) => ())

  // Inductive write for writable head and tail.
  given writeTuple[H, T <: Tuple](
      using
      H: Write[H] OrElse MkWrite[H],
      T: => MkWrite[T]
  ): MkWrite[H *: T] = {
    val head = H.unify

    new MkWrite(
      head.puts ++ T.puts,
      { case h *: t => head.toList(h) ++ T.toList(t) },
      { case (ps, n, h *: t) => head.unsafeSet(ps, n, h); T.unsafeSet(ps, n + head.length, t) },
      { case (rs, n, h *: t) => head.unsafeUpdate(rs, n, h); T.unsafeUpdate(rs, n + head.length, t) }
    )
  }

  // Generic write for products.
  given derived[P <: Product, A](
      using
      m: Mirror.ProductOf[P],
      i: m.MirroredElemTypes =:= A,
      w: MkWrite[A]
  ): MkWrite[P] = {
    val write: Write[P] = w.contramap(p => i(Tuple.fromProductTyped(p)))
    MkWrite.lift(write)
  }

  // Trivial write for option of empty tuple.
  given woe: MkWrite[Option[EmptyTuple]] =
    new MkWrite[Option[EmptyTuple]](Nil, _ => Nil, (_, _, _) => (), (_, _, _) => ())

  // Trivial write for option of Unit.
  given wou: MkWrite[Option[Unit]] =
    new MkWrite[Option[Unit]](Nil, _ => Nil, (_, _, _) => (), (_, _, _) => ())

  // Write[Option[H]], Write[Option[T]] implies Write[Option[H *: T]]
  given cons1[H, T <: Tuple](
      using
      H: => Write[Option[H]] OrElse MkWrite[Option[H]],
      T: => MkWrite[Option[T]]
      // N: H <:!< Option[_],
  ): MkWrite[Option[H *: T]] =
    val head = H.unify

    def split[A](i: Option[H *: T])(f: (Option[H], Option[T]) => A): A =
      i.fold(f(None, None)) { case h *: t => f(Some(h), Some(t)) }

    new MkWrite(
      head.puts ++ T.puts,
      split(_) { (h, t) => head.toList(h) ++ T.toList(t) },
      (ps, n, i) =>
        split(i) { (h, t) =>
          head.unsafeSet(ps, n, h); T.unsafeSet(ps, n + head.length, t)
        },
      (rs, n, i) =>
        split(i) { (h, t) =>
          head.unsafeUpdate(rs, n, h); T.unsafeUpdate(rs, n + head.length, t)
        }
    )

  // Write[Option[H]], Write[Option[T]] implies Write[Option[Option[H] *: T]]
  given cons2[H, T <: Tuple](
      using
      H: Write[Option[H]] OrElse MkWrite[Option[H]],
      T: => MkWrite[Option[T]]
  ): MkWrite[Option[Option[H] *: T]] =
    val head = H.unify

    def split[A](i: Option[Option[H] *: T])(f: (Option[H], Option[T]) => A): A =
      i.fold(f(None, None)) { case oh *: t => f(oh, Some(t)) }

    new MkWrite(
      head.puts ++ T.puts,
      split(_) { (h, t) => head.toList(h) ++ T.toList(t) },
      (ps, n, i) =>
        split(i) { (h, t) =>
          head.unsafeSet(ps, n, h); T.unsafeSet(ps, n + head.length, t)
        },
      (rs, n, i) =>
        split(i) { (h, t) =>
          head.unsafeUpdate(rs, n, h); T.unsafeUpdate(rs, n + head.length, t)
        }
    )

  // Generic write for options of products.
  given writeOptionProduct[P <: Product, A](
      using
      m: Mirror.ProductOf[P],
      i: m.MirroredElemTypes =:= A,
      w: MkWrite[Option[A]]
  ): MkWrite[Option[P]] = {
    val write: Write[Option[P]] = w.contramap(op => op.map(p => i(Tuple.fromProductTyped(p))))
    MkWrite.lift(write)
  }
