// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import scala.deriving.Mirror
import doobie.util.shapeless.OrElse

trait WritePlatform:

  // Derivation for product types (i.e. case class)
  given derived[P <: Product, A](
      using
      m: Mirror.ProductOf[P],
      i: m.MirroredElemTypes =:= A,
      w: MkWrite[A]
  ): MkWrite[P] = {
    val write: Write[P] = w.contramap(p => i(Tuple.fromProductTyped(p)))
    MkWrite.lift(write)
  }

  // Derivation for optional product types
  given derivedOption[P <: Product, A](
      using
      m: Mirror.ProductOf[P],
      i: m.MirroredElemTypes =:= A,
      w: MkWrite[Option[A]]
  ): MkWrite[Option[P]] = {
    val write: Write[Option[P]] = w.contramap(op => op.map(p => i(Tuple.fromProductTyped(p))))
    MkWrite.lift(write)
  }

  // Derivation base case for product types (1-element)
  given productBase[H](
      using H: Write[H] OrElse MkWrite[H]
  ): MkWrite[H *: EmptyTuple] = {
    val head = H.unify
    MkWrite(
      head.puts,
      { case h *: t => head.toList(h) },
      { case (ps, n, h *: t) => head.unsafeSet(ps, n, h) },
      { case (rs, n, h *: t) => head.unsafeUpdate(rs, n, h) }
    )
  }

  // Derivation inductive case for product types
  given product[H, T <: Tuple](
      using
      H: Write[H] OrElse MkWrite[H],
      T: MkWrite[T]
  ): MkWrite[H *: T] = {
    val head = H.unify

    MkWrite(
      head.puts ++ T.puts,
      { case h *: t => head.toList(h) ++ T.toList(t) },
      { case (ps, n, h *: t) => head.unsafeSet(ps, n, h); T.unsafeSet(ps, n + head.length, t) },
      { case (rs, n, h *: t) => head.unsafeUpdate(rs, n, h); T.unsafeUpdate(rs, n + head.length, t) }
    )
  }

  // Derivation base case for Option of product types (1-element)
  given optProductBase[H](
      using H: Write[Option[H]] OrElse MkWrite[Option[H]]
  ): MkWrite[Option[H *: EmptyTuple]] = {
    val head = H.unify

    MkWrite[Option[H *: EmptyTuple]](
      head.puts,
      i => head.toList(i.map { case h *: EmptyTuple => h }),
      (ps, n, i) => head.unsafeSet(ps, n, i.map { case h *: EmptyTuple => h }),
      (rs, n, i) => head.unsafeUpdate(rs, n, i.map { case h *: EmptyTuple => h })
    )
  }

  // Write[Option[H]], Write[Option[T]] implies Write[Option[H *: T]]
  given optProduct[H, T <: Tuple](
      using
      H: Write[Option[H]] OrElse MkWrite[Option[H]],
      T: MkWrite[Option[T]]
  ): MkWrite[Option[H *: T]] =
    val head = H.unify

    def split[A](i: Option[H *: T])(f: (Option[H], Option[T]) => A): A =
      i.fold(f(None, None)) { case h *: t => f(Some(h), Some(t)) }

    MkWrite(
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

  // Derivation base case for Option of product types (where the head element is Option)
  given optProductOptBase[H](
      using H: Write[Option[H]] OrElse MkWrite[Option[H]]
  ): MkWrite[Option[Option[H] *: EmptyTuple]] = {
    val head = H.unify

    MkWrite[Option[Option[H] *: EmptyTuple]](
      head.puts,
      i => head.toList(i.flatMap { case ho *: EmptyTuple => ho }),
      (ps, n, i) => head.unsafeSet(ps, n, i.flatMap { case ho *: EmptyTuple => ho }),
      (rs, n, i) => head.unsafeUpdate(rs, n, i.flatMap { case ho *: EmptyTuple => ho })
    )
  }

  // Write[Option[H]], Write[Option[T]] implies Write[Option[Option[H] *: T]]
  given optProductOpt[H, T <: Tuple](
      using
      H: Write[Option[H]] OrElse MkWrite[Option[H]],
      T: MkWrite[Option[T]]
  ): MkWrite[Option[Option[H] *: T]] =
    val head = H.unify

    def split[A](i: Option[Option[H] *: T])(f: (Option[H], Option[T]) => A): A =
      i.fold(f(None, None)) { case oh *: t => f(oh, Some(t)) }

    MkWrite(
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
