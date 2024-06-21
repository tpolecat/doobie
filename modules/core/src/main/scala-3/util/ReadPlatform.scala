// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import scala.deriving.Mirror
import doobie.util.shapeless.OrElse

trait ReadPlatform:

  // Generic Read for products.
  given derived[P <: Product, A](
      using
      m: Mirror.ProductOf[P],
      i: A =:= m.MirroredElemTypes,
      w: MkRead[A]
  ): MkRead[P] = {
    val read = w.map(a => m.fromProduct(i(a)))
    MkRead.lift(read)
  }

  // Generic Read for option of products.
  given derivedOption[P <: Product, A](
      using
      m: Mirror.ProductOf[P],
      i: A =:= m.MirroredElemTypes,
      w: MkRead[Option[A]]
  ): MkRead[Option[P]] = {
    val read = w.map(a => a.map(a => m.fromProduct(i(a))))
    MkRead.lift(read)
  }

  // Derivation base case for product types (1-element)
  given productBase[H](
      using H: Read[H] OrElse MkRead[H]
  ): MkRead[H *: EmptyTuple] = {
    val head = H.unify
    new MkRead(
      head.gets,
      (rs, n) => head.unsafeGet(rs, n) *: EmptyTuple
    )
  }

  // Read for head and tail.
  given product[H, T <: Tuple](
      using
      H: Read[H] OrElse MkRead[H],
      T: MkRead[T]
  ): MkRead[H *: T] = {
    val head = H.unify

    new MkRead[H *: T](
      head.gets ++ T.gets,
      (rs, n) => head.unsafeGet(rs, n) *: T.unsafeGet(rs, n + head.length)
    )
  }

  given optProductBase[H](
      using H: Read[Option[H]] OrElse MkRead[Option[H]]
  ): MkRead[Option[H *: EmptyTuple]] = {
    val head = H.unify
    MkRead[Option[H *: EmptyTuple]](
      head.gets,
      (rs, n) => head.unsafeGet(rs, n).map(_ *: EmptyTuple)
    )
  }

  given optProduct[H, T <: Tuple](
      using
      H: Read[Option[H]] OrElse MkRead[Option[H]],
      T: MkRead[Option[T]]
  ): MkRead[Option[H *: T]] = {
    val head = H.unify

    new MkRead[Option[H *: T]](
      head.gets ++ T.gets,
      (rs, n) =>
        for {
          h <- head.unsafeGet(rs, n)
          t <- T.unsafeGet(rs, n + head.length)
        } yield h *: t
    )
  }

  given optProductOptBase[H](
      using H: Read[Option[H]] OrElse MkRead[Option[H]]
  ): MkRead[Option[Option[H] *: EmptyTuple]] = {
    val head = H.unify

    MkRead[Option[Option[H] *: EmptyTuple]](
      head.gets,
      (rs, n) => head.unsafeGet(rs, n).map(h => Some(h) *: EmptyTuple)
    )
  }

  given optProductOpt[H, T <: Tuple](
      using
      H: Read[Option[H]] OrElse MkRead[Option[H]],
      T: MkRead[Option[T]]
  ): MkRead[Option[Option[H] *: T]] = {
    val head = H.unify

    new MkRead[Option[Option[H] *: T]](
      head.gets ++ T.gets,
      (rs, n) => T.unsafeGet(rs, n + head.length).map(head.unsafeGet(rs, n) *: _)
    )
  }
