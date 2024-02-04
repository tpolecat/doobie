// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import scala.deriving.Mirror
import doobie.util.shapeless.OrElse

trait ReadPlatform:

  // Trivial Read for EmptyTuple
  given readEmpty: MkRead[EmptyTuple] =
    new MkRead[EmptyTuple](Nil, (_, _) => EmptyTuple)

  // Read for head and tail.
  given readTuple[H, T <: Tuple](
      using
      H: Read[H] OrElse MkRead[H],
      T: => MkRead[T]
  ): MkRead[H *: T] = {
    val head = H.unify

    new MkRead[H *: T](
      head.gets ++ T.gets,
      (rs, n) => head.unsafeGet(rs, n) *: T.unsafeGet(rs, n + head.length)
    )
  }

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

  given roe: MkRead[Option[EmptyTuple]] =
    new MkRead[Option[EmptyTuple]](Nil, (_, _) => Some(EmptyTuple))

  given rou: MkRead[Option[Unit]] =
    new MkRead[Option[Unit]](Nil, (_, _) => Some(()))

  given cons1[H, T <: Tuple](
      using
      H: Read[Option[H]] OrElse MkRead[Option[H]],
      T: => MkRead[Option[T]]
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

  given cons2[H, T <: Tuple](
      using
      H: Read[Option[H]] OrElse MkRead[Option[H]],
      T: => MkRead[Option[T]]
  ): MkRead[Option[Option[H] *: T]] = {
    val head = H.unify

    new MkRead[Option[Option[H] *: T]](
      head.gets ++ T.gets,
      (rs, n) => T.unsafeGet(rs, n + head.length).map(head.unsafeGet(rs, n) *: _)
    )
  }

  // Generic Read for option of products.
  given readOptionProduct[P <: Product, A](
      using
      m: Mirror.ProductOf[P],
      i: A =:= m.MirroredElemTypes,
      w: MkRead[Option[A]]
  ): MkRead[Option[P]] = {
    val read = w.map(a => a.map(a => m.fromProduct(i(a))))
    MkRead.lift(read)
  }
