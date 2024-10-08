// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import scala.deriving.Mirror
import doobie.util.shapeless.OrElse

trait MkWritePlatform:

  // Derivation for product types (i.e. case class)
  given derived[P <: Product, A](
      using
      m: Mirror.ProductOf[P],
      i: m.MirroredElemTypes =:= A,
      w: MkWrite[A]
  ): MkWrite[P] =
    val write: Write[P] = w.instance.contramap(p => i(Tuple.fromProductTyped(p)))
    new MkWrite(write)

  // Derivation base case for product types (1-element)
  given productBase[H](
      using H: Write[H] `OrElse` MkWrite[H]
  ): MkWrite[H *: EmptyTuple] = {
    val headWrite = H.fold(identity, _.instance)
    new MkWrite(Write.Composite(
      List(headWrite),
      { case h *: EmptyTuple => List(h) }
    ))
  }

  // Derivation inductive case for product types
  given product[H, T <: Tuple](
      using
      H: Write[H] `OrElse` MkWrite[H],
      T: Write[H] `OrElse` MkWrite[H]
  ): MkWrite[H *: T] = {
    val headWrite = H.fold(identity, _.instance)
    val tailWrite = T.fold(identity, _.instance)

    new MkWrite(Write.Composite(
      List(headWrite, tailWrite),
      { case h *: t => List(h, t) }
    ))
  }
