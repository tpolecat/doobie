// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import scala.deriving.Mirror

trait TextPlatform { this: Text.type =>

  // EmptyTuple isn't a valid Text but a single-element Tuple is
  given [A](using csv: Text[A]): Text[A *: EmptyTuple] =
    csv.contramap(_.head)

  // Tuples of more that one element
  given [H, T <: Tuple](using h: Text[H], t: Text[T]): Text[H *: T] =
    (h product t).contramap(l => (l.head, l.tail))

  // Put is available for single-element products.
  given derived[P <: Product, A](
      using
      m: Mirror.ProductOf[P],
      i: m.MirroredElemTypes =:= A,
      t: Text[A]
  ): Text[P] =
    t.contramap(p => i(Tuple.fromProductTyped(p)))

}
