// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import scala.deriving.Mirror

trait PutPlatform:

  // Put is available for single-element products.
  given [P <: Product, A](
      using
      m: Mirror.ProductOf[P],
      i: m.MirroredElemTypes =:= (A *: EmptyTuple),
      p: Put[A]
  ): MkPut[P] = {
    val put: Put[P] = p.contramap(p => i(Tuple.fromProductTyped(p)).head)
    MkPut.lift(put)
  }
