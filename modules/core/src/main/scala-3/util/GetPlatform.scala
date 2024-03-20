// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import scala.deriving.Mirror

trait GetPlatform:

  // Get is available for single-element products.
  given x[P <: Product, A](
      using
      p: Mirror.ProductOf[P],
      i: p.MirroredElemTypes =:= (A *: EmptyTuple),
      g: Get[A]
  ): MkGet[P] = {
    val get = g.map(a => p.fromProduct(a *: EmptyTuple))
    MkGet.lift(get)
  }
