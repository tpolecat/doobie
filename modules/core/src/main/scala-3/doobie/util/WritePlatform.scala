// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import scala.deriving.Mirror

trait WritePlatform extends LowerPriority1WritePlatform:

  // Derivation for product types (i.e. case class)
  given derivedTuple[P <: Tuple, A](
      using
      m: Mirror.ProductOf[P],
      i: m.MirroredElemTypes =:= A,
      w: MkWrite[A]
  ): MkWrite[P] =
    MkWrite.derived[P, A]
    
trait LowerPriority1WritePlatform:
  given fromDerived[A](using ev: MkWrite[A]): Write[A] = ev.instance
    

