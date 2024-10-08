// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import shapeless.{HList, HNil, ::, Generic, Lazy, OrElse}
import shapeless.labelled.FieldType

trait MkWritePlatform extends LowerPriority0MkWrite {

  // Derivation base case for shapelss record (1-element)
  implicit def recordBase[K <: Symbol, H](
      implicit H: Write[H] OrElse MkWrite[H]
  ): MkWrite[FieldType[K, H] :: HNil] = {
    val head = H.fold(identity, _.instance)

    new MkWrite(
      Write.Composite(List(head), { case h :: HNil => List(h) })
    )
  }

  // Derivation base case for product types (1-element)
  implicit def productBase[H](
      implicit H: Write[H] OrElse MkWrite[H]
  ): MkWrite[H :: HNil] = {
    val head = H.fold(identity, _.instance)

    new MkWrite[H :: HNil](
      Write.Composite(List(head), { case h :: HNil => List(h) })
    )
  }

}

trait LowerPriority0MkWrite extends LowerPriority1MkWrite {

  // Derivation inductive case for product types
  implicit def product[H, T <: HList](
      implicit
      H: Write[H] OrElse MkWrite[H],
      T: Write[T] OrElse MkWrite[T]
  ): MkWrite[H :: T] = {
    val head = H.fold(identity, _.instance)
    val tail = T.fold(identity, _.instance)

    new MkWrite[H :: T](
      Write.Composite(
        List(head, tail),
        { case h :: t => List(h, t) }
      )
    )
  }

  // Derivation inductive case for shapeless records
  implicit def record[K <: Symbol, H, T <: HList](
      implicit
      H: Write[H] OrElse MkWrite[H],
      T: Write[T] OrElse MkWrite[T]
  ): MkWrite[FieldType[K, H] :: T] = {
    val head = H.fold(identity, _.instance)
    val tail = T.fold(identity, _.instance)

    new MkWrite(
      Write.Composite(
        List(head, tail),
        {
          case h :: t => List(h, t)
        }
      )
    )
  }

}

trait LowerPriority1MkWrite extends LowerPriority2MkWrite {}

trait LowerPriority2MkWrite {

  // Derivation for product types (i.e. case class)
  implicit def generic[A, Repr <: HList](
      implicit
      gen: Generic.Aux[A, Repr],
      hlistWrite: Lazy[Write[Repr] OrElse MkWrite[Repr]]
  ): MkWrite[A] = {
    val g = hlistWrite.value.fold(identity, _.instance)

    // FIXME: 
    new MkWrite[A](
      Write.Composite(List(g), a => List(gen.to(a)))
    )
  }

}

object MkWritePlatform extends MkWritePlatform
