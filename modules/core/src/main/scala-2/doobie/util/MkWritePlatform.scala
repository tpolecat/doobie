// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import shapeless.{HList, HNil, ::, Generic, Lazy, <:!<, OrElse}
import shapeless.labelled.FieldType
import java.sql.{PreparedStatement, ResultSet}
import MkWritePlatform.{ToListFunc, UnsafeSetFunc, UnsafeUpdateFunc}

trait MkWritePlatform extends LowerPriority0MkWrite {

  type ToListFunc[A] = A => List[Any]
  type UnsafeSetFunc[A] = (PreparedStatement, Int, A) => Unit
  type UnsafeUpdateFunc[A] = (ResultSet, Int, A) => Unit

  // Derivation base case for shapelss record (1-element)
  implicit def recordBase[K <: Symbol, H](
      implicit H: Write[H] OrElse Mkk[H]
  ): Mkk[FieldType[K, H] :: HNil] = {
    val head = H.fold(identity, _.instance)

    new Mkk(
      Write.Composite(List(head), { case h :: HNil => List(h) })
    )
  }

  // Derivation base case for product types (1-element)
  implicit def productBase[H](
      implicit H: Write[H] OrElse Mkk[H]
  ): Mkk[H :: HNil] = {
    val head = H.fold(identity, _.instance)

    new Mkk[H :: HNil](
      Write.Composite(List(head), { case h :: HNil => List(h) })
    )
  }

}

trait LowerPriority0MkWrite extends LowerPriority1MkWrite {

  // Derivation inductive case for product types
  implicit def product[H, T <: HList](
      implicit
      H: Write[H] OrElse Mkk[H],
      T: Write[T] OrElse Mkk[T]
  ): Mkk[H :: T] = {
    val head = H.fold(identity, _.instance)
    val tail = T.fold(identity, _.instance)

    new Mkk[H :: T](
      Write.Composite(
        List(head, tail),
        { case h :: t => List(h, t) }
      )
    )
  }

  // Derivation inductive case for shapeless records
  implicit def record[K <: Symbol, H, T <: HList](
      implicit
      H: Write[H] OrElse Mkk[H],
      T: Write[T] OrElse Mkk[T]
  ): Mkk[FieldType[K, H] :: T] = {
    val head = H.fold(identity, _.instance)
    val tail = T.fold(identity, _.instance)

    new Mkk(
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
      hlistWrite: Lazy[Write[Repr] OrElse Mkk[Repr]]
  ): Mkk[A] = {
    val g = hlistWrite.value.fold(identity, _.instance)

    new Mkk[A](
      Write.Composite(List(g), a => List(gen.to(a)))
    )
  }

}

object MkWritePlatform extends MkWritePlatform
