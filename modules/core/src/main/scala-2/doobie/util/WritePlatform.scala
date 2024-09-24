// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import shapeless.*
import shapeless.labelled.FieldType

trait WritePlatform extends LowerPriorityWritePlatform {

  implicit def genericTuple[A, Repr <: HList](
      implicit
      gen: Generic.Aux[A, Repr],
      A: Lazy[Write[Repr]],
      isTuple: IsTuple[A]
  ): Write[A] = {
    val _ = isTuple
    implicit val hlistWrite: Lazy[Write[Repr] OrElse Derived[MkWrite[Repr]]] = OrElse.primary(A.value)
    MkWrite.generic[A, Repr].instance
  }

  implicit def ogenericTuple[A, Repr <: HList](
      implicit
      G: Generic.Aux[A, Repr],
      A: Lazy[Write[Option[Repr]]],
      isTuple: IsTuple[A]
  ): Write[Option[A]] = {
    val _ = isTuple
    implicit val hlistWrite: Lazy[Write[Option[Repr]] OrElse Derived[MkWrite[Option[Repr]]]] = OrElse.primary(A.value)
    MkWrite.ogeneric[A, Repr].instance
  }

  @deprecated("Use Write.derived instead to derive instances explicitly", "1.0.0-RC6")
  def generic[T, Repr <: HList](implicit gen: Generic.Aux[T, Repr], A: Lazy[Write[Repr]]): Write[T] = {
    implicit val hlistWrite: Lazy[Write[Repr] OrElse Derived[MkWrite[Repr]]] = OrElse.primary(A.value)
    MkWrite.generic[T, Repr].instance
  }

  // Forwarded to MkWritePlatform
  implicit def recordBase[K <: Symbol, H](
      implicit H: Write[H]
  ): Derived[MkWrite[FieldType[K, H] :: HNil]] = MkWritePlatform.recordBase

  implicit def productBase[H](
      implicit H: Write[H]
  ): Write[H :: HNil] = MkWritePlatform.productBase

}

trait LowerPriorityWritePlatform extends EvenLowerPriorityWritePlatform {

  // Forwarded to MkWritePlatform
  implicit def product[H, T <: HList](
      implicit
      H: Write[H],
      T: Write[T]
  ): Write[H :: T] = MkWritePlatform.product

  implicit def optProductBase[H](
      implicit
      H: Write[Option[H]],
      N: H <:!< Option[α] forSome { type α }
  ): Write[Option[H :: HNil]] = MkWritePlatform.optProductBase

  implicit def optProductOptBase[H](
      implicit H: Write[Option[H]]
  ): Write[Option[Option[H] :: HNil]] = MkWritePlatform.optProductOptBase

  implicit def record[K <: Symbol, H, T <: HList](
      implicit
      H: Write[H],
      T: Write[T]
  ): Derived[MkWrite[FieldType[K, H] :: T]] = MkWritePlatform.record

}

trait EvenLowerPriorityWritePlatform {

  // Forwarded to MkWritePlatform
  implicit def optProduct[H, T <: HList](
      implicit
      H: Write[Option[H]],
      T: Write[Option[T]],
      N: H <:!< Option[α] forSome { type α }
  ): Write[Option[H :: T]] = MkWritePlatform.optProduct

  implicit def optProductOpt[H, T <: HList](
      implicit
      H: Write[Option[H]],
      T: Write[Option[T]]
  ): Write[Option[Option[H] :: T]] = MkWritePlatform.optProductOpt

}

