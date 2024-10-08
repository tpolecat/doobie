// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import shapeless.*
import shapeless.labelled.FieldType

trait WritePlatform extends LowerPriority1WritePlatform {

  implicit def genericTuple[A, Repr <: HList](
      implicit
      gen: Generic.Aux[A, Repr],
      A: Lazy[Write[Repr]],
      isTuple: IsTuple[A]
  ): Write[A] = {
    val _ = isTuple
    implicit val hlistWrite: Lazy[Write[Repr] OrElse Mkk[Repr]] = OrElse.primary(A.value)
    MkWrite.generic[A, Repr].instance
  }

  implicit def ogenericTuple[A, Repr <: HList](
      implicit
      G: Generic.Aux[A, Repr],
      A: Lazy[Write[Option[Repr]]],
      isTuple: IsTuple[A]
  ): Write[Option[A]] = {
    val _ = isTuple
    implicit val hlistWrite: Lazy[Write[Option[Repr]] OrElse Mkk[Option[Repr]]] = OrElse.primary(A.value)
    MkWrite.ogeneric[A, Repr].instance
  }

  @deprecated("Use Write.derived instead to derive instances explicitly", "1.0.0-RC6")
  def generic[T, Repr <: HList](implicit
      gen: Generic.Aux[T, Repr],
      A: Write[Repr] OrElse Mkk[Repr]
  ): Write[T] = {
    implicit val hlistWrite: Lazy[Write[Repr] OrElse Mkk[Repr]] = A
    MkWrite.generic[T, Repr].instance
  }

  implicit def recordBase[K <: Symbol, H](
      implicit H: Write[H]
  ): Write[FieldType[K, H] :: HNil] = MkWritePlatform.recordBase[K, H].instance

  implicit def productBase[H](
      implicit H: Write[H]
  ): Write[H :: HNil] = MkWritePlatform.productBase[H].instance

}

trait LowerPriority1WritePlatform extends LowerPriority2WritePlatform {

  implicit def product[H, T <: HList](
      implicit
      H: Write[H],
      T: Write[T]
  ): Write[H :: T] = MkWritePlatform.product[H, T].instance

  implicit def optProductBase[H](
      implicit
      H: Write[Option[H]],
      N: H <:!< Option[α] forSome { type α }
  ): Write[Option[H :: HNil]] = MkWritePlatform.optProductBase[H].instance

  implicit def optProductOptBase[H](
      implicit H: Write[Option[H]]
  ): Write[Option[Option[H] :: HNil]] = MkWritePlatform.optProductOptBase[H].instance

  implicit def record[K <: Symbol, H, T <: HList](
      implicit
      H: Write[H],
      T: Write[T]
  ): Write[FieldType[K, H] :: T] = MkWritePlatform.record[K, H, T].instance

}

trait LowerPriority2WritePlatform extends LowerPriority3WritePlatform {

  implicit def optProduct[H, T <: HList](
      implicit
      H: Write[Option[H]],
      T: Write[Option[T]],
      N: H <:!< Option[α] forSome { type α }
  ): Write[Option[H :: T]] = MkWritePlatform.optProduct[H, T].instance

  implicit def optProductOpt[H, T <: HList](
      implicit
      H: Write[Option[H]],
      T: Write[Option[T]]
  ): Write[Option[Option[H] :: T]] = MkWritePlatform.optProductOpt[H, T].instance

}

trait LowerPriority3WritePlatform {
  implicit def fromDerived[A](implicit ev: Derived[Write[A]]): Write[A] = ev.instance
}
