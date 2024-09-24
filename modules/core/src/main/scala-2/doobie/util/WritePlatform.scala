// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import shapeless.*


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
}

trait LowerPriorityWritePlatform extends EvenLowerPriorityWritePlatform {

}

trait EvenLowerPriorityWritePlatform {
}

