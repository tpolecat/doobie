// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import shapeless.*

import java.sql.{PreparedStatement, ResultSet}
import WritePlatform.*

trait WritePlatform extends LowerPriorityWritePlatform {

  implicit def genericTuple[A, Repr](
      implicit
      gen: Generic.Aux[A, Repr],
      A: Lazy[Write[Repr]],
      isTuple: IsTuple[A]
  ): Write[A] = {
    val _ = isTuple
    MkWrite.generic[A, Repr].instance
  }

  implicit def ogenericTuple[A, Repr <: HList](
      implicit
      G: Generic.Aux[A, Repr],
      A: Lazy[Write[Option[Repr]]],
      isTuple: IsTuple[A]
  ): Write[Option[A]] = {
    val _ = isTuple
    MkWrite.ogeneric[A, Repr].instance
  }

  @deprecated("Use Write.derived instead to derive instances explicitly", "1.0.0-RC6")
  def generic[T, Repr](implicit gen: Generic.Aux[T, Repr], A: Lazy[Write[Repr]]): Write[T] =
    MkWrite.generic[T, Repr].instance
}

trait LowerPriorityWritePlatform extends EvenLowerPriorityWritePlatform {

}

trait EvenLowerPriorityWritePlatform {
}

object WritePlatform {
  type ToListFunc[A] = A => List[Any]
  type UnsafeSetFunc[A] = (PreparedStatement, Int, A) => Unit
  type UnsafeUpdateFunc[A] = (ResultSet, Int, A) => Unit
}
