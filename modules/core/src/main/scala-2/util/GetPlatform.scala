// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import shapeless._
import shapeless.ops.hlist.IsHCons

trait GetPlatform {
  import doobie.util.compat.=:=

  /** @group Instances */
  implicit def unaryProductGet[A, L <: HList, H, T <: HList](
      implicit
      G: Generic.Aux[A, L],
      C: IsHCons.Aux[L, H, T],
      H: Lazy[Get[H]],
      E: (H :: HNil) =:= L
  ): MkGet[A] = {
    void(C) // C drives inference but is not used directly
    val get = H.value.tmap[A](h => G.from(h :: HNil))
    MkGet.lift(get)
  }

}
