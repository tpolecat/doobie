// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import shapeless.{HList, HNil, ::, Generic, Lazy, <:!<, OrElse}
import shapeless.labelled.{field, FieldType}

trait ReadPlatform extends LowerPriorityRead {

  implicit def recordRead[K <: Symbol, H, T <: HList](
      implicit
      H: Lazy[Read[H] OrElse MkRead[H]],
      T: Lazy[MkRead[T]]
  ): MkRead[FieldType[K, H] :: T] = {
    val head = H.value.unify

    new MkRead[FieldType[K, H] :: T](
      head.gets ++ T.value.gets,
      (rs, n) => field[K](head.unsafeGet(rs, n)) :: T.value.unsafeGet(rs, n + head.length)
    )
  }

}

trait LowerPriorityRead extends EvenLower {

  implicit def product[H, T <: HList](
      implicit
      H: Lazy[Read[H] OrElse MkRead[H]],
      T: Lazy[MkRead[T]]
  ): MkRead[H :: T] = {
    val head = H.value.unify

    new MkRead[H :: T](
      head.gets ++ T.value.gets,
      (rs, n) => head.unsafeGet(rs, n) :: T.value.unsafeGet(rs, n + head.length)
    )
  }

  implicit val emptyProduct: MkRead[HNil] =
    new MkRead[HNil](Nil, (_, _) => HNil)

  implicit def generic[F, G](implicit gen: Generic.Aux[F, G], G: Lazy[MkRead[G]]): MkRead[F] =
    new MkRead[F](G.value.gets, (rs, n) => gen.from(G.value.unsafeGet(rs, n)))

}

trait EvenLower {

  implicit val ohnil: MkRead[Option[HNil]] =
    new MkRead[Option[HNil]](Nil, (_, _) => Some(HNil))

  implicit def ohcons1[H, T <: HList](
      implicit
      H: Lazy[Read[Option[H]] OrElse MkRead[Option[H]]],
      T: Lazy[MkRead[Option[T]]],
      N: H <:!< Option[α] forSome { type α }
  ): MkRead[Option[H :: T]] = {
    void(N)
    val head = H.value.unify

    new MkRead[Option[H :: T]](
      head.gets ++ T.value.gets,
      (rs, n) =>
        for {
          h <- head.unsafeGet(rs, n)
          t <- T.value.unsafeGet(rs, n + head.length)
        } yield h :: t
    )
  }

  implicit def ohcons2[H, T <: HList](
      implicit
      H: Lazy[Read[Option[H]] OrElse MkRead[Option[H]]],
      T: Lazy[MkRead[Option[T]]]
  ): MkRead[Option[Option[H] :: T]] = {
    val head = H.value.unify

    new MkRead[Option[Option[H] :: T]](
      head.gets ++ T.value.gets,
      (rs, n) => T.value.unsafeGet(rs, n + head.length).map(head.unsafeGet(rs, n) :: _)
    )
  }

  implicit def ogeneric[A, Repr <: HList](
      implicit
      G: Generic.Aux[A, Repr],
      B: Lazy[MkRead[Option[Repr]]]
  ): MkRead[Option[A]] =
    new MkRead[Option[A]](B.value.gets, B.value.unsafeGet(_, _).map(G.from))

}
