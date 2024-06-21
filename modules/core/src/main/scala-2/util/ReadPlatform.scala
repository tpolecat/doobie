// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import shapeless.{HList, HNil, ::, Generic, Lazy, <:!<, OrElse}
import shapeless.labelled.{field, FieldType}

trait ReadPlatform extends LowerPriorityRead {

  // Derivation base case for product types (1-element)
  implicit def productBase[H](
      implicit H: Read[H] OrElse MkRead[H]
  ): MkRead[H :: HNil] = {
    val head = H.unify

    new MkRead[H :: HNil](
      head.gets,
      (rs, n) => head.unsafeGet(rs, n) :: HNil
    )
  }

  // Derivation base case for shapeless record (1-element)
  implicit def recordBase[K <: Symbol, H](
      implicit H: Read[H] OrElse MkRead[H]
  ): MkRead[FieldType[K, H] :: HNil] = {
    val head = H.unify

    new MkRead[FieldType[K, H] :: HNil](
      head.gets,
      (rs, n) => field[K](head.unsafeGet(rs, n)) :: HNil
    )
  }
}

trait LowerPriorityRead extends EvenLowerPriorityRead {

  // Derivation inductive case for product types
  implicit def product[H, T <: HList](
      implicit
      H: Read[H] OrElse MkRead[H],
      T: MkRead[T]
  ): MkRead[H :: T] = {
    val head = H.unify

    new MkRead[H :: T](
      head.gets ++ T.gets,
      (rs, n) => head.unsafeGet(rs, n) :: T.unsafeGet(rs, n + head.length)
    )
  }

  // Derivation inductive case for shapeless records
  implicit def record[K <: Symbol, H, T <: HList](
      implicit
      H: Read[H] OrElse MkRead[H],
      T: MkRead[T]
  ): MkRead[FieldType[K, H] :: T] = {
    val head = H.unify

    new MkRead[FieldType[K, H] :: T](
      head.gets ++ T.gets,
      (rs, n) => field[K](head.unsafeGet(rs, n)) :: T.unsafeGet(rs, n + head.length)
    )
  }

  // Derivation for product types (i.e. case class)
  implicit def generic[F, G](implicit gen: Generic.Aux[F, G], G: Lazy[MkRead[G]]): MkRead[F] =
    new MkRead[F](G.value.gets, (rs, n) => gen.from(G.value.unsafeGet(rs, n)))

  // Derivation base case for Option of product types (1-element)
  implicit def optProductBase[H](
      implicit
      H: Read[Option[H]] OrElse MkRead[Option[H]],
      N: H <:!< Option[α] forSome { type α }
  ): MkRead[Option[H :: HNil]] = {
    void(N)
    val head = H.unify

    new MkRead[Option[H :: HNil]](
      head.gets,
      (rs, n) =>
        head.unsafeGet(rs, n).map(_ :: HNil)
    )
  }

  // Derivation base case for Option of product types (where the head element is Option)
  implicit def optProductOptBase[H](
      implicit H: Read[Option[H]] OrElse MkRead[Option[H]]
  ): MkRead[Option[Option[H] :: HNil]] = {
    val head = H.unify

    new MkRead[Option[Option[H] :: HNil]](
      head.gets,
      (rs, n) => head.unsafeGet(rs, n).map(h => Some(h) :: HNil)
    )
  }

}

trait EvenLowerPriorityRead {

  // Read[Option[H]], Read[Option[T]] implies Read[Option[H *: T]]
  implicit def optProduct[H, T <: HList](
      implicit
      H: Read[Option[H]] OrElse MkRead[Option[H]],
      T: MkRead[Option[T]],
      N: H <:!< Option[α] forSome { type α }
  ): MkRead[Option[H :: T]] = {
    void(N)
    val head = H.unify

    new MkRead[Option[H :: T]](
      head.gets ++ T.gets,
      (rs, n) =>
        for {
          h <- head.unsafeGet(rs, n)
          t <- T.unsafeGet(rs, n + head.length)
        } yield h :: t
    )
  }

  // Read[Option[H]], Read[Option[T]] implies Read[Option[Option[H] *: T]]
  implicit def optProductOpt[H, T <: HList](
      implicit
      H: Read[Option[H]] OrElse MkRead[Option[H]],
      T: MkRead[Option[T]]
  ): MkRead[Option[Option[H] :: T]] = {
    val head = H.unify

    new MkRead[Option[Option[H] :: T]](
      head.gets ++ T.gets,
      (rs, n) => T.unsafeGet(rs, n + head.length).map(head.unsafeGet(rs, n) :: _)
    )
  }

  // Derivation for optional of product types (i.e. case class)
  implicit def ogeneric[A, Repr <: HList](
      implicit
      G: Generic.Aux[A, Repr],
      B: Lazy[MkRead[Option[Repr]]]
  ): MkRead[Option[A]] =
    new MkRead[Option[A]](B.value.gets, B.value.unsafeGet(_, _).map(G.from))

}
