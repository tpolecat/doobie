// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import shapeless.{HList, HNil, ::, Generic, Lazy, <:!<, OrElse}
import shapeless.labelled.{FieldType}

trait WritePlatform extends LowerPriorityWrite {

  // Derivation base case for product types (1-element)
  implicit def productBase[H](
      implicit H: Write[H] OrElse MkWrite[H]
  ): MkWrite[H :: HNil] = {
    val head = H.unify

    new MkWrite[H :: HNil](
      head.puts,
      { case h :: HNil => head.toList(h) },
      { case (ps, n, h :: HNil) => head.unsafeSet(ps, n, h); },
      { case (rs, n, h :: HNil) => head.unsafeUpdate(rs, n, h); }
    )
  }

  // Derivation base case for shapelss record (1-element)
  implicit def recordBase[K <: Symbol, H](
      implicit H: Write[H] OrElse MkWrite[H]
  ): MkWrite[FieldType[K, H] :: HNil] = {
    val head = H.unify

    new MkWrite(
      head.puts,
      { case h :: HNil => head.toList(h) },
      { case (ps, n, h :: HNil) => head.unsafeSet(ps, n, h) },
      { case (rs, n, h :: HNil) => head.unsafeUpdate(rs, n, h) }
    )
  }

}

trait LowerPriorityWrite extends EvenLowerPriorityWrite {

  // Derivation inductive case for product types
  implicit def product[H, T <: HList](
      implicit
      H: Write[H] OrElse MkWrite[H],
      T: MkWrite[T]
  ): MkWrite[H :: T] = {
    val head = H.unify

    new MkWrite(
      head.puts ++ T.puts,
      { case h :: t => head.toList(h) ++ T.toList(t) },
      { case (ps, n, h :: t) => head.unsafeSet(ps, n, h); T.unsafeSet(ps, n + head.length, t) },
      { case (rs, n, h :: t) => head.unsafeUpdate(rs, n, h); T.unsafeUpdate(rs, n + head.length, t) }
    )
  }

  // Derivation for product types (i.e. case class)
  implicit def generic[B, A](implicit gen: Generic.Aux[B, A], A: Lazy[MkWrite[A]]): MkWrite[B] =
    new MkWrite[B](
      A.value.puts,
      b => A.value.toList(gen.to(b)),
      (ps, n, b) => A.value.unsafeSet(ps, n, gen.to(b)),
      (rs, n, b) => A.value.unsafeUpdate(rs, n, gen.to(b))
    )

  // Derivation inductive case for shapeless records
  implicit def record[K <: Symbol, H, T <: HList](
      implicit
      H: Write[H] OrElse MkWrite[H],
      T: MkWrite[T]
  ): MkWrite[FieldType[K, H] :: T] = {
    val head = H.unify

    new MkWrite(
      head.puts ++ T.puts,
      { case h :: t => head.toList(h) ++ T.toList(t) },
      { case (ps, n, h :: t) => head.unsafeSet(ps, n, h); T.unsafeSet(ps, n + head.length, t) },
      { case (rs, n, h :: t) => head.unsafeUpdate(rs, n, h); T.unsafeUpdate(rs, n + head.length, t) }
    )
  }

  // Derivation base case for Option of product types (1-element)
  implicit def optProductBase[H](
      implicit
      H: Write[Option[H]] OrElse MkWrite[Option[H]],
      N: H <:!< Option[α] forSome { type α }
  ): MkWrite[Option[H :: HNil]] = {
    void(N)
    val head = H.unify

    def withHead[A](opt: Option[H :: HNil])(f: Option[H] => A): A = {
      f(opt.map(_.head))
    }

    new MkWrite(
      head.puts,
      withHead(_)(head.toList(_)),
      (ps, n, i) => withHead(i)(h => head.unsafeSet(ps, n, h)),
      (rs, n, i) => withHead(i)(h => head.unsafeUpdate(rs, n, h))
    )

  }

  // Derivation base case for Option of product types (where the head element is Option)
  implicit def optProductOptBase[H](
      implicit H: Write[Option[H]] OrElse MkWrite[Option[H]]
  ): MkWrite[Option[Option[H] :: HNil]] = {
    val head = H.unify

    def withHead[A](opt: Option[Option[H] :: HNil])(f: Option[H] => A): A = {
      opt match {
        case Some(h :: _) => f(h)
        case None         => f(None)
      }
    }

    new MkWrite(
      head.puts,
      withHead(_) { h => head.toList(h) },
      (ps, n, i) => withHead(i) { h => head.unsafeSet(ps, n, h) },
      (rs, n, i) => withHead(i) { h => head.unsafeUpdate(rs, n, h) }
    )

  }

}

trait EvenLowerPriorityWrite {

  // Write[Option[H]], Write[Option[T]] implies Write[Option[H *: T]]
  implicit def optPorduct[H, T <: HList](
      implicit
      H: Write[Option[H]] OrElse MkWrite[Option[H]],
      T: MkWrite[Option[T]],
      N: H <:!< Option[α] forSome { type α }
  ): MkWrite[Option[H :: T]] = {
    void(N)
    val head = H.unify

    def split[A](i: Option[H :: T])(f: (Option[H], Option[T]) => A): A =
      i.fold(f(None, None)) { case h :: t => f(Some(h), Some(t)) }

    new MkWrite(
      head.puts ++ T.puts,
      split(_) { (h, t) => head.toList(h) ++ T.toList(t) },
      (ps, n, i) => split(i) { (h, t) => head.unsafeSet(ps, n, h); T.unsafeSet(ps, n + head.length, t) },
      (rs, n, i) => split(i) { (h, t) => head.unsafeUpdate(rs, n, h); T.unsafeUpdate(rs, n + head.length, t) }
    )

  }

  // Write[Option[H]], Write[Option[T]] implies Write[Option[Option[H] *: T]]
  implicit def optProductOpt[H, T <: HList](
      implicit
      H: Write[Option[H]] OrElse MkWrite[Option[H]],
      T: MkWrite[Option[T]]
  ): MkWrite[Option[Option[H] :: T]] = {
    val head = H.unify

    def split[A](i: Option[Option[H] :: T])(f: (Option[H], Option[T]) => A): A =
      i.fold(f(None, None)) { case oh :: t => f(oh, Some(t)) }

    new MkWrite(
      head.puts ++ T.puts,
      split(_) { (h, t) => head.toList(h) ++ T.toList(t) },
      (ps, n, i) => split(i) { (h, t) => head.unsafeSet(ps, n, h); T.unsafeSet(ps, n + head.length, t) },
      (rs, n, i) => split(i) { (h, t) => head.unsafeUpdate(rs, n, h); T.unsafeUpdate(rs, n + head.length, t) }
    )

  }

  // Derivation for optional of product types (i.e. case class)
  implicit def ogeneric[B, A <: HList](
      implicit
      G: Generic.Aux[B, A],
      A: Lazy[MkWrite[Option[A]]]
  ): MkWrite[Option[B]] =
    new MkWrite(
      A.value.puts,
      b => A.value.toList(b.map(G.to)),
      (rs, n, a) => A.value.unsafeSet(rs, n, a.map(G.to)),
      (rs, n, a) => A.value.unsafeUpdate(rs, n, a.map(G.to))
    )

}
