// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import shapeless.{HList, HNil, ::, Generic, Lazy, <:!<}
import shapeless.labelled.FieldType

trait MkWritePlatform extends LowerPriorityMkWrite {

  // FIXME: move
  // Derivation base case for shapelss record (1-element)
  implicit def recordBase[K <: Symbol, H](
      implicit H: Write[H]
  ): Derived[MkWrite[FieldType[K, H] :: HNil]] = {
    val head = H

    Derived(new MkWrite(
      head.puts,
      { case h :: HNil => head.toList(h) },
      { case (ps, n, h :: HNil) => head.unsafeSet(ps, n, h) },
      { case (rs, n, h :: HNil) => head.unsafeUpdate(rs, n, h) }
    ))
  }

  // Derivation base case for product types (1-element)
  implicit def productBase[H](
      implicit H: Write[H]
  ): Write[H :: HNil] = {
    val head = H

    Write[H :: HNil](
      head.puts,
      { case h :: HNil => head.toList(h) }: ToListFunc[H :: HNil],
      { case (ps, n, h :: HNil) => head.unsafeSet(ps, n, h); }: UnsafeSetFunc[H :: HNil],
      { case (rs, n, h :: HNil) => head.unsafeUpdate(rs, n, h); }: UnsafeUpdateFunc[H :: HNil]
    )
  }

}

trait LowerPriorityMkWrite extends EvenLowerPriorityMkWrite {

  // Derivation inductive case for product types
  implicit def product[H, T <: HList](
      implicit
      H: Write[H],
      T: Write[T]
  ): Write[H :: T] = {
    val head = H

    Write(
      head.puts ++ T.puts,
      { case h :: t => head.toList(h) ++ T.toList(t) }: ToListFunc[H :: T],
      { case (ps, n, h :: t) => head.unsafeSet(ps, n, h); T.unsafeSet(ps, n + head.length, t) }: UnsafeSetFunc[H :: T],
      { case (rs, n, h :: t) => head.unsafeUpdate(rs, n, h); T.unsafeUpdate(rs, n + head.length, t) }: UnsafeUpdateFunc[
        H :: T]
    )
  }

  // Derivation base case for Option of product types (1-element)
  implicit def optProductBase[H](
      implicit
      H: Write[Option[H]],
      N: H <:!< Option[α] forSome { type α }
  ): Write[Option[H :: HNil]] = {
    void(N)
    val head = H

    def withHead[A](opt: Option[H :: HNil])(f: Option[H] => A): A = {
      f(opt.map(_.head))
    }

    Write[Option[H :: HNil]](
      head.puts,
      { withHead(_)(head.toList(_)) }: ToListFunc[Option[H :: HNil]],
      { (ps, n, i) => withHead(i)(h => head.unsafeSet(ps, n, h)) }: UnsafeSetFunc[Option[H :: HNil]],
      { (rs, n, i) => withHead(i)(h => head.unsafeUpdate(rs, n, h)) }: UnsafeUpdateFunc[Option[H :: HNil]]
    )

  }

  // Derivation base case for Option of product types (where the head element is Option)
  implicit def optProductOptBase[H](
      implicit H: Write[Option[H]]
  ): Write[Option[Option[H] :: HNil]] = {
    val head = H

    def withHead[A](opt: Option[Option[H] :: HNil])(f: Option[H] => A): A = {
      opt match {
        case Some(h :: _) => f(h)
        case None         => f(None)
      }
    }

    Write(
      head.puts,
      withHead(_) { h => head.toList(h) },
      (ps, n, i) => withHead(i) { h => head.unsafeSet(ps, n, h) },
      (rs, n, i) => withHead(i) { h => head.unsafeUpdate(rs, n, h) }
    )

  }

  // Derivation for product types (i.e. case class)
  implicit def generic[B, A](
      implicit
      gen: Generic.Aux[B, A],
      A: Lazy[Write[A]]
  ): Derived[MkWrite[B]] =
    Derived(new MkWrite[B](
      A.value.puts,
      b => A.value.toList(gen.to(b)),
      (ps, n, b) => A.value.unsafeSet(ps, n, gen.to(b)),
      (rs, n, b) => A.value.unsafeUpdate(rs, n, gen.to(b))
    ))

  // Derivation inductive case for shapeless records
  implicit def record[K <: Symbol, H, T <: HList](
      implicit
      H: Write[H],
      T: Write[T]
  ): Derived[MkWrite[FieldType[K, H] :: T]] = {
    val head = H

    Derived(new MkWrite(
      head.puts ++ T.puts,
      { case h :: t => head.toList(h) ++ T.toList(t) },
      { case (ps, n, h :: t) => head.unsafeSet(ps, n, h); T.unsafeSet(ps, n + head.length, t) },
      { case (rs, n, h :: t) => head.unsafeUpdate(rs, n, h); T.unsafeUpdate(rs, n + head.length, t) }
    ))
  }

}

trait EvenLowerPriorityMkWrite {

  // Write[Option[H]], Write[Option[T]] implies Write[Option[H *: T]]
  implicit def optProduct[H, T <: HList](
      implicit
      H: Write[Option[H]],
      T: Write[Option[T]],
      N: H <:!< Option[α] forSome { type α }
  ): Write[Option[H :: T]] = {
    void(N)
    val head = H

    def split[A](i: Option[H :: T])(f: (Option[H], Option[T]) => A): A =
      i.fold(f(None, None)) { case h :: t => f(Some(h), Some(t)) }

    Write(
      head.puts ++ T.puts,
      split(_) { (h, t) => head.toList(h) ++ T.toList(t) },
      (ps, n, i) => split(i) { (h, t) => head.unsafeSet(ps, n, h); T.unsafeSet(ps, n + head.length, t) },
      (rs, n, i) => split(i) { (h, t) => head.unsafeUpdate(rs, n, h); T.unsafeUpdate(rs, n + head.length, t) }
    )

  }

  // Write[Option[H]], Write[Option[T]] implies Write[Option[Option[H] *: T]]
  implicit def optProductOpt[H, T <: HList](
      implicit
      H: Write[Option[H]],
      T: Write[Option[T]]
  ): Write[Option[Option[H] :: T]] = {
    val head = H

    def split[A](i: Option[Option[H] :: T])(f: (Option[H], Option[T]) => A): A =
      i.fold(f(None, None)) { case oh :: t => f(oh, Some(t)) }

    Write(
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
      A: Lazy[Write[Option[A]]]
  ): Derived[MkWrite[Option[B]]] =
    Derived(new MkWrite(
      A.value.puts,
      b => A.value.toList(b.map(G.to)),
      (rs, n, a) => A.value.unsafeSet(rs, n, a.map(G.to)),
      (rs, n, a) => A.value.unsafeUpdate(rs, n, a.map(G.to))
    ))

}
