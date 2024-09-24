// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import shapeless.{HList, HNil, ::, Generic, Lazy, <:!<, OrElse}
import shapeless.labelled.FieldType
import java.sql.{PreparedStatement, ResultSet}
import MkWritePlatform.{ToListFunc, UnsafeSetFunc, UnsafeUpdateFunc}

trait MkWritePlatform extends LowerPriorityMkWrite {

  // Derivation base case for shapelss record (1-element)
  implicit def recordBase[K <: Symbol, H](
      implicit H: Write[H] OrElse Derived[MkWrite[H]]
  ): Derived[MkWrite[FieldType[K, H] :: HNil]] = {
    val head = H.fold(identity, _.instance)

    Derived(new MkWrite(
      head.puts,
      { case h :: HNil => head.toList(h) },
      { case (ps, n, h :: HNil) => head.unsafeSet(ps, n, h) },
      { case (rs, n, h :: HNil) => head.unsafeUpdate(rs, n, h) }
    ))
  }

  // Derivation base case for product types (1-element)
  implicit def productBase[H](
      implicit H: Write[H] OrElse Derived[MkWrite[H]]
  ): Write[H :: HNil] = {
    val head = H.fold(identity, _.instance)

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
      H: Write[H] OrElse Derived[MkWrite[H]],
      T: Write[T] OrElse Derived[MkWrite[T]]
  ): Write[H :: T] = {
    val head = H.fold(identity, _.instance)
    val tail = T.fold(identity, _.instance)

    Write[H :: T](
      head.puts ++ tail.puts,
      { case h :: t => head.toList(h) ++ tail.toList(t) }: ToListFunc[H :: T],
      { case (ps, n, h :: t) => head.unsafeSet(ps, n, h); tail.unsafeSet(ps, n + head.length, t) }: UnsafeSetFunc[H :: T],
      { case (rs, n, h :: t) => head.unsafeUpdate(rs, n, h); tail.unsafeUpdate(rs, n + head.length, t) }: UnsafeUpdateFunc[H :: T]
    )
  }

  // Derivation base case for Option of product types (1-element)
  implicit def optProductBase[H](
      implicit
      H: Write[Option[H]] OrElse Derived[MkWrite[Option[H]]],
      N: H <:!< Option[α] forSome { type α }
  ): Write[Option[H :: HNil]] = {
    void(N)
    val head = H.fold(identity, _.instance)

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
      implicit H: Write[Option[H]] OrElse Derived[MkWrite[Option[H]]]
  ): Write[Option[Option[H] :: HNil]] = {
    val head = H.fold(identity, _.instance)

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
  implicit def generic[A, Repr <: HList](
      implicit
      gen: Generic.Aux[A, Repr],
      hlistWrite: Lazy[Write[Repr] OrElse Derived[MkWrite[Repr]]]
  ): Derived[MkWrite[A]] = {
    val g = hlistWrite.value.fold(identity, _.instance)

    Derived(new MkWrite[A](
      g.puts,
      b => g.toList(gen.to(b)),
      (ps, n, b) => g.unsafeSet(ps, n, gen.to(b)),
      (rs, n, b) => g.unsafeUpdate(rs, n, gen.to(b))
    ))
  }

  // Derivation inductive case for shapeless records
  implicit def record[K <: Symbol, H, T <: HList](
      implicit
      H: Write[H] OrElse Derived[MkWrite[H]],
      T: Write[T] OrElse Derived[MkWrite[T]]
  ): Derived[MkWrite[FieldType[K, H] :: T]] = {
    val head = H.fold(identity, _.instance)
    val tail = T.fold(identity, _.instance)

    Derived(new MkWrite(
      head.puts ++ tail.puts,
      { case h :: t => head.toList(h) ++ tail.toList(t) },
      { case (ps, n, h :: t) => head.unsafeSet(ps, n, h); tail.unsafeSet(ps, n + head.length, t) },
      { case (rs, n, h :: t) => head.unsafeUpdate(rs, n, h); tail.unsafeUpdate(rs, n + head.length, t) }
    ))
  }

}

trait EvenLowerPriorityMkWrite {

  // Write[Option[H]], Write[Option[T]] implies Write[Option[H *: T]]
  implicit def optProduct[H, T <: HList](
      implicit
      H: Write[Option[H]] OrElse Derived[MkWrite[Option[H]]],
      T: Write[Option[T]] OrElse Derived[MkWrite[Option[T]]],
      N: H <:!< Option[α] forSome { type α }
  ): Write[Option[H :: T]] = {
    void(N)
    val head = H.fold(identity, _.instance)
    val tail = T.fold(identity, _.instance)

    def split[A](i: Option[H :: T])(f: (Option[H], Option[T]) => A): A =
      i.fold(f(None, None)) { case h :: t => f(Some(h), Some(t)) }

    Write(
      head.puts ++ tail.puts,
      split(_) { (h, t) => head.toList(h) ++ tail.toList(t) },
      (ps, n, i) => split(i) { (h, t) => head.unsafeSet(ps, n, h); tail.unsafeSet(ps, n + head.length, t) },
      (rs, n, i) => split(i) { (h, t) => head.unsafeUpdate(rs, n, h); tail.unsafeUpdate(rs, n + head.length, t) }
    )

  }

  // Write[Option[H]], Write[Option[T]] implies Write[Option[Option[H] *: T]]
  implicit def optProductOpt[H, T <: HList](
      implicit
      H: Write[Option[H]] OrElse Derived[MkWrite[Option[H]]],
      T: Write[Option[T]] OrElse Derived[MkWrite[Option[T]]]
  ): Write[Option[Option[H] :: T]] = {
    val head = H.fold(identity, _.instance)
    val tail = T.fold(identity, _.instance)

    def split[A](i: Option[Option[H] :: T])(f: (Option[H], Option[T]) => A): A =
      i.fold(f(None, None)) { case oh :: t => f(oh, Some(t)) }

    Write(
      head.puts ++ tail.puts,
      split(_) { (h, t) => head.toList(h) ++ tail.toList(t) },
      (ps, n, i) => split(i) { (h, t) => head.unsafeSet(ps, n, h); tail.unsafeSet(ps, n + head.length, t) },
      (rs, n, i) => split(i) { (h, t) => head.unsafeUpdate(rs, n, h); tail.unsafeUpdate(rs, n + head.length, t) }
    )

  }

  // Derivation for optional of product types (i.e. case class)
  implicit def ogeneric[B, A <: HList](
      implicit
      G: Generic.Aux[B, A],
      A: Lazy[Write[Option[A]] OrElse Derived[MkWrite[Option[A]]]]
  ): Derived[MkWrite[Option[B]]] = {
    val g = A.value.fold(identity, _.instance)
    Derived(new MkWrite(
      g.puts,
      b => g.toList(b.map(G.to)),
      (ps, n, b) => g.unsafeSet(ps, n, b.map(G.to)),
      (rs, n, b) => g.unsafeUpdate(rs, n, b.map(G.to))
    ))
  }

}

object MkWritePlatform {
  type ToListFunc[A] = A => List[Any]
  type UnsafeSetFunc[A] = (PreparedStatement, Int, A) => Unit
  type UnsafeUpdateFunc[A] = (ResultSet, Int, A) => Unit
}