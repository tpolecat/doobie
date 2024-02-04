// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import shapeless.{HList, HNil, ::, Generic, Lazy, <:!<, OrElse}
import shapeless.labelled.{FieldType}

trait WritePlatform extends LowerPriorityWrite {

  implicit def recordWrite[K <: Symbol, H, T <: HList](
      implicit
      H: Lazy[Write[H] OrElse MkWrite[H]],
      T: Lazy[MkWrite[T]]
  ): MkWrite[FieldType[K, H] :: T] = {
    val head = H.value.unify

    new MkWrite(
      head.puts ++ T.value.puts,
      { case h :: t => head.toList(h) ++ T.value.toList(t) },
      { case (ps, n, h :: t) => head.unsafeSet(ps, n, h); T.value.unsafeSet(ps, n + head.length, t) },
      { case (rs, n, h :: t) => head.unsafeUpdate(rs, n, h); T.value.unsafeUpdate(rs, n + head.length, t) }
    )
  }

}

trait LowerPriorityWrite extends EvenLowerPriorityWrite {

  implicit def product[H, T <: HList](
      implicit
      H: Lazy[Write[H] OrElse MkWrite[H]],
      T: Lazy[MkWrite[T]]
  ): MkWrite[H :: T] = {
    val head = H.value.unify

    new MkWrite(
      head.puts ++ T.value.puts,
      { case h :: t => head.toList(h) ++ T.value.toList(t) },
      { case (ps, n, h :: t) => head.unsafeSet(ps, n, h); T.value.unsafeSet(ps, n + head.length, t) },
      { case (rs, n, h :: t) => head.unsafeUpdate(rs, n, h); T.value.unsafeUpdate(rs, n + head.length, t) }
    )
  }

  implicit val emptyProduct: MkWrite[HNil] =
    new MkWrite[HNil](Nil, _ => Nil, (_, _, _) => (), (_, _, _) => ())

  implicit def generic[B, A](implicit gen: Generic.Aux[B, A], A: Lazy[MkWrite[A]]): MkWrite[B] =
    new MkWrite[B](
      A.value.puts,
      b => A.value.toList(gen.to(b)),
      (ps, n, b) => A.value.unsafeSet(ps, n, gen.to(b)),
      (rs, n, b) => A.value.unsafeUpdate(rs, n, gen.to(b))
    )

}

trait EvenLowerPriorityWrite {

  implicit val ohnil: MkWrite[Option[HNil]] =
    new MkWrite[Option[HNil]](Nil, _ => Nil, (_, _, _) => (), (_, _, _) => ())

  implicit def ohcons1[H, T <: HList](
      implicit
      H: Lazy[Write[Option[H]] OrElse MkWrite[Option[H]]],
      T: Lazy[MkWrite[Option[T]]],
      N: H <:!< Option[α] forSome { type α }
  ): MkWrite[Option[H :: T]] = {
    void(N)
    val head = H.value.unify

    def split[A](i: Option[H :: T])(f: (Option[H], Option[T]) => A): A =
      i.fold(f(None, None)) { case h :: t => f(Some(h), Some(t)) }

    new MkWrite(
      head.puts ++ T.value.puts,
      split(_) { (h, t) => head.toList(h) ++ T.value.toList(t) },
      (ps, n, i) => split(i) { (h, t) => head.unsafeSet(ps, n, h); T.value.unsafeSet(ps, n + head.length, t) },
      (rs, n, i) => split(i) { (h, t) => head.unsafeUpdate(rs, n, h); T.value.unsafeUpdate(rs, n + head.length, t) }
    )

  }

  implicit def ohcons2[H, T <: HList](
      implicit
      H: Lazy[Write[Option[H]] OrElse MkWrite[Option[H]]],
      T: Lazy[MkWrite[Option[T]]]
  ): MkWrite[Option[Option[H] :: T]] = {
    val head = H.value.unify

    def split[A](i: Option[Option[H] :: T])(f: (Option[H], Option[T]) => A): A =
      i.fold(f(None, None)) { case oh :: t => f(oh, Some(t)) }

    new MkWrite(
      head.puts ++ T.value.puts,
      split(_) { (h, t) => head.toList(h) ++ T.value.toList(t) },
      (ps, n, i) => split(i) { (h, t) => head.unsafeSet(ps, n, h); T.value.unsafeSet(ps, n + head.length, t) },
      (rs, n, i) => split(i) { (h, t) => head.unsafeUpdate(rs, n, h); T.value.unsafeUpdate(rs, n + head.length, t) }
    )

  }

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
