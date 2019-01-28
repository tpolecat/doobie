// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats._
import doobie.free.{ FRS, ResultSetIO }
import doobie.enum.Nullability._
import java.sql.ResultSet
import scala.annotation.implicitNotFound
import shapeless.{ HList, HNil, ::, Generic, Lazy, <:!< }
import shapeless.labelled.{ field, FieldType }

@implicitNotFound("""
Cannot find or construct a Read instance for type:

  ${A}

This can happen for a few reasons, but the most common case is that a data
member somewhere within this type doesn't have a Get instance in scope. Here are
some debugging hints:

- For Option types, ensure that a Read instance is in scope for the non-Option
  version.
- For types you expect to map to a single column ensure that a Get instance is
  in scope.
- For case classes, HLists, and shapeless records ensure that each element
  has a Read instance in scope.
- Lather, rinse, repeat, recusively until you find the problematic bit.

You can check that an instance exists for Read in the REPL or in your code:

  scala> Read[Foo]

and similarly with Get:

  scala> Get[Foo]

And find the missing instance and construct it as needed. Refer to Chapter 12
of the book of doobie for more information.
""")
final class Read[A](
  val gets: List[(Get[_], NullabilityKnown)],
  val unsafeGet: (ResultSet, Int) => A
) {

  final lazy val length: Int = gets.length

  def map[B](f: A => B): Read[B] =
      new Read(gets, (rs, n) => f(unsafeGet(rs, n)))

  def ap[B](ff: Read[A => B]): Read[B] =
    new Read(gets ++ ff.gets, (rs, n) => ff.unsafeGet(rs, n + length)(unsafeGet(rs, n)))

  def get(n: Int): ResultSetIO[A] =
    FRS.raw(e => unsafeGet(e.jdbc, n))

}

object Read extends LowerPriorityRead {

  def apply[A](implicit ev: Read[A]): ev.type = ev

  implicit val ReadApply: Apply[Read] =
    new Apply[Read] {
      def ap[A, B](ff: Read[A => B])(fa: Read[A]): Read[B] = fa.ap(ff)
      def map[A, B](fa: Read[A])(f: A => B): Read[B] = fa.map(f)
    }

  implicit val unit: Read[Unit] =
    new Read(Nil, (_, _) => ())

  implicit def fromGet[A](implicit ev: Get[A]): Read[A] =
    new Read(List((ev, NoNulls)), ev.unsafeGetNonNullable)

  implicit def fromGetOption[A](implicit ev: Get[A]): Read[Option[A]] =
    new Read(List((ev, Nullable)), ev.unsafeGetNullable)

  implicit def recordRead[K <: Symbol, H, T <: HList](
    implicit H: Lazy[Read[H]],
              T: Lazy[Read[T]]
  ): Read[FieldType[K, H] :: T] =
    new Read[FieldType[K, H] :: T](
      H.value.gets ++ T.value.gets,
      (rs, n) => field[K](H.value.unsafeGet(rs, n)) :: T.value.unsafeGet(rs, n + H.value.length)
    )

}

trait LowerPriorityRead extends EvenLower { this: Read.type =>

  implicit def product[H, T <: HList](
    implicit H: Lazy[Read[H]],
              T: Lazy[Read[T]]
  ): Read[H :: T] =
    new Read[H :: T](
      H.value.gets ++ T.value.gets,
      (rs, n) => H.value.unsafeGet(rs, n) :: T.value.unsafeGet(rs, n + H.value.length)
    )

  implicit def emptyProduct: Read[HNil] =
    new Read[HNil](Nil, (_, _) => HNil)

  implicit def generic[F, G](implicit gen: Generic.Aux[F, G], G: Lazy[Read[G]]): Read[F] =
    new Read[F](G.value.gets, (rs, n) => gen.from(G.value.unsafeGet(rs, n)))

}

trait EvenLower {

  implicit val ohnil: Read[Option[HNil]] =
    new Read[Option[HNil]](Nil, (_, _) => Some(HNil))

  implicit def ohcons1[H, T <: HList](
    implicit H: Lazy[Read[Option[H]]],
              T: Lazy[Read[Option[T]]],
              N: H <:!< Option[α] forSome { type α }
  ): Read[Option[H :: T]] = {
    void(N)
    new Read[Option[H :: T]](
      H.value.gets ++ T.value.gets,
      (rs, n) =>
        for {
          h <- H.value.unsafeGet(rs, n)
          t <- T.value.unsafeGet(rs, n + H.value.length)
        } yield h :: t
    )
  }

  implicit def ohcons2[H, T <: HList](
    implicit H: Lazy[Read[Option[H]]],
              T: Lazy[Read[Option[T]]]
  ): Read[Option[Option[H] :: T]] =
    new Read[Option[Option[H] :: T]](
      H.value.gets ++ T.value.gets,
      (rs, n) => T.value.unsafeGet(rs, n + H.value.length).map(H.value.unsafeGet(rs, n) :: _)
    )

  implicit def ogeneric[A, Repr <: HList](
    implicit G: Generic.Aux[A, Repr],
              B: Lazy[Read[Option[Repr]]]
  ): Read[Option[A]] =
    new Read[Option[A]](B.value.gets, B.value.unsafeGet(_, _).map(G.from))

}

