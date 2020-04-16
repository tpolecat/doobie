// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.ContravariantSemigroupal
import doobie.enum.Nullability._
import doobie.free.{ FPS, FRS, PreparedStatementIO, ResultSetIO }
import java.sql.{ PreparedStatement, ResultSet }
import shapeless.{ HList, HNil, ::, Generic, Lazy, <:!< }
import shapeless.labelled.{ FieldType }
import doobie.util.fragment.Fragment
import doobie.util.fragment.Elem

final class Write[A](
  val puts: List[(Put[_], NullabilityKnown)],
  val toList: A => List[Any],
  val unsafeSet: (PreparedStatement, Int, A) => Unit,
  val unsafeUpdate: (ResultSet, Int, A) => Unit
) {

  lazy val length = puts.length

  def set(n: Int, a: A): PreparedStatementIO[Unit] =
    FPS.raw(unsafeSet(_, n, a))

  def update(n: Int, a: A): ResultSetIO[Unit] =
    FRS.raw(unsafeUpdate(_, n, a))

  def contramap[B](f: B => A): Write[B] =
    new Write(
      puts,
      b => toList(f(b)),
      (ps, n, a) => unsafeSet(ps, n, f(a)),
      (rs, n, a) => unsafeUpdate(rs, n, f(a))
    )

  def product[B](fb: Write[B]): Write[(A, B)] =
    new Write(
      puts ++ fb.puts,
      { case (a, b) => toList(a) ++ fb.toList(b) },
      { case (ps, n, (a, b)) => unsafeSet(ps, n, a); fb.unsafeSet(ps, n + length, b) },
      { case (rs, n, (a, b)) => unsafeUpdate(rs, n, a); fb.unsafeUpdate(rs, n + length, b) }
    )

  /**
   * Given a value of type `A` and an appropriately parameterized SQL string we can construct a
   * `Fragment`. If `sql` is unspecified a comma-separated list of `length` placeholders will be used.
   */
  def toFragment(a: A, sql: String = List.fill(length)("?").mkString(",")): Fragment[Any] = {
    val elems: List[Elem[Any]] = (puts zip toList(a)).map {
      case ((p: Put[a], NoNulls), a)  => Elem.Arg(a.asInstanceOf[a], p)
      case ((p: Put[a], Nullable), a) => Elem.Opt(a.asInstanceOf[Option[a]], p)
    }
    Fragment(sql, elems, None)
  }

}

object Write extends LowerPriorityWrite {

  def apply[A](implicit A: Write[A]): Write[A] = A

  implicit val ReadContravariantSemigroupal: ContravariantSemigroupal[Write] =
    new ContravariantSemigroupal[Write] {
      def contramap[A, B](fa: Write[A])(f: B => A) = fa.contramap(f)
      def product[A, B](fa: Write[A], fb: Write[B]) = fa.product(fb)
    }

  implicit val unitComposite: Write[Unit] =
    new Write(Nil, _ => Nil, (_, _, _) => (), (_, _, _) => ())

  implicit def fromPut[A](implicit P: Put[A]): Write[A] =
    new Write(
      List((P, NoNulls)),
      a => List(a),
      (ps, n, a) => P.unsafeSetNonNullable(ps, n, a),
      (rs, n, a) => P.unsafeUpdateNonNullable(rs, n, a)
    )

  implicit def fromPutOption[A](implicit P: Put[A]): Write[Option[A]] =
    new Write(
      List((P, Nullable)),
      a => List(a),
      (ps, n, a) => P.unsafeSetNullable(ps, n, a),
      (rs, n, a) => P.unsafeUpdateNullable(rs, n, a)
    )

  implicit def recordWrite[K <: Symbol, H, T <: HList](
    implicit H: Lazy[Write[H]],
              T: Lazy[Write[T]]
  ): Write[FieldType[K, H] :: T] = {
    new Write(
      H.value.puts ++ T.value.puts,
      { case h :: t => H.value.toList(h) ++ T.value.toList(t) },
      { case (ps, n, h :: t) => H.value.unsafeSet(ps, n, h); T.value.unsafeSet(ps, n + H.value.length, t) },
      { case (rs, n, h :: t) => H.value.unsafeUpdate(rs, n, h); T.value.unsafeUpdate(rs, n + H.value.length, t) }
    )
  }

}

trait LowerPriorityWrite extends EvenLowerPriorityWrite {

  implicit def product[H, T <: HList](
    implicit H: Lazy[Write[H]],
              T: Lazy[Write[T]]
  ): Write[H :: T] =
    new Write(
      H.value.puts ++ T.value.puts,
      { case h :: t => H.value.toList(h) ++ T.value.toList(t) },
      { case (ps, n, h :: t) => H.value.unsafeSet(ps, n, h); T.value.unsafeSet(ps, n + H.value.length, t) },
      { case (rs, n, h :: t) => H.value.unsafeUpdate(rs, n, h); T.value.unsafeUpdate(rs, n + H.value.length, t) }
    )

  implicit def emptyProduct: Write[HNil] =
    new Write[HNil](Nil, _ => Nil, (_, _, _) => (), (_, _, _) => ())

  implicit def generic[B, A](implicit gen: Generic.Aux[B, A], A: Lazy[Write[A]]): Write[B] =
    new Write[B](
      A.value.puts,
      b => A.value.toList(gen.to(b)),
      (ps, n, b) => A.value.unsafeSet(ps, n, gen.to(b)),
      (rs, n, b) => A.value.unsafeUpdate(rs, n, gen.to(b))
    )

}

trait EvenLowerPriorityWrite {

  implicit val ohnil: Write[Option[HNil]] =
    new Write[Option[HNil]](Nil, _ => Nil, (_, _, _) => (), (_, _, _) => ())

  implicit def ohcons1[H, T <: HList](
    implicit H: Lazy[Write[Option[H]]],
             T: Lazy[Write[Option[T]]],
             N: H <:!< Option[α] forSome { type α }
  ): Write[Option[H :: T]] = {
    void(N)

    def split[A](i: Option[H :: T])(f: (Option[H], Option[T]) => A): A =
      i.fold(f(None, None)) { case h :: t => f(Some(h), Some(t)) }

    new Write(
      H.value.puts ++ T.value.puts,
      split(_) { (h, t) => H.value.toList(h) ++ T.value.toList(t) },
      (ps, n, i) => split(i) { (h, t) => H.value.unsafeSet(ps, n, h); T.value.unsafeSet(ps, n + H.value.length, t) },
      (rs, n, i) => split(i) { (h, t) => H.value.unsafeUpdate(rs, n, h); T.value.unsafeUpdate(rs, n + H.value.length, t) }
    )

  }

  implicit def ohcons2[H, T <: HList](
    implicit H: Lazy[Write[Option[H]]],
             T: Lazy[Write[Option[T]]]
  ): Write[Option[Option[H] :: T]] = {

    def split[A](i: Option[Option[H] :: T])(f: (Option[H], Option[T]) => A): A =
      i.fold(f(None, None)) { case oh :: t => f(oh, Some(t)) }

    new Write(
      H.value.puts ++ T.value.puts,
      split(_) { (h, t) => H.value.toList(h) ++ T.value.toList(t) },
      (ps, n, i) => split(i) { (h, t) => H.value.unsafeSet(ps, n, h); T.value.unsafeSet(ps, n + H.value.length, t) },
      (rs, n, i) => split(i) { (h, t) => H.value.unsafeUpdate(rs, n, h); T.value.unsafeUpdate(rs, n + H.value.length, t) }
    )

  }

  implicit def ogeneric[B, A <: HList](
    implicit G: Generic.Aux[B, A],
             A: Lazy[Write[Option[A]]]
  ): Write[Option[B]] =
    new Write(
      A.value.puts,
      b => A.value.toList(b.map(G.to)),
      (rs, n, a) => A.value.unsafeSet(rs, n, a.map(G.to)),
      (rs, n, a) => A.value.unsafeUpdate(rs, n, a.map(G.to))
    )

}
