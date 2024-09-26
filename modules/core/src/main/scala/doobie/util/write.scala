// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.ContravariantSemigroupal
import doobie.enumerated.Nullability.*
import doobie.free.{PreparedStatementIO, ResultSetIO}

import java.sql.{PreparedStatement, ResultSet}
import doobie.util.fragment.Fragment
import doobie.util.fragment.Elem
import doobie.free.{preparedstatement as IFPS, resultset as IFRS}

//import scala.annotation.implicitNotFound

// FIXME:
/*
@implicitNotFound("""
Cannot find or construct a Write instance for type:

  ${A}

This can happen for a few reasons, but the most common case is that a data
member somewhere within this type doesn't have a Put instance in scope. Here are
some debugging hints:

- For Option types, ensure that a Write instance is in scope for the non-Option
  version.
- For types you expect to map to a single column ensure that a Put instance is
  in scope.
- For case classes, HLists, and shapeless records ensure that each element
  has a Write instance in scope.
- Lather, rinse, repeat, recursively until you find the problematic bit.

You can check that an instance exists for Write in the REPL or in your code:

  scala> Write[Foo]

and similarly with Put:

  scala> Put[Foo]

And find the missing instance and construct it as needed. Refer to Chapter 12
of the book of doobie for more information.
""")
 */
sealed abstract class Write[A](
    val puts: List[(Put[?], NullabilityKnown)],
    val toList: A => List[Any],
    val unsafeSet: (PreparedStatement, Int, A) => Unit,
    val unsafeUpdate: (ResultSet, Int, A) => Unit
) {

  lazy val length = puts.length

  def set(n: Int, a: A): PreparedStatementIO[Unit] =
    IFPS.raw(unsafeSet(_, n, a))

  def update(n: Int, a: A): ResultSetIO[Unit] =
    IFRS.raw(unsafeUpdate(_, n, a))

  def contramap[B](f: B => A): Write[B] =
    new Write[B](
      puts,
      b => toList(f(b)),
      (ps, n, a) => unsafeSet(ps, n, f(a)),
      (rs, n, a) => unsafeUpdate(rs, n, f(a))
    ) {}

  def product[B](fb: Write[B]): Write[(A, B)] =
    new Write[(A, B)](
      puts ++ fb.puts,
      { case (a, b) => toList(a) ++ fb.toList(b) },
      { case (ps, n, (a, b)) => unsafeSet(ps, n, a); fb.unsafeSet(ps, n + length, b) },
      { case (rs, n, (a, b)) => unsafeUpdate(rs, n, a); fb.unsafeUpdate(rs, n + length, b) }
    ) {}

  /** Given a value of type `A` and an appropriately parameterized SQL string we can construct a `Fragment`. If `sql` is
    * unspecified a comma-separated list of `length` placeholders will be used.
    */
  def toFragment(a: A, sql: String = List.fill(length)("?").mkString(",")): Fragment = {
    val elems: List[Elem] = (puts zip toList(a)).map {
      case ((p: Put[a], NoNulls), a)  => Elem.Arg(a.asInstanceOf[a], p)
      case ((p: Put[a], Nullable), a) => Elem.Opt(a.asInstanceOf[Option[a]], p)
    }
    Fragment(sql, elems, None)
  }

}

object Write extends WriteInstances {

  def apply[A](
      puts: List[(Put[?], NullabilityKnown)],
      toList: A => List[Any],
      unsafeSet: (PreparedStatement, Int, A) => Unit,
      unsafeUpdate: (ResultSet, Int, A) => Unit
  ): Write[A] = new Write(puts, toList, unsafeSet, unsafeUpdate) {}

  def apply[A](implicit A: Write[A]): Write[A] = A

  def derived[A](implicit ev: Derived[MkWrite[A]]): Write[A] = ev.instance

  trait Auto extends MkWritePlatform {}

  implicit val WriteContravariantSemigroupal: ContravariantSemigroupal[Write] =
    new ContravariantSemigroupal[Write] {
      def contramap[A, B](fa: Write[A])(f: B => A) = fa.contramap(f)
      def product[A, B](fa: Write[A], fb: Write[B]) = fa.product(fb)
    }

  private def doNothing[P, A](p: P, i: Int, a: A): Unit = {
    void(p, i, a)
  }

  private def empty[A](a: A): List[Any] = {
    void(a)
    List.empty
  }

  implicit val unitComposite: Write[Unit] =
    Write[Unit](Nil, empty[Unit](_), doNothing[PreparedStatement, Unit](_, _, _), doNothing[ResultSet, Unit](_, _, _))

  implicit val optionUnit: Write[Option[Unit]] =
    Write[Option[Unit]](
      Nil,
      empty[Option[Unit]](_),
      doNothing[PreparedStatement, Option[Unit]](_, _, _),
      doNothing[ResultSet, Option[Unit]](_, _, _))

  implicit def fromPut[A](implicit P: Put[A]): Write[A] =
    new Write[A](
      List((P, NoNulls)),
      a => List(a),
      (ps, n, a) => P.unsafeSetNonNullable(ps, n, a),
      (rs, n, a) => P.unsafeUpdateNonNullable(rs, n, a)
    ) {}

  implicit def fromPutOption[A](implicit P: Put[A]): Write[Option[A]] =
    new Write[Option[A]](
      List((P, Nullable)),
      a => List(a),
      (ps, n, a) => P.unsafeSetNullable(ps, n, a),
      (rs, n, a) => P.unsafeUpdateNullable(rs, n, a)
    ) {}

}

trait WriteInstances extends WritePlatform

final class MkWrite[A](
    override val puts: List[(Put[?], NullabilityKnown)],
    override val toList: A => List[Any],
    override val unsafeSet: (PreparedStatement, Int, A) => Unit,
    override val unsafeUpdate: (ResultSet, Int, A) => Unit
) extends Write[A](puts, toList, unsafeSet, unsafeUpdate)
object MkWrite extends MkWritePlatform {
  def lift[A](w: Write[A]): MkWrite[A] =
    new MkWrite[A](w.puts, w.toList, w.unsafeSet, w.unsafeUpdate)
}
