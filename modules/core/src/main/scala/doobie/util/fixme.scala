// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.ContravariantSemigroupal
import cats.implicits.*
import doobie.{Fragment, PreparedStatementIO, ResultSetIO}
import doobie.enumerated.Nullability
import doobie.enumerated.Nullability.{NoNulls, NullabilityKnown, Nullable}
import doobie.free.{preparedstatement as IFPS, resultset as IFRS}
import doobie.util.fragment.Elem

import java.sql.{PreparedStatement, ResultSet}
// FIXME:
sealed trait Rr[A] {
  def unsafeGet(rs: ResultSet, startidx: Int): A

  def gets: List[(Get[?], NullabilityKnown)]

  def toOpt: Rr[Option[A]]

  def length: Int
}

object Rr {
  def ret[A](implicit get: Get[A]): Rr[A] = Ret(get)

  def roet[A](implicit get: Get[A]): Rr[Option[A]] = Roet(get)

  case class Ret[A](get: Get[A]) extends Rr[A] {
    def unsafeGet(rs: ResultSet, startIdx: Int): A =
      get.unsafeGetNonNullable(rs, startIdx)

    override def toOpt: Rr[Option[A]] = Roet(get)

    override def gets: List[(Get[_], NullabilityKnown)] = List(get -> NoNulls)

    override val length: Int = 1
  }

  case class Roet[A](get: Get[A]) extends Rr[Option[A]] {
    def unsafeGet(rs: ResultSet, startIdx: Int): Option[A] =
      get.unsafeGetNullable(rs, startIdx)

    override def toOpt: Rr[Option[Option[A]]] = Comp(List(this), l => Some(l.head.asInstanceOf[Option[A]]))
    override def gets: List[(Get[_], NullabilityKnown)] = List(get -> Nullability.Nullable)

    override val length: Int = 1
  }

  case class Comp[A](rrs: List[Rr[?]], construct: List[Any] => A) extends Rr[A] {

    override val length: Int = rrs.map(_.length).sum

    override def gets: List[(Get[_], NullabilityKnown)] = rrs.flatMap(_.gets)

    override def unsafeGet(rs: ResultSet, startIdx: Int): A = {
      import scala.collection.mutable
      val accum = mutable.ArrayBuffer.empty[Any]
      var idx = startIdx
      rrs.foreach { rr =>
        accum += rr.unsafeGet(rs, idx)
        idx += rr.length
      }
      construct(accum.toList)
    }

    override def toOpt: Rr[Option[A]] = {
      val orrs = rrs.map(_.toOpt)

      val constr: List[Option[Any]] => Option[A] = l =>
        l.sequence.map(construct)

      new Comp[Option[A]](orrs, constr.asInstanceOf[List[Any] => Option[A]])

    }

  }

}

sealed trait Ww[A] {
  def puts: List[(Put[?], NullabilityKnown)]
  def toList(a: A): List[Any]
  def unsafeSet(ps: PreparedStatement, startIdx: Int, a: A): Unit
  def unsafeUpdate(rs: ResultSet, startIdx: Int, a: A): Unit
  def toOpt: Ww[Option[A]]
  def length: Int

  final def set(n: Int, a: A): PreparedStatementIO[Unit] =
    IFPS.raw(unsafeSet(_, n, a))

  final def update(n: Int, a: A): ResultSetIO[Unit] =
    IFRS.raw(unsafeUpdate(_, n, a))

  def contramap[B](f: B => A): Ww[B]

  final def product[B](fb: Ww[B]): Ww[(A, B)] = {
    new Ww.Composite[(A, B)](List(this, fb), tuple => List(tuple._1, tuple._2))
  }

  def toFragment(a: A, sql: String = List.fill(length)("?").mkString(",")): Fragment = {
    val elems: List[Elem] = (puts zip toList(a)).map {
      case ((p: Put[a], NoNulls), a)  => Elem.Arg(a.asInstanceOf[a], p)
      case ((p: Put[a], Nullable), a) => Elem.Opt(a.asInstanceOf[Option[a]], p)
    }
    Fragment(sql, elems, None)
  }
}

object Ww {
  def apply[A](implicit A: Ww[A]): Ww[A] = A

  def derived[A](implicit ev: Derived[MkWrite[A]]): Write[A] = ev.instance

  trait Auto extends MkWritePlatform {}

  implicit val WriteContravariantSemigroupal: ContravariantSemigroupal[Ww] =
    new ContravariantSemigroupal[Ww] {
      def contramap[A, B](fa: Ww[A])(f: B => A): Ww[B] = fa.contramap(f)
      def product[A, B](fa: Ww[A], fb: Ww[B]): Ww[(A, B)] = fa.product(fb)
    }

  private def doNothing[P, A](p: P, i: Int, a: A): Unit = {
    void(p, i, a)
  }

  implicit val unitComposite: Ww[Unit] =
    Ww.Composite[Unit](Nil, _ => List.empty)

  implicit val optionUnit: Ww[Option[Unit]] =
    Ww.Composite[Option[Unit]](Nil, _ => List.empty)

  implicit def fromPut[A](implicit put: Put[A]): Ww[A] =
    Ww.Single(put)

  implicit def fromPutOption[A](implicit put: Put[A]): Ww[Option[A]] =
    Ww.OptSingle(put)

  case class Single[A](put: Put[A]) extends Ww[A] {
    override val length: Int = 1

    override def unsafeSet(ps: PreparedStatement, startIdx: Int, a: A): Unit =
      put.unsafeSetNonNullable(ps, startIdx, a)

    override def unsafeUpdate(rs: ResultSet, startIdx: Int, a: A): Unit =
      put.unsafeUpdateNonNullable(rs, startIdx, a)

    override lazy val puts: List[(Put[?], NullabilityKnown)] = List(put -> Nullability.NoNulls)

    override def toList(a: A): List[Any] = List(a)

    override def toOpt: Ww[Option[A]] = OptSingle(put)

    override def contramap[B](f: B => A): Ww[B] = Composite[B](List(this), b => List(f(b)))
  }

  case class OptSingle[A](put: Put[A]) extends Ww[Option[A]] {
    override val length: Int = 1

    override def unsafeSet(ps: PreparedStatement, startIdx: Int, a: Option[A]): Unit =
      put.unsafeSetNullable(ps, startIdx, a)

    override def unsafeUpdate(rs: ResultSet, startIdx: Int, a: Option[A]): Unit =
      put.unsafeUpdateNullable(rs, startIdx, a)

    override lazy val puts: List[(Put[?], NullabilityKnown)] = List(put -> Nullability.Nullable)

    override def toList(a: Option[A]): List[Any] = List(a)

    override def toOpt: Ww[Option[Option[A]]] = Composite[Option[Option[A]]](List(this), x => List(x.flatten))

    override def contramap[B](f: B => Option[A]): Ww[B] = Composite[B](List(this), b => List(f(b)))
  }

  case class Composite[A](writes: List[Ww[?]], decompose: A => List[Any]) extends Ww[A] {
    override lazy val length: Int = writes.map(_.length).sum

    // Make the types match up with decompose
    private val anyWrites: List[Ww[Any]] = writes.asInstanceOf[List[Ww[Any]]]

    override def unsafeSet(ps: PreparedStatement, startIdx: Int, a: A): Unit = {
      val parts = decompose(a)
      var idx = startIdx
      anyWrites.zip(parts).foreach { case (w, p) =>
        w.unsafeSet(ps, idx, p)
        idx += w.length
      }
    }

    override def unsafeUpdate(rs: ResultSet, startIdx: Int, a: A): Unit = {
      val parts = decompose(a)
      var idx = startIdx
      anyWrites.zip(parts).foreach { case (w, p) =>
        w.unsafeUpdate(rs, idx, p)
        idx += w.length
      }
    }

    override lazy val puts: List[(Put[?], NullabilityKnown)] = writes.flatMap(_.puts)

    override def toList(a: A): List[Any] =
      anyWrites.zip(decompose(a)).flatMap { case (w, p) => w.toList(p) }

    override def toOpt: Ww[Option[A]] = Composite[Option[A]](
      writes.map(_.toOpt),
      {
        case Some(a) => decompose(a).map(Some(_))
        case None    => List.fill(writes.length)(None) // All Nones
      }
    )

    def contramap[B](f: B => A): Ww[B] = {
      Composite[B](writes, f.andThen(decompose))
    }
  }
}

final class MkWw[A](
    underlying: Ww[A]
) extends Ww[A] {
  override def puts: List[(Put[_], NullabilityKnown)] = underlying.puts
  override def toList(a: A): List[Any] = underlying.toList(a)
  override def unsafeSet(ps: PreparedStatement, startIdx: Int, a: A): Unit =
    underlying.unsafeSet(ps, startIdx, a)
  override def unsafeUpdate(rs: ResultSet, startIdx: Int, a: A): Unit =
    underlying.unsafeUpdate(rs, startIdx, a)
  override def toOpt: Ww[Option[A]] = underlying.toOpt
  override def length: Int = underlying.length
  override def contramap[B](f: B => A): Ww[B] = underlying.contramap(f)
}

object MkWw {
  // FIXME: need?
  def lift[A](w: Ww[A]): MkWw[A] = new MkWw(w)
}
