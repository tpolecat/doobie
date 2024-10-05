// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.implicits.*
import doobie.enumerated.Nullability
import doobie.enumerated.Nullability.{NoNulls, NullabilityKnown}

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
  def unsafeSet(ps: PreparedStatement, startIdx: Int, a: A): Unit
  def unsafeUpdate(rs: ResultSet, startIdx: Int, a: A): Unit
  def puts: List[(Put[?], NullabilityKnown)]
  def toList(a: A): List[Any]
  def toOpt: Ww[Option[A]]
  def length: Int
  def contramap[B](f: B => A): Ww[B]
}

object Ww {
  case class Pp[A](put: Put[A]) extends Ww[A] {
    override val length: Int = 1

    override def unsafeSet(ps: PreparedStatement, startIdx: Int, a: A): Unit =
      put.unsafeSetNonNullable(ps, startIdx, a)

    override def unsafeUpdate(rs: ResultSet, startIdx: Int, a: A): Unit =
      put.unsafeUpdateNonNullable(rs, startIdx, a)

    override lazy val puts: List[(Put[?], NullabilityKnown)] = List(put -> Nullability.NoNulls)

    override def toList(a: A): List[Any] = List(a)

    override def toOpt: Ww[Option[A]] = Ppo(put)

    override def contramap[B](f: B => A): Ww[B] = Womp[B](List(this), b => List(f(b)))
  }

  case class Ppo[A](put: Put[A]) extends Ww[Option[A]] {
    override val length: Int = 1

    override def unsafeSet(ps: PreparedStatement, startIdx: Int, a: Option[A]): Unit =
      put.unsafeSetNullable(ps, startIdx, a)

    override def unsafeUpdate(rs: ResultSet, startIdx: Int, a: Option[A]): Unit =
      put.unsafeUpdateNullable(rs, startIdx, a)

    override lazy val puts: List[(Put[?], NullabilityKnown)] = List(put -> Nullability.Nullable)

    override def toList(a: Option[A]): List[Any] = List(a)

    override def toOpt: Ww[Option[Option[A]]] = Womp[Option[Option[A]]](List(this), x => List(x.flatten))

    override def contramap[B](f: B => Option[A]): Ww[B] = Womp[B](List(this), b => List(f(b)))
  }

  case class Womp[A](writes: List[Ww[?]], decompose: A => List[Any]) extends Ww[A] {
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

    override def toOpt: Ww[Option[A]] = Womp[Option[A]](
      writes.map(_.toOpt),
      {
        case Some(a) => decompose(a).map(Some(_))
        case None    => List.fill(writes.length)(None) // All Nones
      }
    )

    def contramap[B](f: B => A): Ww[B] = {
      Womp[B](writes, f.andThen(decompose))
    }
  }
}
