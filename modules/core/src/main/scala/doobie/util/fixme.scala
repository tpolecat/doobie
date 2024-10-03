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

  def getInstances: List[(Get[?], NullabilityKnown)]

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

    override def getInstances: List[(Get[_], NullabilityKnown)] = List(get -> NoNulls)

    override val length: Int = 1
  }

  case class Roet[A](get: Get[A]) extends Rr[Option[A]] {
    def unsafeGet(rs: ResultSet, startIdx: Int): Option[A] =
      get.unsafeGetNullable(rs, startIdx)

    override def toOpt: Rr[Option[Option[A]]] = Comp(List(this), l => Some(l.head.asInstanceOf[Option[A]]))
    override def getInstances: List[(Get[_], NullabilityKnown)] = List(get -> Nullability.Nullable)

    override val length: Int = 1
  }

  case class Comp[A](rrs: List[Rr[?]], construct: List[Any] => A) extends Rr[A] {

    override val length: Int = rrs.map(_.length).sum

    override def getInstances: List[(Get[_], NullabilityKnown)] = rrs.flatMap(_.getInstances)

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

      def constr(l: List[Option[Any]]): Option[A] = {
        l.sequence.map(construct)
      }

      new Comp[Option[A]](orrs, constr.asInstanceOf[List[Any] => Option[A]])

    }

  }

}

sealed trait Ww[A] {
  def unsafeSet(ps: PreparedStatement, i: Int, a: A): Unit
  def unsafeUpdate(rs: ResultSet, i: Int, a: A): Unit
  def puts: List[(Put[?], NullabilityKnown)]
  def toList(a: A): List[Any]
  def toOpt: Ww[Option[A]]
}

object Ww {
  case class Pp[A](put: Put[A]) extends Ww[A] {
    override def unsafeSet(ps: PreparedStatement, i: Int, a: A): Unit =
      put.unsafeSetNonNullable(ps, i, a)

    override def unsafeUpdate(rs: ResultSet, i: Int, a: A): Unit =
      put.unsafeUpdateNonNullable(rs, i, a)

    override def puts: List[(Put[?], NullabilityKnown)] = List(put -> Nullability.NoNulls)

    override def toList(a: A): List[Any] = List(a)

    override def toOpt: Ww[Option[A]] = Ppo(put)
  }
  
  case class Ppo[A](put: Put[A]) extends Ww[Option[A]] {

    override def unsafeSet(ps: PreparedStatement, i: Int, a: Option[A]): Unit =
      put.unsafeSetNullable(ps, i, a)

    override def unsafeUpdate(rs: ResultSet, i: Int, a: Option[A]): Unit = 
      put.unsafeUpdateNullable(rs, i, a)

    override def puts: List[(Put[_], NullabilityKnown)] = List(put -> Nullability.Nullable)

    override def toList(a: Option[A]): List[Any] = List(a)

    override def toOpt: Ww[Option[Option[A]]] = ???
  }
  
  case class Womp[A](wuts: List[Ww[?]], decompose: A => List[Any]) extends Ww[A] {

    override def unsafeSet(ps: PreparedStatement, startIdx: Int, a: A): Unit = {
      val parts = decompose(a)
      var idx = startIdx
      parts.foreach { p =>
        wuts(0)
      }
    }

    override def unsafeUpdate(rs: ResultSet, i: Int, a: A): Unit = ???

    override def puts: List[(Put[_], NullabilityKnown)] = ???

    override def toList(a: A): List[Any] = ???

    override def toOpt: Ww[Option[A]] = ???
  }
}
