// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

/*
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

*/
