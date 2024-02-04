// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.h2

import doobie.enumerated.JdbcType
import doobie.util.invariant._
import java.util.UUID

import scala.Predef._
import scala.reflect.ClassTag
import cats.data.NonEmptyList.{of => NonEmptyListOf}
import doobie.util.meta.Meta

trait Instances {

  implicit val UuidType: Meta[UUID] =
    Meta.Advanced.many[UUID](
      NonEmptyListOf(JdbcType.Binary),
      NonEmptyListOf("uuid", "UUID"),
      _.getObject(_) match {
        case null => null
        case uuidObj =>
          try uuidObj.asInstanceOf[UUID]
          catch {
            case _: ClassCastException => throw InvalidObjectMapping(classOf[UUID], uuidObj.getClass)
          }
      },
      (ps, n, a) => ps.setObject(n, a),
      (rs, n, a) => rs.updateObject(n, a)
    )

  // see postgres contrib for an explanation of array mapping; we may want to factor this out

  private def boxedPair[A >: Null <: AnyRef: ClassTag]: (Meta[Array[A]], Meta[Array[Option[A]]]) = {
    val raw = Meta.Advanced.other[Array[Object]]("ARRAY").timap[Array[A]](a =>
      if (a == null) null else a.map(_.asInstanceOf[A]))(a => if (a == null) null else a.map(_.asInstanceOf[Object]))
    def checkNull[B >: Null](a: Array[B], e: Exception): Array[B] =
      if (a == null) null else if (a.exists(_ == null)) throw e else a
    (
      raw.timap(checkNull(_, NullableCellRead))(checkNull(_, NullableCellUpdate)),
      raw.timap[Array[Option[A]]](_.map(Option(_)))(_.map(_.orNull).toArray))
  }

  private val boxedBooleanPair = boxedPair[java.lang.Boolean]
  implicit val unliftedBooleanArrayType: Meta[Array[java.lang.Boolean]] = boxedBooleanPair._1
  implicit val liftedBooleanArrayType: Meta[Array[Option[java.lang.Boolean]]] = boxedBooleanPair._2

  private val boxedIntegerPair = boxedPair[java.lang.Integer]
  implicit val unliftedIntegerArrayType: Meta[Array[java.lang.Integer]] = boxedIntegerPair._1
  implicit val liftedIntegerArrayType: Meta[Array[Option[java.lang.Integer]]] = boxedIntegerPair._2

  private val boxedLongPair = boxedPair[java.lang.Long]
  implicit val unliftedLongArrayType: Meta[Array[java.lang.Long]] = boxedLongPair._1
  implicit val liftedLongArrayType: Meta[Array[Option[java.lang.Long]]] = boxedLongPair._2

  private val boxedFloatPair = boxedPair[java.lang.Float]
  implicit val unliftedFloatArrayType: Meta[Array[java.lang.Float]] = boxedFloatPair._1
  implicit val liftedFloatArrayType: Meta[Array[Option[java.lang.Float]]] = boxedFloatPair._2

  private val boxedDoublePair = boxedPair[java.lang.Double]
  implicit val unliftedDoubleArrayType: Meta[Array[java.lang.Double]] = boxedDoublePair._1
  implicit val liftedDoubleArrayType: Meta[Array[Option[java.lang.Double]]] = boxedDoublePair._2

  private val boxedStringPair = boxedPair[java.lang.String]
  implicit val unliftedStringArrayType: Meta[Array[java.lang.String]] = boxedStringPair._1
  implicit val liftedStringArrayType: Meta[Array[Option[java.lang.String]]] = boxedStringPair._2

  private def unboxedPair[A >: Null <: AnyRef: ClassTag, B <: AnyVal: ClassTag](f: A => B, g: B => A)(
      implicit
      boxed: Meta[Array[A]],
      boxedLifted: Meta[Array[Option[A]]]
  ): (Meta[Array[B]], Meta[Array[Option[B]]]) =
    (
      boxed.timap(a => if (a == null) null else a.map(f))(a => if (a == null) null else a.map(g)),
      boxedLifted.timap(_.asInstanceOf[Array[Option[B]]])(_.asInstanceOf[Array[Option[A]]]))

  private val unboxedBooleanPair =
    unboxedPair[java.lang.Boolean, scala.Boolean](_.booleanValue, java.lang.Boolean.valueOf)
  implicit val unboxedBooleanArrayType: Meta[Array[scala.Boolean]] = unboxedBooleanPair._1
  implicit val liftedUnboxedBooleanArrayType: Meta[Array[Option[scala.Boolean]]] = unboxedBooleanPair._2

  private val unboxedIntegerPair = unboxedPair[java.lang.Integer, scala.Int](_.intValue, java.lang.Integer.valueOf)
  implicit val unboxedIntegerArrayType: Meta[Array[scala.Int]] = unboxedIntegerPair._1
  implicit val liftedUnboxedIntegerArrayType: Meta[Array[Option[scala.Int]]] = unboxedIntegerPair._2

  private val unboxedLongPair = unboxedPair[java.lang.Long, scala.Long](_.longValue, java.lang.Long.valueOf)
  implicit val unboxedLongArrayType: Meta[Array[scala.Long]] = unboxedLongPair._1
  implicit val liftedUnboxedLongArrayType: Meta[Array[Option[scala.Long]]] = unboxedLongPair._2

  private val unboxedFloatPair = unboxedPair[java.lang.Float, scala.Float](_.floatValue, java.lang.Float.valueOf)
  implicit val unboxedFloatArrayType: Meta[Array[scala.Float]] = unboxedFloatPair._1
  implicit val liftedUnboxedFloatArrayType: Meta[Array[Option[scala.Float]]] = unboxedFloatPair._2

  private val unboxedDoublePair = unboxedPair[java.lang.Double, scala.Double](_.doubleValue, java.lang.Double.valueOf)
  implicit val unboxedDoubleArrayType: Meta[Array[scala.Double]] = unboxedDoublePair._1
  implicit val liftedUnboxedDoubleArrayType: Meta[Array[Option[scala.Double]]] = unboxedDoublePair._2

}
