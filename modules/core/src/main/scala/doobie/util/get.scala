// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.{Functor, Show}
import cats.data.NonEmptyList
import cats.free.Coyoneda
import doobie.enumerated.JdbcType
import doobie.util.invariant.{InvalidObjectMapping, InvalidValue, NonNullableColumnRead}
import java.sql.ResultSet
import scala.reflect.ClassTag
import org.tpolecat.typename._
import doobie.util.meta.Meta

/** @param typeStack
  *   List of types which provides the lineage of this Get instance
  * @param jdbcSources
  *   Allowed JDBC types for query typechecking purposes
  * @param jdbcSourceSecondary
  *   Alternative allowed JDBC types for query typechecking (warns)
  * @param vendorTypeNames
  *   If non-empty, the column/parameter type reported by the database will be checked to match this list during
  *   typechecking against the database.
  */
sealed abstract class Get[A](
    val typeStack: NonEmptyList[Option[String]],
    val jdbcSources: NonEmptyList[JdbcType],
    val jdbcSourceSecondary: List[JdbcType],
    val vendorTypeNames: List[String],
    val get: Coyoneda[(ResultSet, Int) => *, A]
) {

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  final def unsafeGetNonNullable(rs: ResultSet, n: Int): A = {
    val i = get.fi(rs, n)
    if (rs.wasNull)
      throw NonNullableColumnRead(n, jdbcSources.head)
    get.k(i)
  }

  final def unsafeGetNullable(rs: ResultSet, n: Int): Option[A] = {
    val i = get.fi(rs, n)
    if (rs.wasNull) None else Some(get.k(i))
  }

  /** Apply `f` to values retrieved by this `Get`. Prefer `tmap` when possible because it will allow for better
    * diagnostics when checking queries. Note that `null` values will not be transformed, so you do not need to (nor can
    * you) handle this case.
    */
  final def map[B](f: A => B): Get[B] =
    mapImpl(f, None)

  /** Equivalent to `map`, but accumulates the destination type in the type stack for improved diagnostics. Prefer this
    * method when you have concrete types or an available TypeName.
    */
  final def tmap[B](f: A => B)(implicit ev: TypeName[B]): Get[B] =
    mapImpl(f, Some(ev.value))

  private def mapImpl[B](f: A => B, typ: Option[String]): Get[B] =
    new Get[B](
      typeStack = typ :: typeStack,
      jdbcSources = jdbcSources,
      jdbcSourceSecondary = jdbcSourceSecondary,
      vendorTypeNames = vendorTypeNames,
      get = get.map(f)
    ) {}

  /** Equivalent to `tmap`, but allows the conversion to fail with an error message.
    */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  final def temap[B](f: A => Either[String, B])(implicit sA: Show[A], evA: TypeName[A], evB: TypeName[B]): Get[B] =
    tmap { a =>
      f(a) match {
        case Left(reason) => throw InvalidValue[A, B](a, reason)
        case Right(b)     => b
      }
    }

}

object Get extends GetInstances {

  def apply[A](implicit ev: Get[A]): ev.type = ev

  def derived[A](implicit ev: MkGet[A]): Get[A] = ev

  trait Auto {
    implicit def deriveGet[A](implicit ev: MkGet[A]): Get[A] = ev
  }

  /** Get instance for a basic JDBC type. */
  object Basic {

    def apply[A](
        typeStack: NonEmptyList[Option[String]],
        jdbcSources: NonEmptyList[JdbcType],
        jdbcSourceSecondary: List[JdbcType],
        get: Coyoneda[(ResultSet, Int) => *, A],
        checkedVendorType: Option[String]
    ): Get[A] = new Get[A](
      typeStack,
      jdbcSources = jdbcSources,
      jdbcSourceSecondary = jdbcSourceSecondary,
      vendorTypeNames = checkedVendorType.toList,
      get = get
    ) {}

    def many[A](
        jdbcSources: NonEmptyList[JdbcType],
        jdbcSourceSecondary: List[JdbcType],
        get: (ResultSet, Int) => A,
        checkedVendorType: Option[String]
    )(implicit ev: TypeName[A]): Get[A] =
      Basic(NonEmptyList.of(Some(ev.value)), jdbcSources, jdbcSourceSecondary, Coyoneda.lift(get), checkedVendorType)

    def one[A: TypeName](
        jdbcSources: JdbcType,
        jdbcSourceSecondary: List[JdbcType],
        get: (ResultSet, Int) => A,
        checkedVendorType: Option[String]
    ): Get[A] =
      many(NonEmptyList.of(jdbcSources), jdbcSourceSecondary, get, checkedVendorType)

  }

  /** Get instance for an advanced JDBC type. */
  object Advanced {

    def apply[A](
        typeStack: NonEmptyList[Option[String]],
        jdbcSources: NonEmptyList[JdbcType],
        vendorTypeNames: NonEmptyList[String],
        get: Coyoneda[(ResultSet, Int) => *, A]
    ): Get[A] = new Get[A](
      typeStack,
      jdbcSources = jdbcSources,
      jdbcSourceSecondary = Nil,
      vendorTypeNames = vendorTypeNames.toList,
      get = get
    ) {}

    def many[A](
        jdbcSources: NonEmptyList[JdbcType],
        vendorTypeNames: NonEmptyList[String],
        get: (ResultSet, Int) => A
    )(implicit ev: TypeName[A]): Get[A] =
      Advanced(NonEmptyList.of(Some(ev.value)), jdbcSources, vendorTypeNames, Coyoneda.lift(get))

    def one[A](
        jdbcSource: JdbcType,
        vendorTypeNames: NonEmptyList[String],
        get: (ResultSet, Int) => A
    )(implicit ev: TypeName[A]): Get[A] =
      Advanced(NonEmptyList.of(Some(ev.value)), NonEmptyList.of(jdbcSource), vendorTypeNames, Coyoneda.lift(get))

    @SuppressWarnings(Array("org.wartremover.warts.Equals", "org.wartremover.warts.AsInstanceOf"))
    def array[A >: Null <: AnyRef](vendorTypeNames: NonEmptyList[String]): Get[Array[A]] =
      one(
        JdbcType.Array,
        vendorTypeNames,
        (r, n) => {
          val a = r.getArray(n)
          (if (a == null) null else a.getArray).asInstanceOf[Array[A]]
        }
      )

    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Throw"))
    def other[A >: Null <: AnyRef: TypeName](vendorTypeNames: NonEmptyList[String])(
        implicit A: ClassTag[A]
    ): Get[A] =
      many(
        NonEmptyList.of(JdbcType.Other, JdbcType.JavaObject),
        vendorTypeNames,
        (rs, n) => {
          rs.getObject(n) match {
            case null => null
            case a    =>
              // force the cast here rather than letting a potentially ill-typed value escape
              try A.runtimeClass.cast(a).asInstanceOf[A]
              catch {
                case _: ClassCastException => throw InvalidObjectMapping(A.runtimeClass, a.getClass)
              }
          }
        }
      )

  }

  /** An implicit Meta[A] means we also have an implicit Get[A]. */
  implicit def metaProjection[A](
      implicit m: Meta[A]
  ): Get[A] =
    m.get

}

trait GetInstances {
  import Predef._ // for array ops

  /** @group Instances */
  implicit val FunctorGet: Functor[Get] =
    new Functor[Get] {
      def map[A, B](fa: Get[A])(f: A => B): Get[B] =
        fa.map(f)
    }

  /** @group Instances */
  implicit def ArrayTypeAsListGet[A](implicit ev: Get[Array[A]]): Get[List[A]] =
    ev.tmap(_.toList)

  /** @group Instances */
  implicit def ArrayTypeAsVectorGet[A](implicit ev: Get[Array[A]]): Get[Vector[A]] =
    ev.tmap(_.toVector)

}

sealed abstract class MkGet[A](
    override val typeStack: NonEmptyList[Option[String]],
    override val jdbcSources: NonEmptyList[JdbcType],
    override val jdbcSourceSecondary: List[JdbcType],
    override val vendorTypeNames: List[String],
    override val get: Coyoneda[(ResultSet, Int) => *, A]
) extends Get[A](typeStack, jdbcSources, jdbcSourceSecondary, vendorTypeNames, get)
object MkGet extends GetPlatform {

  def lift[A](g: Get[A]): MkGet[A] =
    new MkGet[A](
      typeStack = g.typeStack,
      jdbcSources = g.jdbcSources,
      jdbcSourceSecondary = g.jdbcSourceSecondary,
      vendorTypeNames = g.vendorTypeNames,
      get = g.get
    ) {}
}
