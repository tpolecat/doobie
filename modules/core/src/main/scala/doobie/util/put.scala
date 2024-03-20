// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.Contravariant
import cats.free.ContravariantCoyoneda
import cats.data.NonEmptyList
import doobie.enumerated.JdbcType
import java.sql.{PreparedStatement, ResultSet}
import org.tpolecat.typename._
import doobie.util.meta.Meta

import scala.reflect.ClassTag

sealed abstract class Put[A](
    val typeStack: NonEmptyList[Option[String]],
    val jdbcTargets: NonEmptyList[JdbcType],
    val schemaTypes: List[String],
    val put: ContravariantCoyoneda[(PreparedStatement, Int, *) => Unit, A],
    val update: ContravariantCoyoneda[(ResultSet, Int, *) => Unit, A]
) {

  def unsafeSetNull(ps: PreparedStatement, n: Int): Unit = {
    val sqlType = jdbcTargets.head.toInt

    schemaTypes.headOption match {
      case None             => ps.setNull(n, sqlType)
      case Some(schemaType) => ps.setNull(n, sqlType, schemaType)
    }
  }

  final def contramap[B](f: B => A): Put[B] =
    contramapImpl(f, None)

  final def tcontramap[B](f: B => A)(implicit ev: TypeName[B]): Put[B] =
    contramapImpl(f, Some(ev.value))

  private def contramapImpl[B](f: B => A, typ: Option[String]): Put[B] =
    new Put[B](
      typeStack = typ :: typeStack,
      jdbcTargets = jdbcTargets,
      schemaTypes = schemaTypes,
      put = put.contramap(f),
      update = update.contramap(f)
    ) {}

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def unsafeSetNonNullable(ps: PreparedStatement, n: Int, a: A): Unit =
    if (a == null) sys.error("oops, null")
    else put.fi.apply(ps, n, (put.k(a)))

  def unsafeSetNullable(ps: PreparedStatement, n: Int, oa: Option[A]): Unit =
    oa match {
      case Some(a) => unsafeSetNonNullable(ps, n, a)
      case None    => unsafeSetNull(ps, n)
    }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def unsafeUpdateNonNullable(rs: ResultSet, n: Int, a: A): Unit =
    if (a == null) sys.error("oops, null")
    else update.fi.apply(rs, n, (update.k(a)))

  def unsafeUpdateNullable(rs: ResultSet, n: Int, oa: Option[A]): Unit =
    oa match {
      case Some(a) => unsafeUpdateNonNullable(rs, n, a)
      case None    => rs.updateNull(n)
    }

}

object Put extends PutInstances {

  def apply[A](implicit ev: Put[A]): ev.type = ev

  def derived[A](implicit ev: MkPut[A]): Put[A] = ev

  trait Auto {
    implicit def derivePut[A](implicit ev: MkPut[A]): Put[A] = ev
  }

  object Basic {

    def apply[A](
        typeStack: NonEmptyList[Option[String]],
        jdbcTargets: NonEmptyList[JdbcType],
        put: ContravariantCoyoneda[(PreparedStatement, Int, *) => Unit, A],
        update: ContravariantCoyoneda[(ResultSet, Int, *) => Unit, A]
    ): Put[A] = new Put[A](typeStack, jdbcTargets, schemaTypes = Nil, put, update) {}

    def many[A](
        jdbcTargets: NonEmptyList[JdbcType],
        put: (PreparedStatement, Int, A) => Unit,
        update: (ResultSet, Int, A) => Unit
    )(implicit ev: TypeName[A]): Put[A] =
      Basic(
        NonEmptyList.of(Some(ev.value)),
        jdbcTargets,
        ContravariantCoyoneda.lift[(PreparedStatement, Int, *) => Unit, A](put),
        ContravariantCoyoneda.lift[(ResultSet, Int, *) => Unit, A](update)
      )

    def one[A](
        jdbcTarget: JdbcType,
        put: (PreparedStatement, Int, A) => Unit,
        update: (ResultSet, Int, A) => Unit
    )(implicit ev: TypeName[A]): Put[A] =
      many(NonEmptyList.of(jdbcTarget), put, update)

  }

  object Advanced {

    def apply[A](
        typeStack: NonEmptyList[Option[String]],
        jdbcTargets: NonEmptyList[JdbcType],
        schemaTypes: NonEmptyList[String],
        put: ContravariantCoyoneda[(PreparedStatement, Int, *) => Unit, A],
        update: ContravariantCoyoneda[(ResultSet, Int, *) => Unit, A]
    ): Put[A] = new Put[A](typeStack, jdbcTargets, schemaTypes.toList, put, update) {}

    def many[A](
        jdbcTargets: NonEmptyList[JdbcType],
        schemaTypes: NonEmptyList[String],
        put: (PreparedStatement, Int, A) => Unit,
        update: (ResultSet, Int, A) => Unit
    )(implicit ev: TypeName[A]): Put[A] =
      Advanced(
        NonEmptyList.of(Some(ev.value)),
        jdbcTargets,
        schemaTypes,
        ContravariantCoyoneda.lift[(PreparedStatement, Int, *) => Unit, A](put),
        ContravariantCoyoneda.lift[(ResultSet, Int, *) => Unit, A](update)
      )

    def one[A: TypeName](
        jdbcTarget: JdbcType,
        schemaTypes: NonEmptyList[String],
        put: (PreparedStatement, Int, A) => Unit,
        update: (ResultSet, Int, A) => Unit
    ): Put[A] =
      many(NonEmptyList.of(jdbcTarget), schemaTypes, put, update)

    @SuppressWarnings(Array("org.wartremover.warts.Equals", "org.wartremover.warts.AsInstanceOf"))
    def array[A >: Null <: AnyRef](
        schemaTypes: NonEmptyList[String],
        elementType: String
    ): Put[Array[A]] =
      one(
        JdbcType.Array,
        schemaTypes,
        (ps, n, a) => {
          val conn = ps.getConnection
          val arr = conn.createArrayOf(elementType, a.asInstanceOf[Array[AnyRef]])
          ps.setArray(n, arr)
        },
        (rs, n, a) => {
          val stmt = rs.getStatement
          val conn = stmt.getConnection
          val arr = conn.createArrayOf(elementType, a.asInstanceOf[Array[AnyRef]])
          rs.updateArray(n, arr)
        }
      )

    def other[A >: Null <: AnyRef: TypeName](schemaTypes: NonEmptyList[String]): Put[A] =
      many(
        NonEmptyList.of(JdbcType.Other, JdbcType.JavaObject),
        schemaTypes,
        (ps, n, a) => ps.setObject(n, a),
        (rs, n, a) => rs.updateObject(n, a)
      )

  }

  /** An implicit Meta[A] means we also have an implicit Put[A]. */
  implicit def metaProjectionWrite[A](
      implicit m: Meta[A]
  ): Put[A] =
    m.put

}

trait PutInstances {

  /** @group Instances */
  implicit val ContravariantPut: Contravariant[Put] =
    new Contravariant[Put] {
      def contramap[A, B](fa: Put[A])(f: B => A): Put[B] =
        fa.contramap(f)
    }

  /** @group Instances */
  implicit def ArrayTypeAsListPut[A: ClassTag](implicit ev: Put[Array[A]]): Put[List[A]] =
    ev.tcontramap(_.toArray)

  /** @group Instances */
  implicit def ArrayTypeAsVectorPut[A: ClassTag](implicit ev: Put[Array[A]]): Put[Vector[A]] =
    ev.tcontramap(_.toArray)

}

sealed abstract class MkPut[A](
    override val typeStack: NonEmptyList[Option[String]],
    override val jdbcTargets: NonEmptyList[JdbcType],
    override val schemaTypes: List[String],
    override val put: ContravariantCoyoneda[(PreparedStatement, Int, *) => Unit, A],
    override val update: ContravariantCoyoneda[(ResultSet, Int, *) => Unit, A]
) extends Put[A](typeStack, jdbcTargets, schemaTypes, put, update)
object MkPut extends PutPlatform {

  def lift[A](g: Put[A]): MkPut[A] =
    new MkPut[A](g.typeStack, g.jdbcTargets, g.schemaTypes, g.put, g.update) {}
}
