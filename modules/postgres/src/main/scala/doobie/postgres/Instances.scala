// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import doobie.enum.JdbcType
import doobie._
import doobie.util.invariant._

import java.util.{ UUID, Map => JMap }
import java.net.InetAddress

import org.postgresql.util._
import org.postgresql.geometric._

import scala.collection.JavaConverters._
import scala.Predef._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

import cats.data.NonEmptyList.{ of => NonEmptyListOf }

@SuppressWarnings(Array("org.wartremover.warts.ExplicitImplicitTypes"))
trait Instances {

  // N.B. `Meta` is the lowest-level mapping and must always cope with NULL. Easy to forget.

  // Geometric Types, minus PGline which is "not fully implemented"
  implicit val PGboxType      = Meta.Advanced.other[PGbox]("box")
  implicit val PGcircleType   = Meta.Advanced.other[PGcircle]("circle")
  implicit val PGlsegType     = Meta.Advanced.other[PGlseg]("lseg")
  implicit val PGpathType     = Meta.Advanced.other[PGpath]("path")
  implicit val PGpointType    = Meta.Advanced.other[PGpoint]("point")
  implicit val PGpolygonType  = Meta.Advanced.other[PGpolygon]("polygon")

  // PGmoney doesn't seem to work:
  // PSQLException: : Bad value for type double : 1,234.56  (AbstractJdbc2ResultSet.java:3059)
  //   org.postgresql.jdbc2.AbstractJdbc2ResultSet.toDouble(AbstractJdbc2ResultSet.java:3059)
  //   org.postgresql.jdbc2.AbstractJdbc2ResultSet.getDouble(AbstractJdbc2ResultSet.java:2383)
  //   org.postgresql.jdbc2.AbstractJdbc2ResultSet.internalGetObject(AbstractJdbc2ResultSet.java:152)
  //   org.postgresql.jdbc3.AbstractJdbc3ResultSet.internalGetObject(AbstractJdbc3ResultSet.java:36)
  //   org.postgresql.jdbc4.AbstractJdbc4ResultSet.internalGetObject(AbstractJdbc4ResultSet.java:300)
  //   org.postgresql.jdbc2.AbstractJdbc2ResultSet.getObject(AbstractJdbc2ResultSet.java:2704)

  // Interval Type
  // There is no natural mapping to java.time types (https://github.com/tpolecat/doobie/pull/315)
  // so we provide the bare mapping and leave it at that.
  implicit val PGIntervalType = Meta.Advanced.other[PGInterval]("interval")

  // UUID
  implicit val UuidType = Meta.Advanced.other[UUID]("uuid")

  // Network Address Types
  implicit val InetType = Meta.Advanced.other[PGobject]("inet").timap[InetAddress](
    o => Option(o).map(a => InetAddress.getByName(a.getValue)).orNull)(
    a => Option(a).map { a =>
      val o = new PGobject
      o.setType("inet")
      o.setValue(a.getHostAddress)
      o
    } .orNull
  )

  // java.sql.Array::getArray returns an Object that may be of primitive type or of boxed type,
  // depending on the driver, so we can't really abstract over it. Also there's no telling what
  // happens with multi-dimensional arrays since most databases don't support them. So anyway here
  // we go with PostgreSQL support:
  //
  // PostgreSQL arrays show up as Array[AnyRef] with `null` for NULL, so that's mostly sensible;
  // there would be no way to distinguish 0 from NULL otherwise for an int[], for example. So,
  // these arrays can be multi-dimensional and can have NULL cells, but cannot have NULL slices;
  // i.e., {{1,2,3}, {4,5,NULL}} is ok but {{1,2,3}, NULL} is not. So this means we only have to
  // worry about Array[Array[...[A]]] and Array[Array[...[Option[A]]]] in our mappings.

  // Construct a pair of Meta instances for arrays of lifted (nullable) and unlifted (non-
  // nullable) reference types (as noted above, PostgreSQL doesn't ship arrays of primitives). The
  // automatic lifting to Meta will give us lifted and unlifted arrays, for a total of four variants
  // of each 1-d array type. In the non-nullable case we simply check for nulls and perform a cast;
  // in the nullable case we must copy the array in both directions to lift/unlift Option.
  @SuppressWarnings(Array("org.wartremover.warts.Equals", "org.wartremover.warts.ArrayEquals"))
  private def boxedPair[A >: Null <: AnyRef: ClassTag: TypeTag](elemType: String, arrayType: String, arrayTypeT: String*): (Meta[Array[A]], Meta[Array[Option[A]]]) = {
    val raw = Meta.Advanced.array[A](elemType, arrayType, arrayTypeT: _*)
    // Ensure `a`, which may be null, which is ok, contains no null elements.
    def checkNull[B >: Null](a: Array[B], e: Exception): Array[B] =
      if (a == null) null else if (a.exists(_ == null)) throw e else a
    (raw.timap(checkNull(_, NullableCellRead))(checkNull(_, NullableCellUpdate)),
     raw.timap[Array[Option[A]]](_.map(Option(_)))(_.map(_.orNull).toArray))
  }

  // Arrays of lifted (nullable) and unlifted (non-nullable) Java wrapped primitives. PostgreSQL
  // does not seem to support tinyint[] (use a bytea instead) and smallint[] always arrives as Int[]
  // so you can xmap if you need Short[]. The type names provided here are what is reported by JDBC
  // when metadata is requested; there are numerous aliases but these are the ones we need. Nothing
  // about this is portable, sorry. (╯°□°）╯︵ ┻━┻
  implicit val (unliftedBooleanArrayType, liftedBooleanArrayType) = boxedPair[java.lang.Boolean]("bit",     "_bit")
  implicit val (unliftedIntegerArrayType, liftedIntegerArrayType) = boxedPair[java.lang.Integer]("int4",    "_int4")
  implicit val (unliftedLongArrayType,    liftedLongArrayType)    = boxedPair[java.lang.Long]   ("int8",    "_int8")
  implicit val (unliftedFloatArrayType,   liftedFloatArrayType)   = boxedPair[java.lang.Float]  ("float4",  "_float4")
  implicit val (unliftedDoubleArrayType,  liftedDoubleArrayType)  = boxedPair[java.lang.Double] ("float8",  "_float8")
  implicit val (unliftedStringArrayType,  liftedStringArrayType)  = boxedPair[java.lang.String] ("varchar", "_varchar", "_char", "_text", "_bpchar")
  implicit val (unliftedUUIDArrayType,    liftedUUIDArrayType)    = boxedPair[java.util.UUID]   ("uuid", "_uuid")

  // Unboxed equivalents (actually identical in the lifted case). We require that B is the unboxed
  // equivalent of A, otherwise this will fail in spectacular fashion, and we're using a cast in the
  // lifted case because the representation is identical, assuming no nulls. In the long run this
  // may need to become something slower but safer. Unclear.
  @SuppressWarnings(Array("org.wartremover.warts.Equals", "org.wartremover.warts.ArrayEquals", "org.wartremover.warts.AsInstanceOf"))
  private def unboxedPair[A >: Null <: AnyRef: ClassTag, B <: AnyVal: ClassTag: TypeTag](f: A => B, g: B => A)(
    implicit boxed: Meta[Array[A]], boxedLifted: Meta[Array[Option[A]]]): (Meta[Array[B]], Meta[Array[Option[B]]]) =
    // TODO: assert, somehow, that A is the boxed version of B so we catch errors on instance
    // construction, which is somewhat better than at [logical] execution time.
    (boxed.timap(a => if (a == null) null else a.map(f))(a => if (a == null) null else a.map(g)),
     boxedLifted.timap(_.asInstanceOf[Array[Option[B]]])(_.asInstanceOf[Array[Option[A]]]))

  // Arrays of lifted (nullable) and unlifted (non-nullable) AnyVals
  implicit val (unliftedUnboxedBooleanArrayType, liftedUnboxedBooleanArrayType) = unboxedPair[java.lang.Boolean, scala.Boolean](_.booleanValue, java.lang.Boolean.valueOf)
  implicit val (unliftedUnboxedIntegerArrayType, liftedUnboxedIntegerArrayType) = unboxedPair[java.lang.Integer, scala.Int]    (_.intValue,     java.lang.Integer.valueOf)
  implicit val (unliftedUnboxedLongArrayType,    liftedUnboxedLongArrayType)    = unboxedPair[java.lang.Long,    scala.Long]   (_.longValue,    java.lang.Long.valueOf)
  implicit val (unliftedUnboxedFloatArrayType,   liftedUnboxedFloatArrayType)   = unboxedPair[java.lang.Float,   scala.Float]  (_.floatValue,   java.lang.Float.valueOf)
  implicit val (unliftedUnboxedDoubleArrayType,  liftedUnboxedDoubleArrayType)  = unboxedPair[java.lang.Double,  scala.Double] (_.doubleValue,  java.lang.Double.valueOf)

  // So, it turns out that arrays of structs don't work because something is missing from the
  // implementation. So this means we will only be able to support primitive types for arrays.
  //
  // java.sql.SQLFeatureNotSupportedException: Method org.postgresql.jdbc4.Jdbc4Array.getArrayImpl(long,int,Map) is not yet implemented.
  //   at org.postgresql.Driver.notImplemented(Driver.java:729)
  //   at org.postgresql.jdbc2.AbstractJdbc2Array.buildArray(AbstractJdbc2Array.java:771)
  //   at org.postgresql.jdbc2.AbstractJdbc2Array.getArrayImpl(AbstractJdbc2Array.java:171)
  //   at org.postgresql.jdbc2.AbstractJdbc2Array.getArray(AbstractJdbc2Array.java:128)

  // TODO: multidimensional arrays; in the worst case it's just copy/paste of everything above but
  // we can certainly do better than that.


  private def enumPartialMeta(name: String): Meta[String] =
    Meta.Basic.many[String](
      NonEmptyListOf(JdbcType.Other, JdbcType.VarChar), // https://github.com/tpolecat/doobie/issues/303
      NonEmptyListOf(JdbcType.Other, JdbcType.VarChar),
      Nil,
      (rs, n) => rs.getString(n),
      (ps, n, a) => {
        val o = new PGobject
        o.setValue(a.toString)
        o.setType(name)
        ps.setObject(n, o)
      },
      (rs, n, a) => {
        val o = new PGobject
        o.setValue(a.toString)
        o.setType(name)
        rs.updateObject(n, o)
      }
    )

  /**
   * Construct a `Meta` for values of the given type, mapped via `String` to the named PostgreSQL
   * enum type.
   */
  def pgEnumString[A: TypeTag](name: String, f: String => A, g: A => String): Meta[A] =
    enumPartialMeta(name).timap[A](f)(g)

  /**
   * Construct a `Meta` for values of the given type, mapped via `String` to the named PostgreSQL
   * enum type with tranparent partiality.
   */
  def pgEnumStringOpt[A: TypeTag](name: String, f: String => Option[A], g: A => String): Meta[A] =
    pgEnumString(name, {s: String => f(s).getOrElse(throw doobie.util.invariant.InvalidEnum[A](s))} ,g)

  /**
   * Construct a `Meta` for value members of the given `Enumeration`.
   */
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.ToString"))
  def pgEnum(e: Enumeration, name: String): Meta[e.Value] =
    pgEnumString[e.Value](name,
      a => try e.withName(a) catch {
        case _: NoSuchElementException => throw InvalidEnum[e.Value](a)
      }, _.toString)

  /**
   * Construct a `Meta` for value members of the given Jave `enum`.
   */
   @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def pgJavaEnum[E <: java.lang.Enum[E]: TypeTag](name: String)(implicit E: ClassTag[E]): Meta[E] = {
    val clazz = E.runtimeClass.asInstanceOf[Class[E]]
    pgEnumString[E](name,
      a => try java.lang.Enum.valueOf(clazz, a).asInstanceOf[E] catch {
        case _: NoSuchElementException => throw InvalidEnum[E](a)
      }, _.name)
  }

  /** HSTORE maps to a java.util.Map[String, String]. */
  implicit val hstoreMetaJava: Meta[JMap[String, String]] =
    Meta.Advanced.other[JMap[String, String]]("hstore")

  /** HSTORE maps to a Map[String, String]. */
  implicit val hstoreMeta: Meta[Map[String, String]] =
    hstoreMetaJava.timap[Map[String, String]](_.asScala.toMap)(_.asJava)

}
