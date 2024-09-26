// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import java.math.{BigDecimal as JBigDecimal}
import java.net.InetAddress
import java.time.{LocalDate, LocalDateTime, OffsetDateTime, ZoneOffset}
import java.util.UUID
import doobie.*
import doobie.util.{Put, Get}
import doobie.implicits.*
import doobie.postgres.enums.*
import doobie.postgres.implicits.*
import doobie.postgres.pgisimplicits.*
import doobie.postgres.rangeimplicits.*
import doobie.postgres.types.{EmptyRange, NonEmptyRange, Range}
import doobie.postgres.types.Range.Edge.*
import doobie.postgres.util.arbitraries.SQLArbitraries.*
import doobie.postgres.util.arbitraries.TimeArbitraries.*
import doobie.util.arbitraries.StringArbitraries.*
import net.postgis.jdbc.geometry.*
import org.postgresql.geometric.*
import org.postgresql.util.*
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import scala.collection.compat.immutable.LazyList

// Establish that we can write and read various types.
class TypesSuite extends munit.ScalaCheckSuite {
  import cats.effect.unsafe.implicits.global
  import PostgresTestTransactor.xa

  def inOut[A: Get: Put](col: String, a: A): ConnectionIO[A] = for {
    _ <- Update0(s"CREATE TEMPORARY TABLE TEST (value $col NOT NULL)", None).run
    a0 <- Update[A](s"INSERT INTO TEST VALUES (?)", None).withUniqueGeneratedKeys[A]("value")(a)
  } yield a0

  def inOutOpt[A: Get: Put](col: String, a: Option[A]): ConnectionIO[Option[A]] =
    for {
      _ <- Update0(s"CREATE TEMPORARY TABLE TEST (value $col)", None).run
      a0 <- Update[Option[A]](s"INSERT INTO TEST VALUES (?)", None).withUniqueGeneratedKeys[Option[A]]("value")(a)
    } yield a0

  def testInOut[A](col: String)(implicit m: Get[A], p: Put[A], arbitrary: Arbitrary[A]) = {
    testInOutWithCustomGen(col, arbitrary.arbitrary)
  }

  def testInOutTweakExpected[A](col: String)(f: A => A)(implicit m: Get[A], p: Put[A], arbitrary: Arbitrary[A]) = {
    testInOutWithCustomGen(col, arbitrary.arbitrary, f)
  }

  def testInOut[A](col: String, a: A)(implicit m: Get[A], p: Put[A]) = {
    test(s"Mapping for $col as ${m.typeStack} - write+read $col as ${m.typeStack}") {
      assertEquals(inOut(col, a).transact(xa).attempt.unsafeRunSync(), Right(a))
    }
    test(s"Mapping for $col as ${m.typeStack} - write+read $col as Option[${m.typeStack}] (Some)") {
      assertEquals(inOutOpt[A](col, Some(a)).transact(xa).attempt.unsafeRunSync(), Right(Some(a)))
    }
    test(s"Mapping for $col as ${m.typeStack} - write+read $col as Option[${m.typeStack}] (None)") {
      assertEquals(inOutOpt[A](col, None).transact(xa).attempt.unsafeRunSync(), Right(None))
    }
  }

  def testInOutWithCustomGen[A](col: String, gen: Gen[A], expected: A => A = identity[A](_))(implicit
      m: Get[A],
      p: Put[A]
  ) = {
    test(s"Mapping for $col as ${m.typeStack} - write+read $col as ${m.typeStack}") {
      forAll(gen) { (t: A) => assertEquals(inOut(col, t).transact(xa).attempt.unsafeRunSync(), Right(expected(t))) }
    }
    test(s"Mapping for $col as ${m.typeStack} - write+read $col as Option[${m.typeStack}] (Some)") {
      forAll(gen) { (t: A) =>
        assertEquals(inOutOpt[A](col, Some(t)).transact(xa).attempt.unsafeRunSync(), Right(Some(expected(t))))
      }
    }
    test(s"Mapping for $col as ${m.typeStack} - write+read $col as Option[${m.typeStack}] (None)") {
      assertEquals(inOutOpt[A](col, None).transact(xa).attempt.unsafeRunSync(), Right(None))
    }
  }

  def skip(col: String, msg: String = "not yet implemented") =
    test(s"Mapping for $col ($msg)".ignore) {}

  // 8.1 Numeric Types
  testInOut[Short]("smallint")
  testInOut[Int]("integer")
  testInOut[Long]("bigint")
  testInOut[BigDecimal]("decimal")
  testInOut[BigDecimal]("numeric")
  testInOut[Float]("real")
  testInOut[Double]("double precision")

  // 8.2 Monetary Types
  skip("pgmoney", "getObject returns Double")

  // 8.3 Character Types"
  testInOut[String]("character varying")
  testInOut[String]("varchar")
  testInOutWithCustomGen("character(6)", nLongString(6))
  testInOutWithCustomGen("char(6)", nLongString(6))
  testInOut[String]("text")

  // 8.4 Binary Types
  testInOut[List[Byte]]("bytea")
  testInOut[Vector[Byte]]("bytea")

  // 8.5 Date/Time Types"

  /*
      timestamp
      The allowed range of p is from 0 to 6 for the timestamp and interval types.
   */
  testInOut[java.sql.Timestamp]("timestamptz")
  testInOut[java.time.Instant]("timestamptz")
  testInOutTweakExpected[java.time.OffsetDateTime]("timestamptz")(
    _.withOffsetSameInstant(ZoneOffset.UTC)
  ) // +148488-07-03T02:38:17Z != +148488-07-03T00:00-02:38:17

  /*
    local date & time (not an instant in time)
   */
  testInOut[java.time.LocalDateTime]("timestamp")

  testInOut[java.sql.Date]("date")
  testInOut[java.time.LocalDate]("date")

  testInOut[java.sql.Time]("time")
  testInOut[java.time.LocalTime]("time")

  testInOut[java.time.OffsetTime]("time with time zone")

  testInOut("interval", new PGInterval(1, 2, 3, 4, 5, 6.7))

  testInOut[java.time.ZoneId]("text")

  // 8.6 Boolean Type
  testInOut[Boolean]("boolean")

  // 8.7 Enumerated Types
  testInOut("myenum", MyEnum.Foo: MyEnum)

  // as scala.Enumeration
  implicit val MyEnumMeta: Meta[MyScalaEnum.Value] = pgEnum(MyScalaEnum, "myenum")
  testInOut("myenum", MyScalaEnum.foo)

  // as java.lang.Enum
  implicit val MyJavaEnumMeta: Meta[MyJavaEnum] = pgJavaEnum[MyJavaEnum]("myenum")
  testInOut("myenum", MyJavaEnum.bar)

  // 8.8 Geometric Types
  testInOut("box", new PGbox(new PGpoint(1, 2), new PGpoint(3, 4)))
  testInOut("circle", new PGcircle(new PGpoint(1, 2), 3))
  testInOut("lseg", new PGlseg(new PGpoint(1, 2), new PGpoint(3, 4)))
  testInOut("path", new PGpath(Array(new PGpoint(1, 2), new PGpoint(3, 4)), false))
  testInOut("path", new PGpath(Array(new PGpoint(1, 2), new PGpoint(3, 4)), true))
  testInOut("point", new PGpoint(1, 2))
  testInOut("polygon", new PGpolygon(Array(new PGpoint(1, 2), new PGpoint(3, 4))))
  skip("line", "doc says \"not fully implemented\"")

  // test postgis geography mappings
  {
    def createPoint(lat: Double, lon: Double): Point = {
      val p = new Point(lon, lat)
      p.setSrid(4326)

      p
    }

    import doobie.postgres.pgisgeographyimplicits.*
    val point1 = createPoint(1, 2)
    val point2 = createPoint(1, 3)
    val lineString = new LineString(Array[Point](point1, point2))
    lineString.setSrid(4326)

    // test geography points
    testInOut("GEOGRAPHY(POINT)", point1)
    testInOut("GEOGRAPHY(LINESTRING)", lineString)
  }

  // 8.9 Network Address Types
  testInOut("inet", InetAddress.getByName("123.45.67.8"))
  skip("inet", "no suitable JDK type")
  skip("macaddr", "no suitable JDK type")

  // 8.10 Bit String Types
  skip("bit")
  skip("bit varying")

  // 8.11 Text Search Types
  skip("tsvector")
  skip("tsquery")

  // 8.12 UUID Type
  testInOut[UUID]("uuid")

  // 8.13 XML Type
  skip("xml")

  // 8.14 JSON Type
  skip("json")

  // 8.15 Arrays
  skip("bit[]", "Requires a cast")
  skip("smallint[]", "always comes back as Array[Int]")
  testInOut[List[Int]]("integer[]")
  testInOut[List[Long]]("bigint[]")
  testInOut[List[Float]]("real[]")
  testInOut[List[Double]]("double precision[]")
  testInOut[List[String]]("varchar[]")
  testInOut[List[UUID]]("uuid[]")
  testInOut("numeric[]", List[JBigDecimal](BigDecimal("3.14").bigDecimal, BigDecimal("42.0").bigDecimal))
  testInOut[List[BigDecimal]]("numeric[]", List[BigDecimal](BigDecimal("3.14"), BigDecimal("42.0")))

  // 8.16 Structs
  skip("structs")

  // 8.17 Range Types
  testInOutWithCustomGen[Range[Int]]("int4range", EmptyRange)
  testInOutWithCustomGen[Range[Long]]("int8range", EmptyRange)
  testInOutWithCustomGen[Range[BigDecimal]]("numrange", EmptyRange)
  testInOutWithCustomGen[Range[LocalDate]]("daterange", EmptyRange)
  testInOutWithCustomGen[Range[LocalDateTime]]("tsrange", EmptyRange)
  testInOutWithCustomGen[Range[OffsetDateTime]]("tstzrange", EmptyRange)
  testInOutWithCustomGen[Range[Int]]("int4range", Range(11, 22, ExclExcl), _ => Range(12, 22, InclExcl))
  testInOutWithCustomGen[Range[Int]]("int4range", Range(11, 22, InclExcl), _ => Range(11, 22, InclExcl))
  testInOutWithCustomGen[Range[Int]]("int4range", Range(11, 22, ExclIncl), _ => Range(12, 23, InclExcl))
  testInOutWithCustomGen[Range[Int]]("int4range", Range(11, 22, InclIncl), _ => Range(11, 23, InclExcl))
  testInOut[Range[Int]]("int4range", Range(11, 22))
  testInOut[Range[Long]]("int8range", Range[Long](111, 222))
  testInOut[Range[BigDecimal]]("numrange", Range[BigDecimal](111.111, 222.222, ExclExcl))
  testInOut[Range[LocalDate]]("daterange", Range(LocalDate.now.minusDays(10), LocalDate.now, InclExcl))
  testInOut[Range[LocalDateTime]]("tsrange", Range(LocalDateTime.now.minusDays(10), LocalDateTime.now, ExclExcl))
  testInOut[Range[OffsetDateTime]]("tstzrange", Range(OffsetDateTime.now.minusDays(10), OffsetDateTime.now, ExclExcl))
  testInOutWithCustomGen[Range[Int]](
    "int4range",
    NonEmptyRange[Int](None, Some(1), InclExcl),
    _ => NonEmptyRange[Int](None, Some(1), ExclExcl))
  testInOutWithCustomGen[Range[Int]](
    "int4range",
    NonEmptyRange[Int](Some(1), None, ExclIncl),
    _ => NonEmptyRange[Int](Some(2), None, InclExcl))

  // Custom byte range
  implicit val byteRangeMeta: Meta[Range[Byte]] = rangeMeta[Byte]("int4range")(_.toString, java.lang.Byte.parseByte)

  testInOut[Range[Byte]]("int4range", Range[Byte](-128, 127))

  // PostGIS geometry types

  // Random streams of geometry values
  lazy val rnd: Iterator[Double] = LazyList.continually(scala.util.Random.nextDouble()).iterator
  lazy val pts: Iterator[Point] = LazyList.continually(new Point(rnd.next(), rnd.next())).iterator
  lazy val lss: Iterator[LineString] =
    LazyList.continually(new LineString(Array(pts.next(), pts.next(), pts.next()))).iterator
  lazy val lrs: Iterator[LinearRing] = LazyList.continually(new LinearRing({
    lazy val p = pts.next();
    Array(p, pts.next(), pts.next(), pts.next(), p)
  })).iterator
  lazy val pls: Iterator[Polygon] = LazyList.continually(new Polygon(lras.next())).iterator

  // Streams of arrays of random geometry values
  lazy val ptas: Iterator[Array[Point]] = LazyList.continually(Array(pts.next(), pts.next(), pts.next())).iterator
  lazy val plas: Iterator[Array[Polygon]] = LazyList.continually(Array(pls.next(), pls.next(), pls.next())).iterator
  lazy val lsas: Iterator[Array[LineString]] = LazyList.continually(Array(lss.next(), lss.next(), lss.next())).iterator
  lazy val lras: Iterator[Array[LinearRing]] = LazyList.continually(Array(lrs.next(), lrs.next(), lrs.next())).iterator

  // All these types map to `geometry`
  def testInOutGeom[A <: Geometry: Meta](a: A) =
    testInOut[A]("geometry", a)

  testInOutGeom[Geometry](pts.next())
  testInOutGeom[ComposedGeom](new MultiLineString(lsas.next()))
  testInOutGeom[GeometryCollection](new GeometryCollection(Array(pts.next(), lss.next())))
  testInOutGeom[MultiLineString](new MultiLineString(lsas.next()))
  testInOutGeom[MultiPolygon](new MultiPolygon(plas.next()))
  testInOutGeom[PointComposedGeom](lss.next())
  testInOutGeom[LineString](lss.next())
  testInOutGeom[MultiPoint](new MultiPoint(ptas.next()))
  testInOutGeom[Polygon](pls.next())
  testInOutGeom[Point](pts.next())

  // hstore
  testInOut[Map[String, String]]("hstore")
}
