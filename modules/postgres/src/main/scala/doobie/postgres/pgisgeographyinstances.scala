// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import doobie._
import doobie.util.invariant._
import net.postgis.jdbc.PGgeography
import net.postgis.jdbc.geometry._

import scala.reflect.ClassTag
import org.tpolecat.typename._

// Implicit conversions for postgis geography types
trait PgisGeographyInstances {

  // PostGIS outer types
  implicit val PGgeographyType: Meta[PGgeography] = Meta.Advanced.other[PGgeography]("geography")

  // Constructor for geometry types via the `Geometry` member of PGgeography
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Throw"))
  private def geometryType[A >: Null <: Geometry: TypeName](implicit A: ClassTag[A]): Meta[A] =
    PGgeographyType.timap[A](g =>
      try A.runtimeClass.cast(g.getGeometry).asInstanceOf[A]
      catch {
        case _: ClassCastException => throw InvalidObjectMapping(A.runtimeClass, g.getGeometry.getClass)
      })(new PGgeography(_))

  // PostGIS Geometry Types
  implicit val MultiLineStringType: Meta[MultiLineString] = geometryType[MultiLineString]
  implicit val MultiPolygonType: Meta[MultiPolygon] = geometryType[MultiPolygon]
  implicit val PointComposedGeomType: Meta[PointComposedGeom] = geometryType[PointComposedGeom]
  implicit val LineStringType: Meta[LineString] = geometryType[LineString]
  implicit val MultiPointType: Meta[MultiPoint] = geometryType[MultiPoint]
  implicit val PolygonType: Meta[Polygon] = geometryType[Polygon]
  implicit val PointType: Meta[Point] = geometryType[Point]
}
