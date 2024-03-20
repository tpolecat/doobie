// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import doobie._
import doobie.util.invariant._
import net.postgis.jdbc.{PGbox2d, PGbox3d, PGgeometry}
import net.postgis.jdbc.geometry._

import scala.reflect.ClassTag
import org.tpolecat.typename._

trait PgisInstances {

  // PostGIS outer types
  implicit val PGgeometryType: Meta[PGgeometry] = Meta.Advanced.other[PGgeometry]("geometry")
  implicit val PGbox3dType: Meta[PGbox3d] = Meta.Advanced.other[PGbox3d]("box3d")
  implicit val PGbox2dType: Meta[PGbox2d] = Meta.Advanced.other[PGbox2d]("box2d")

  // Constructor for geometry types via the `Geometry` member of PGgeometry
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Throw"))
  private def geometryType[A >: Null <: Geometry: TypeName](implicit A: ClassTag[A]): Meta[A] =
    PGgeometryType.timap[A](g =>
      try A.runtimeClass.cast(g.getGeometry).asInstanceOf[A]
      catch {
        case _: ClassCastException => throw InvalidObjectMapping(A.runtimeClass, g.getGeometry.getClass)
      })(new PGgeometry(_))

  // PostGIS Geometry Types
  implicit val GeometryType: Meta[Geometry] = geometryType[Geometry]
  implicit val ComposedGeomType: Meta[ComposedGeom] = geometryType[ComposedGeom]
  implicit val GeometryCollectionType: Meta[GeometryCollection] = geometryType[GeometryCollection]
  implicit val MultiLineStringType: Meta[MultiLineString] = geometryType[MultiLineString]
  implicit val MultiPolygonType: Meta[MultiPolygon] = geometryType[MultiPolygon]
  implicit val PointComposedGeomType: Meta[PointComposedGeom] = geometryType[PointComposedGeom]
  implicit val LineStringType: Meta[LineString] = geometryType[LineString]
  implicit val MultiPointType: Meta[MultiPoint] = geometryType[MultiPoint]
  implicit val PolygonType: Meta[Polygon] = geometryType[Polygon]
  implicit val PointType: Meta[Point] = geometryType[Point]
}
