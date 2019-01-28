// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import doobie._
import doobie.util.invariant._

import org.postgis._

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

@SuppressWarnings(Array("org.wartremover.warts.ExplicitImplicitTypes"))
trait PgisInstances {

  // PostGIS outer types
  implicit val PGgeometryType = Meta.Advanced.other[PGgeometry]("geometry")
  implicit val PGbox3dType    = Meta.Advanced.other[PGbox3d]("box3d")
  implicit val PGbox2dType    = Meta.Advanced.other[PGbox2d]("box2d")

  // Constructor for geometry types via the `Geometry` member of PGgeometry
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  private def geometryType[A >: Null <: Geometry: TypeTag](implicit A: ClassTag[A]): Meta[A] =
    PGgeometryType.timap[A](g =>
      try A.runtimeClass.cast(g.getGeometry).asInstanceOf[A]
      catch {
        case _: ClassCastException => throw InvalidObjectMapping(A.runtimeClass, g.getGeometry.getClass)
      })(new PGgeometry(_))

  // PostGIS Geometry Types
  implicit val GeometryType           = geometryType[Geometry]
  implicit val ComposedGeomType       = geometryType[ComposedGeom]
  implicit val GeometryCollectionType = geometryType[GeometryCollection]
  implicit val MultiLineStringType    = geometryType[MultiLineString]
  implicit val MultiPolygonType       = geometryType[MultiPolygon]
  implicit val PointComposedGeomType  = geometryType[PointComposedGeom]
  implicit val LineStringType         = geometryType[LineString]
  implicit val MultiPointType         = geometryType[MultiPoint]
  implicit val PolygonType            = geometryType[Polygon]
  implicit val PointType              = geometryType[Point]
}
