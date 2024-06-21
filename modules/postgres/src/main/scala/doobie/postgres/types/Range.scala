// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres.types

import cats.Monoid
import cats.implicits.toBifunctorOps
import doobie.postgres.types.Range.Edge
import doobie.postgres.types.Range.Edge._
import doobie.util.invariant.InvalidValue

import scala.util.Try

/*
  Range Input/Output
  The input for a range value must follow one of the following patterns:

  (lower-bound,upper-bound)
  (lower-bound,upper-bound]
  [lower-bound,upper-bound)
  [lower-bound,upper-bound]
  empty
 */
sealed trait Range[+T]

case object EmptyRange extends Range[Nothing]

case class NonEmptyRange[T](lowerBound: Option[T], upperBound: Option[T], edge: Edge) extends Range[T]

object Range {
  sealed trait Edge

  object Edge {
    case object ExclExcl extends Edge
    case object ExclIncl extends Edge
    case object InclExcl extends Edge
    case object InclIncl extends Edge
  }

  def empty[T]: Range[T] = EmptyRange

  def apply[T](start: T, end: T, edge: Edge = InclExcl): Range[T] = NonEmptyRange(Some(start), Some(end), edge)

  def encode[T](range: Range[T])(E: T => String): String = {

    val encodeBound: Option[T] => String = o =>
      o.map(E).getOrElse(Monoid[String].empty)

    range match {
      case NonEmptyRange(lowerBound, upperBound, edge) => edge match {
          case InclExcl => s"[${encodeBound(lowerBound)},${encodeBound(upperBound)})"
          case ExclIncl => s"(${encodeBound(lowerBound)},${encodeBound(upperBound)}]"
          case ExclExcl => s"(${encodeBound(lowerBound)},${encodeBound(upperBound)})"
          case InclIncl => s"[${encodeBound(lowerBound)},${encodeBound(upperBound)}]"
        }

      case EmptyRange => EmptyRangeStr
    }
  }

  def decode[T](range: String)(D: String => T): Either[InvalidValue[String, Range[T]], Range[T]] = {

    def decodeRange(start: String, end: String, edge: Edge) = {
      val decodeBound: String => Either[InvalidValue[String, Range[T]], Option[T]] = s =>
        Try(Option(s).filter(_.nonEmpty).map(D))
          .toEither
          .leftMap { error =>
            InvalidValue(value = range, reason = Option(error.getMessage).getOrElse("unknown reason"))
          }

      for {
        start <- decodeBound(start)
        end <- decodeBound(end)
      } yield NonEmptyRange[T](start, end, edge)
    }

    range match {
      case InclExclRange(start, end) => decodeRange(start, end, InclExcl)
      case ExclInclRange(start, end) => decodeRange(start, end, ExclIncl)
      case ExclExclRange(start, end) => decodeRange(start, end, ExclExcl)
      case InclInclRange(start, end) => decodeRange(start, end, InclIncl)
      case EmptyRangeStr             => Right(Range.empty)
      case _ => Left(InvalidValue(value = range, reason = "the value does not conform to the range type"))
    }
  }

  private val EmptyRangeStr = "empty"
  private val InclExclRange = """\["?([^,"]*)"?,[ ]*"?([^,"]*)"?\)""".r // [_,_)
  private val ExclInclRange = """\("?([^,"]*)"?,[ ]*"?([^,"]*)"?\]""".r // (_,_]
  private val ExclExclRange = """\("?([^,"]*)"?,[ ]*"?([^,"]*)"?\)""".r // (_,_)
  private val InclInclRange = """\["?([^,"]*)"?,[ ]*"?([^,"]*)"?\]""".r // [_,_]
}
