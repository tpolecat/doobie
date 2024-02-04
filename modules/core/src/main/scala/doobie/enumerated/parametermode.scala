// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.enumerated

import doobie.util.invariant._

import java.sql.ParameterMetaData._

import cats.ApplicativeError
import cats.kernel.Eq
import cats.kernel.instances.int._

/** @group Types */
sealed abstract class ParameterMode(val toInt: Int) extends Product with Serializable

/** @group Modules */
object ParameterMode {

  /** @group Values */
  case object ModeIn extends ParameterMode(parameterModeIn)

  /** @group Values */
  case object ModeOut extends ParameterMode(parameterModeOut)

  /** @group Values */
  case object ModeInOut extends ParameterMode(parameterModeInOut)

  /** @group Values */
  case object ModeUnknown extends ParameterMode(parameterModeUnknown)

  def fromInt(n: Int): Option[ParameterMode] =
    Some(n) collect {
      case ModeIn.toInt      => ModeIn
      case ModeOut.toInt     => ModeOut
      case ModeInOut.toInt   => ModeInOut
      case ModeUnknown.toInt => ModeUnknown
    }

  def fromIntF[F[_]](n: Int)(implicit AE: ApplicativeError[F, Throwable]): F[ParameterMode] =
    ApplicativeError.liftFromOption(fromInt(n), InvalidOrdinal[ParameterMode](n))

  implicit val EqParameterMode: Eq[ParameterMode] =
    Eq.by(_.toInt)
}
