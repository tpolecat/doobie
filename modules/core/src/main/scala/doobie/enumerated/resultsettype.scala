// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.enumerated

import doobie.util.invariant._

import java.sql.ResultSet._

import cats.ApplicativeError
import cats.kernel.Eq
import cats.kernel.instances.int._

/** @group Types */
sealed abstract class ResultSetType(val toInt: Int) extends Product with Serializable

/** @group Modules */
object ResultSetType {

  /** @group Values */
  case object TypeForwardOnly extends ResultSetType(TYPE_FORWARD_ONLY)

  /** @group Values */
  case object TypeScrollInsensitive extends ResultSetType(TYPE_SCROLL_INSENSITIVE)

  /** @group Values */
  case object TypeScrollSensitive extends ResultSetType(TYPE_SCROLL_SENSITIVE)

  def fromInt(n: Int): Option[ResultSetType] =
    Some(n) collect {
      case TypeForwardOnly.toInt       => TypeForwardOnly
      case TypeScrollInsensitive.toInt => TypeScrollInsensitive
      case TypeScrollSensitive.toInt   => TypeScrollSensitive
    }

  def fromIntF[F[_]](n: Int)(implicit AE: ApplicativeError[F, Throwable]): F[ResultSetType] =
    ApplicativeError.liftFromOption(fromInt(n), InvalidOrdinal[ResultSetType](n))

  implicit val EqResultSetType: Eq[ResultSetType] =
    Eq.by(_.toInt)
}
