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
sealed abstract class FetchDirection(val toInt: Int) extends Product with Serializable

/** @group Modules */
object FetchDirection {

  /** @group Values */
  case object Forward extends FetchDirection(FETCH_FORWARD)

  /** @group Values */
  case object Reverse extends FetchDirection(FETCH_REVERSE)

  /** @group Values */
  case object Unknown extends FetchDirection(FETCH_UNKNOWN)

  def fromInt(n: Int): Option[FetchDirection] =
    Some(n) collect {
      case Forward.toInt => Forward
      case Reverse.toInt => Reverse
      case Unknown.toInt => Unknown
    }

  def fromIntF[F[_]](n: Int)(implicit AE: ApplicativeError[F, Throwable]): F[FetchDirection] =
    ApplicativeError.liftFromOption(fromInt(n), InvalidOrdinal[FetchDirection](n))

  implicit val EqFetchDirection: Eq[FetchDirection] =
    Eq.by(_.toInt)

}
