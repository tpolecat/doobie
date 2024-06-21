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
sealed abstract class ResultSetConcurrency(val toInt: Int) extends Product with Serializable

/** @group Modules */
object ResultSetConcurrency {

  /** @group Values */
  case object ConcurReadOnly extends ResultSetConcurrency(CONCUR_READ_ONLY)

  /** @group Values */
  case object ConcurUpdatable extends ResultSetConcurrency(CONCUR_UPDATABLE)

  def fromInt(n: Int): Option[ResultSetConcurrency] =
    Some(n) collect {
      case ConcurReadOnly.toInt  => ConcurReadOnly
      case ConcurUpdatable.toInt => ConcurUpdatable
    }

  def fromIntF[F[_]](n: Int)(implicit AE: ApplicativeError[F, Throwable]): F[ResultSetConcurrency] =
    ApplicativeError.liftFromOption(fromInt(n), InvalidOrdinal[ResultSetConcurrency](n))

  implicit val EqResultSetConcurrency: Eq[ResultSetConcurrency] =
    Eq.by(_.toInt)

}
