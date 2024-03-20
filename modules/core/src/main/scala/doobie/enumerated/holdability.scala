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
sealed abstract class Holdability(val toInt: Int) extends Product with Serializable

/** @group Modules */
object Holdability {

  /** @group Values */
  case object HoldCursorsOverCommit extends Holdability(HOLD_CURSORS_OVER_COMMIT)

  /** @group Values */
  case object CloseCursorsAtCommit extends Holdability(CLOSE_CURSORS_AT_COMMIT)

  def fromInt(n: Int): Option[Holdability] =
    Some(n) collect {
      case HoldCursorsOverCommit.toInt => HoldCursorsOverCommit
      case CloseCursorsAtCommit.toInt  => CloseCursorsAtCommit
    }

  def fromIntF[F[_]](n: Int)(implicit AE: ApplicativeError[F, Throwable]): F[Holdability] =
    ApplicativeError.liftFromOption(fromInt(n), InvalidOrdinal[Holdability](n))

  implicit val EqHoldability: Eq[Holdability] =
    Eq.by(_.toInt)

}
