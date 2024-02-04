// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.enumerated

import doobie.util.invariant._

import java.sql.Connection._

import cats.{ApplicativeError, Show}
import cats.kernel.Eq
import cats.kernel.instances.int._

/** @group Types */
sealed abstract class TransactionIsolation(val toInt: Int) extends Product with Serializable

/** @group Modules */
object TransactionIsolation {

  /** @group Values */
  case object TransactionNone extends TransactionIsolation(TRANSACTION_NONE)

  /** @group Values */
  case object TransactionReadUncommitted extends TransactionIsolation(TRANSACTION_READ_UNCOMMITTED)

  /** @group Values */
  case object TransactionReadCommitted extends TransactionIsolation(TRANSACTION_READ_COMMITTED)

  /** @group Values */
  case object TransactionRepeatableRead extends TransactionIsolation(TRANSACTION_REPEATABLE_READ)

  /** @group Values */
  case object TransactionSerializable extends TransactionIsolation(TRANSACTION_SERIALIZABLE)

  def fromInt(n: Int): Option[TransactionIsolation] =
    Some(n) collect {
      case TransactionNone.toInt            => TransactionNone
      case TransactionReadUncommitted.toInt => TransactionReadUncommitted
      case TransactionReadCommitted.toInt   => TransactionReadCommitted
      case TransactionRepeatableRead.toInt  => TransactionRepeatableRead
      case TransactionSerializable.toInt    => TransactionSerializable
    }

  def fromIntF[F[_]](n: Int)(implicit AE: ApplicativeError[F, Throwable]): F[TransactionIsolation] =
    ApplicativeError.liftFromOption(fromInt(n), InvalidOrdinal[TransactionIsolation](n))

  implicit val EqTransactionIsolation: Eq[TransactionIsolation] =
    Eq.by(_.toInt)

  implicit val TransactionIsolationShow: Show[TransactionIsolation] = {
    case TransactionIsolation.TransactionNone => "TRANSACTION_NONE"
    case TransactionIsolation.TransactionReadUncommitted =>
      "TRANSACTION_READ_UNCOMMITTED"
    case TransactionIsolation.TransactionReadCommitted =>
      "TRANSACTION_READ_COMMITTED"
    case TransactionIsolation.TransactionRepeatableRead =>
      "TRANSACTION_REPEATABLE_READ"
    case TransactionIsolation.TransactionSerializable =>
      "TRANSACTION_SERIALIZABLE"
  }

}
