// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.enumerated

import cats.kernel.Eq

/** Generic nullability that subsumes JDBC's distinct parameter and column nullability.
  * @group Types
  */
sealed abstract class Nullability extends Product with Serializable {

  def toParameterNullable: ParameterNullable =
    ParameterNullable.fromNullability(this)

  def toColumnNullable: ColumnNullable =
    ColumnNullable.fromNullability(this)

}

/** @group Modules */
object Nullability {

  sealed abstract class NullabilityKnown extends Nullability

  /** @group Values */
  case object NoNulls extends NullabilityKnown

  /** @group Values */
  case object Nullable extends NullabilityKnown

  /** @group Values */
  case object NullableUnknown extends Nullability

  def fromBoolean(b: Boolean): Nullability =
    if (b) Nullable else NoNulls

  def fromParameterNullable(pn: ParameterNullable): Nullability =
    pn match {
      case ParameterNullable.NoNulls         => NoNulls
      case ParameterNullable.Nullable        => Nullable
      case ParameterNullable.NullableUnknown => NullableUnknown
    }

  def fromColumnNullable(pn: ColumnNullable): Nullability =
    pn match {
      case ColumnNullable.NoNulls         => NoNulls
      case ColumnNullable.Nullable        => Nullable
      case ColumnNullable.NullableUnknown => NullableUnknown
    }

  implicit val EqNullability: Eq[Nullability] =
    Eq.fromUniversalEquals

}
