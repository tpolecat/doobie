// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie

/** High-level database API. The constructors here are defined in terms of those in `doobie.free.connection` but differ
  * in the following ways:
  *
  *   - Enumerated values represented by `Int` values in JDBC are mapped to one of the proper types defined in
  *     `doobie.enumerated`.
  *   - Nullable values are represented in terms of `Option`.
  *   - Java collection types are translated to immutable Scala equivalents.
  *   - Actions that compute lifetime-managed resources do not return the resource directly, but rather take a
  *     continuation in the resource's monad.
  *   - Actions that compute values of impure types (`CLOB`, `InputStream`, etc.) do not appear in this API. They are
  *     available in the low-level API but must be used with considerable caution.
  *   - Lifting actions, low-level type mapping actions, and resource management actions do not appear in this API.
  */
package object hi
    extends Modules
    with doobie.free.Modules
    with doobie.free.Types {

  object implicits extends doobie.free.Instances

}
