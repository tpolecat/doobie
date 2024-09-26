// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie

/** Mixin containing aliases for the most commonly used types and modules from doobie-core. */
trait Aliases extends Types with Modules

/** Mixin containing aliases for the most commonly used types from doobie-core. */
trait Types {

  /** @group Type Aliases - Core */
  type Meta[A] = doobie.util.meta.Meta[A]

  /** @group Type Aliases - Core */
  type Query[A, B] = doobie.util.query.Query[A, B]

  /** @group Type Aliases - Core */
  type Update[A] = doobie.util.update.Update[A]

  /** @group Type Aliases - Core */
  type Query0[A] = doobie.util.query.Query0[A]

  /** @group Type Aliases - Core */
  type Update0 = doobie.util.update.Update0

  /** @group Type Aliases - Core */
  type SqlState = doobie.enumerated.SqlState

  /** @group Type Aliases - Core */
  type Transactor[M[_]] = doobie.util.transactor.Transactor[M]

  /** @group Type Aliases - Core */
  type LogHandler[M[_]] = doobie.util.log.LogHandler[M]

  /** @group Type Aliases - Core */
  type Fragment = doobie.util.fragment.Fragment

  /** @group Type Aliases - Core */
  type KleisliInterpreter[F[_]] = doobie.free.KleisliInterpreter[F]

  /** @group Type Aliases - Core */
  type DataSourceTransactor[F[_]] = doobie.util.transactor.Transactor.Aux[F, javax.sql.DataSource]
}

/** Mixin containing aliases for the most commonly used modules from doobie-core. */
trait Modules {

  /** @group Module Aliases - Core */
  val Meta = doobie.util.meta.Meta

  /** @group Module Aliases - Core */
  val Query = doobie.util.query.Query

  /** @group Module Aliases - Core */
  val Update = doobie.util.update.Update

  /** @group Module Aliases - Core */
  val Query0 = doobie.util.query.Query0

  /** @group Module Aliases - Core */
  val Update0 = doobie.util.update.Update0

  /** @group Module Aliases - Core */
  val SqlState = doobie.enumerated.SqlState

  /** @group Module Aliases - Core */
  val Transactor = doobie.util.transactor.Transactor

  /** @group Module Aliases - Core */
  val LogHandler = doobie.util.log.LogHandler

  /** @group Module Aliases - Core */
  val Fragment = doobie.util.fragment.Fragment

  /** @group Module Aliases - Core */
  val KleisliInterpreter = doobie.free.KleisliInterpreter

  /** @group Module Aliases - Core */
  val Fragments = doobie.util.fragments

  /** @group Module Aliases - Core */
  val ExecutionContexts = doobie.util.ExecutionContexts
}
