// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie

package object postgres
  extends postgres.free.Types
     with postgres.free.Modules
     with postgres.hi.Modules {

  object implicits
    extends Instances
       with free.Instances
       with syntax.ToPostgresMonadErrorOps
       with syntax.ToFragmentOps
       with syntax.ToPostgresExplainOps

  object rangeimplicits
    extends RangeInstances

  object pgisimplicits
    extends PgisInstances

  object pgisgeographyimplicits
    extends PgisGeographyInstances
}
