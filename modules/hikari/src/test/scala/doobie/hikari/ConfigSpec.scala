// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.hikari

import cats.effect.IO
import com.zaxxer.hikari.HikariConfig
import org.specs2.mutable.Specification

class ConfigSpec extends Specification {

  "Default should be the same as unmodified HikariConfig" in {

    val actual = Config.makeHikariConfig[IO](Config()).unsafeRunSync()
    val expected = new HikariConfig()

    actual.getCatalog must_=== (expected.getCatalog)
    actual.getConnectionTimeout must_=== (expected.getConnectionTimeout)
    actual.getIdleTimeout must_=== (expected.getIdleTimeout)
    actual.getLeakDetectionThreshold must_=== (expected.getLeakDetectionThreshold)
    actual.getMaximumPoolSize must_=== (expected.getMaximumPoolSize)
    actual.getMaxLifetime must_=== (expected.getMaxLifetime)
    actual.getMinimumIdle must_=== (expected.getMinimumIdle)
    actual.getPassword must_=== (expected.getPassword)
    actual.getPoolName must_=== (expected.getPoolName)
    actual.getUsername must_=== (expected.getUsername)
    actual.getValidationTimeout must_=== (expected.getValidationTimeout)

    actual.isAllowPoolSuspension must_=== (expected.isAllowPoolSuspension)
    actual.isAutoCommit must_=== (expected.isAutoCommit)
    actual.getConnectionInitSql must_=== (expected.getConnectionInitSql)
    actual.getConnectionTestQuery must_=== (expected.getConnectionTestQuery)
    actual.getDataSourceClassName must_=== (expected.getDataSourceClassName)
    actual.getDataSourceJNDI must_=== (expected.getDataSourceJNDI)
    actual.getDriverClassName must_=== (expected.getDriverClassName)
    actual.getInitializationFailTimeout must_=== (expected.getInitializationFailTimeout)
    actual.isIsolateInternalQueries must_=== (expected.isIsolateInternalQueries)
    actual.getJdbcUrl must_=== (expected.getJdbcUrl)
    actual.isReadOnly must_=== (expected.isReadOnly)
    actual.isRegisterMbeans must_=== (expected.isRegisterMbeans)
    actual.getSchema must_=== (expected.getSchema)
    actual.getTransactionIsolation must_=== (expected.getTransactionIsolation)

    actual.getDataSource must_=== (expected.getDataSource)
    actual.getDataSourceProperties must_=== (expected.getDataSourceProperties)
    actual.getHealthCheckProperties must_=== (expected.getHealthCheckProperties)
    actual.getHealthCheckRegistry must_=== (expected.getHealthCheckRegistry)
    actual.getMetricRegistry must_=== (expected.getMetricRegistry)
    actual.getMetricsTrackerFactory must_=== (expected.getMetricsTrackerFactory)
    actual.getScheduledExecutor must_=== (expected.getScheduledExecutor)
    actual.getThreadFactory must_=== (expected.getThreadFactory)

  }
}
