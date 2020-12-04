// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.hikari

import java.util.Properties
import java.util.concurrent.{ScheduledExecutorService, ThreadFactory, TimeUnit}

import cats.effect.Sync
import cats.syntax.show._
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.metrics.MetricsTrackerFactory
import doobie.`enum`.TransactionIsolation
import javax.sql.DataSource

import scala.concurrent.duration.Duration

/** Configuration case class, susceptible to PureConfig.
  * Helps with creating `com.zaxxer.hikari.HikariConfig`,
  * which in turn is used to create `doobie.hikari.HikariTransactor`.
  * See the method `HikariTransactor.fromConfig` */
final case class Config(
  catalog: Option[String] = None,
  connectionTimeout: Duration = Duration(30, TimeUnit.SECONDS),
  idleTimeout: Duration = Duration(10, TimeUnit.MINUTES),
  leakDetectionThreshold: Duration = Duration.Zero,
  maximumPoolSize: Option[Int] = None,
  maxLifetime: Duration = Duration(30, TimeUnit.MINUTES),
  minimumIdle: Option[Int] = None,
  password: Option[String] = None,
  poolName: Option[String] = None,
  username: Option[String] = None,
  validationTimeout: Duration = Duration(5, TimeUnit.SECONDS),
  allowPoolSuspension: Boolean = false,
  autoCommit: Boolean = true,
  connectionInitSql: Option[String] = None,
  connectionTestQuery: Option[String] = None,
  dataSourceClassName: Option[String] = None,
  dataSourceJNDI: Option[String] = None,
  driverClassName: Option[String] = None,
  initializationFailTimeout: Duration = Duration(1, TimeUnit.MILLISECONDS),
  isolateInternalQueries: Boolean = false,
  jdbcUrl: Option[String] = None,
  readOnly: Boolean = false,
  registerMbeans: Boolean = false,
  schema: Option[String] = None,
  transactionIsolation: Option[TransactionIsolation] = None,
)

object Config {
  def makeHikariConfig[F[_]](
    config: Config,
    dataSource: Option[DataSource] = None,
    dataSourceProperties: Option[Properties] = None,
    healthCheckProperties: Option[Properties] = None,
    healthCheckRegistry: Option[Object] = None,
    metricRegistry: Option[Object] = None,
    metricsTrackerFactory: Option[MetricsTrackerFactory] = None,
    scheduledExecutor: Option[ScheduledExecutorService] = None,
    threadFactory: Option[ThreadFactory] = None,
  )(implicit F: Sync[F]): F[HikariConfig] =
    F.delay {
      val c = new HikariConfig()

      config.catalog.foreach(c.setCatalog)
      c.setConnectionTimeout(config.connectionTimeout.toMillis)
      c.setIdleTimeout(config.idleTimeout.toMillis)
      c.setLeakDetectionThreshold(config.leakDetectionThreshold.toMillis)
      config.maximumPoolSize.foreach(c.setMaximumPoolSize)
      c.setMaxLifetime(config.maxLifetime.toMillis)
      config.minimumIdle.foreach(c.setMinimumIdle)
      config.password.foreach(c.setPassword)
      config.poolName.foreach(c.setPoolName)
      config.username.foreach(c.setUsername)
      c.setValidationTimeout(config.validationTimeout.toMillis)

      c.setAllowPoolSuspension(config.allowPoolSuspension)
      c.setAutoCommit(config.autoCommit)
      config.connectionInitSql.foreach(c.setConnectionInitSql)
      config.connectionTestQuery.foreach(c.setConnectionTestQuery)
      config.dataSourceClassName.foreach(c.setDataSourceClassName)
      config.dataSourceJNDI.foreach(c.setDataSourceJNDI)
      config.driverClassName.foreach(c.setDriverClassName)
      c.setInitializationFailTimeout(config.initializationFailTimeout.toMillis)
      c.setIsolateInternalQueries(config.isolateInternalQueries)
      config.jdbcUrl.foreach(c.setJdbcUrl)
      c.setReadOnly(config.readOnly)
      c.setRegisterMbeans(config.registerMbeans)
      config.schema.foreach(c.setSchema)
      config.transactionIsolation.map(_.show).foreach(c.setTransactionIsolation)

      dataSource.foreach(c.setDataSource)
      dataSourceProperties.foreach(c.setDataSourceProperties)
      healthCheckProperties.foreach(c.setHealthCheckProperties)
      healthCheckRegistry.foreach(c.setHealthCheckRegistry)
      metricRegistry.foreach(c.setMetricRegistry)
      metricsTrackerFactory.foreach(c.setMetricsTrackerFactory)
      scheduledExecutor.foreach(c.setScheduledExecutor)
      threadFactory.foreach(c.setThreadFactory)

      c
    }

}
