// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.hikari

import java.util.Properties
import java.util.concurrent.{ScheduledExecutorService, ThreadFactory, TimeUnit}

import cats.effect.Sync
import cats.syntax.show._
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.metrics.MetricsTrackerFactory
import doobie.enumerated.TransactionIsolation
import javax.sql.DataSource

import scala.annotation.nowarn
import scala.concurrent.duration.Duration

/** Configuration case class, susceptible to PureConfig. Helps with creating `com.zaxxer.hikari.HikariConfig`, which in
  * turn is used to create `doobie.hikari.HikariTransactor`. See the method `HikariTransactor.fromConfigAutoEc`
  */
// Whenever you add a new field to maintain backward compatibility:
//  * add the field to `copy`
//  * create a new `apply`
//  * add a new case to `fromProduct`
//
// The default values in the constructor are not actually applied (defaults from `apply` are).
// But they still need to be present to enable tools like PureConfig.
@nowarn("msg=(never used|unused)")
final case class Config private (
    jdbcUrl: String,
    catalog: Option[String] = None,
    connectionTimeout: Duration = Duration(30, TimeUnit.SECONDS),
    idleTimeout: Duration = Duration(10, TimeUnit.MINUTES),
    leakDetectionThreshold: Duration = Duration.Zero,
    maximumPoolSize: Int = 10,
    maxLifetime: Duration = Duration(30, TimeUnit.MINUTES),
    minimumIdle: Int = 10,
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
    readOnly: Boolean = false,
    registerMbeans: Boolean = false,
    schema: Option[String] = None,
    transactionIsolation: Option[TransactionIsolation] = None
) {
  @nowarn("msg=(never used|unused)")
  private def copy(
      jdbcUrl: String,
      catalog: Option[String],
      connectionTimeout: Duration,
      idleTimeout: Duration,
      leakDetectionThreshold: Duration,
      maximumPoolSize: Int,
      maxLifetime: Duration,
      minimumIdle: Int,
      password: Option[String],
      poolName: Option[String],
      username: Option[String],
      validationTimeout: Duration,
      allowPoolSuspension: Boolean,
      autoCommit: Boolean,
      connectionInitSql: Option[String],
      connectionTestQuery: Option[String],
      dataSourceClassName: Option[String],
      dataSourceJNDI: Option[String],
      driverClassName: Option[String],
      initializationFailTimeout: Duration,
      isolateInternalQueries: Boolean,
      readOnly: Boolean,
      registerMbeans: Boolean,
      schema: Option[String],
      transactionIsolation: Option[TransactionIsolation]
  ): Any = this
}

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
      threadFactory: Option[ThreadFactory] = None
  )(implicit F: Sync[F]): F[HikariConfig] =
    F.delay {
      val c = new HikariConfig()

      c.setJdbcUrl(config.jdbcUrl)

      config.catalog.foreach(c.setCatalog)
      c.setConnectionTimeout(config.connectionTimeout.toMillis)
      c.setIdleTimeout(config.idleTimeout.toMillis)
      c.setLeakDetectionThreshold(config.leakDetectionThreshold.toMillis)
      c.setMaximumPoolSize(config.maximumPoolSize)
      c.setMaxLifetime(config.maxLifetime.toMillis)
      c.setMinimumIdle(config.minimumIdle)
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

  def apply(
      jdbcUrl: String,
      catalog: Option[String] = None,
      connectionTimeout: Duration = Duration(30, TimeUnit.SECONDS),
      idleTimeout: Duration = Duration(10, TimeUnit.MINUTES),
      leakDetectionThreshold: Duration = Duration.Zero,
      maximumPoolSize: Int = 10,
      maxLifetime: Duration = Duration(30, TimeUnit.MINUTES),
      minimumIdle: Int = 10,
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
      readOnly: Boolean = false,
      registerMbeans: Boolean = false,
      schema: Option[String] = None,
      transactionIsolation: Option[TransactionIsolation] = None
  ): Config = new Config(
    jdbcUrl = jdbcUrl,
    catalog = catalog,
    connectionTimeout = connectionTimeout,
    idleTimeout = idleTimeout,
    leakDetectionThreshold = leakDetectionThreshold,
    maximumPoolSize = maximumPoolSize,
    maxLifetime = maxLifetime,
    minimumIdle = minimumIdle,
    password = password,
    poolName = poolName,
    username = username,
    validationTimeout = validationTimeout,
    allowPoolSuspension = allowPoolSuspension,
    autoCommit = autoCommit,
    connectionInitSql = connectionInitSql,
    connectionTestQuery = connectionTestQuery,
    dataSourceClassName = dataSourceClassName,
    dataSourceJNDI = dataSourceJNDI,
    driverClassName = driverClassName,
    initializationFailTimeout = initializationFailTimeout,
    isolateInternalQueries = isolateInternalQueries,
    readOnly = readOnly,
    registerMbeans = registerMbeans,
    schema = schema,
    transactionIsolation = transactionIsolation
  )

  @nowarn
  def fromProduct(p: Product): Config = p.productArity match {
    case 25 =>
      Config(
        p.productElement(0).asInstanceOf[String],
        p.productElement(1).asInstanceOf[Option[String]],
        p.productElement(2).asInstanceOf[Duration],
        p.productElement(3).asInstanceOf[Duration],
        p.productElement(4).asInstanceOf[Duration],
        p.productElement(5).asInstanceOf[Int],
        p.productElement(6).asInstanceOf[Duration],
        p.productElement(7).asInstanceOf[Int],
        p.productElement(8).asInstanceOf[Option[String]],
        p.productElement(9).asInstanceOf[Option[String]],
        p.productElement(10).asInstanceOf[Option[String]],
        p.productElement(11).asInstanceOf[Duration],
        p.productElement(12).asInstanceOf[Boolean],
        p.productElement(13).asInstanceOf[Boolean],
        p.productElement(14).asInstanceOf[Option[String]],
        p.productElement(15).asInstanceOf[Option[String]],
        p.productElement(16).asInstanceOf[Option[String]],
        p.productElement(17).asInstanceOf[Option[String]],
        p.productElement(18).asInstanceOf[Option[String]],
        p.productElement(19).asInstanceOf[Duration],
        p.productElement(20).asInstanceOf[Boolean],
        p.productElement(21).asInstanceOf[Boolean],
        p.productElement(22).asInstanceOf[Boolean],
        p.productElement(23).asInstanceOf[Option[String]],
        p.productElement(24).asInstanceOf[Option[TransactionIsolation]]
      )
  }

  @nowarn("msg=(never used|unused)")
  private def unapply(c: Config): Any = this
}
