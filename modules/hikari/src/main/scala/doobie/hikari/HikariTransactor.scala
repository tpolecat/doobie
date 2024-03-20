// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie
package hikari

import java.util.Properties
import java.util.concurrent.{ScheduledExecutorService, ThreadFactory}

import cats.effect.implicits._
import cats.effect.kernel.{Async, Resource, Sync}
import com.zaxxer.hikari.metrics.MetricsTrackerFactory
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import javax.sql.DataSource

import scala.concurrent.ExecutionContext

object HikariTransactor {

  /** Construct a `HikariTransactor` from an existing `HikariDatasource`. */
  def apply[M[_]] = new HikariTransactorPartiallyApplied[M]

  class HikariTransactorPartiallyApplied[M[_]] {
    def apply(
        hikariDataSource: HikariDataSource,
        connectEC: ExecutionContext,
        logHandler: Option[LogHandler[M]] = None
    )(implicit ev: Async[M]): HikariTransactor[M] = {
      Transactor.fromDataSource[M](hikariDataSource, connectEC, logHandler)
    }
  }

  /** Resource yielding an unconfigured `HikariTransactor`. */
  def initial[M[_]: Async](
      connectEC: ExecutionContext,
      logHandler: Option[LogHandler[M]] = None
  ): Resource[M, HikariTransactor[M]] = initialWithResEffect[M, M](connectEC, logHandler)

  /** Similar to [[initial]], but with a separate effect for the construction of the Transactor
    *
    * @tparam M0
    *   the effect to construct the [[HikariTransactor]] in
    * @tparam M
    *   the effect under which the [[HikariTransactor]] runs
    */
  def initialWithResEffect[M0[_]: Sync, M[_]: Async](
      connectEC: ExecutionContext,
      logHandler: Option[LogHandler[M]] = None
  ): Resource[M0, HikariTransactor[M]] = {
    Resource.fromAutoCloseable(Sync[M0].delay(new HikariDataSource))
      .map(Transactor.fromDataSource[M](_, connectEC, logHandler))
  }

  /** Resource yielding a new `HikariTransactor` configured with the given Config. Unless you have a good reason,
    * consider using `fromConfig` which creates the `connectEC` for you.
    */
  def fromConfigCustomEc[M[_]: Async](
      config: Config,
      connectEC: ExecutionContext,
      logHandler: Option[LogHandler[M]] = None,
      dataSource: Option[DataSource] = None,
      dataSourceProperties: Option[Properties] = None,
      healthCheckProperties: Option[Properties] = None,
      healthCheckRegistry: Option[Object] = None,
      metricRegistry: Option[Object] = None,
      metricsTrackerFactory: Option[MetricsTrackerFactory] = None,
      scheduledExecutor: Option[ScheduledExecutorService] = None,
      threadFactory: Option[ThreadFactory] = None
  ): Resource[M, HikariTransactor[M]] = {
    fromConfigCustomEcWithResEffect[M, M](
      config = config,
      connectEC = connectEC,
      logHandler = logHandler,
      dataSource = dataSource,
      dataSourceProperties = dataSourceProperties,
      healthCheckProperties = healthCheckProperties,
      healthCheckRegistry = healthCheckRegistry,
      metricRegistry = metricRegistry,
      metricsTrackerFactory = metricsTrackerFactory,
      scheduledExecutor = scheduledExecutor,
      threadFactory = threadFactory
    )
  }

  /** Similar to [[fromConfigCustomEc]], but with a separate effect for the construction of the Transactor
    *
    * @tparam M0
    *   the effect to construct the [[HikariTransactor]] in
    * @tparam M
    *   the effect under which the [[HikariTransactor]] runs
    */
  def fromConfigCustomEcWithResEffect[M0[_]: Sync, M[_]: Async](
      config: Config,
      connectEC: ExecutionContext,
      logHandler: Option[LogHandler[M]] = None,
      dataSource: Option[DataSource] = None,
      dataSourceProperties: Option[Properties] = None,
      healthCheckProperties: Option[Properties] = None,
      healthCheckRegistry: Option[Object] = None,
      metricRegistry: Option[Object] = None,
      metricsTrackerFactory: Option[MetricsTrackerFactory] = None,
      scheduledExecutor: Option[ScheduledExecutorService] = None,
      threadFactory: Option[ThreadFactory] = None
  ): Resource[M0, HikariTransactor[M]] = {
    Resource
      .liftK(
        Config.makeHikariConfig[M0](
          config = config,
          dataSource = dataSource,
          dataSourceProperties = dataSourceProperties,
          healthCheckProperties = healthCheckProperties,
          healthCheckRegistry = healthCheckRegistry,
          metricRegistry = metricRegistry,
          metricsTrackerFactory = metricsTrackerFactory,
          scheduledExecutor = scheduledExecutor,
          threadFactory = threadFactory
        )
      )
      .flatMap(fromHikariConfigCustomEcWithResEffect[M0, M](_, connectEC, logHandler))
  }

  /** Resource yielding a new `HikariTransactor` configured with the given Config. The `connectEC` is created
    * automatically, with the same size as the Hikari pool.
    *
    * @tparam M
    *   the effect under which the [[HikariTransactor]] runs
    */
  def fromConfig[M[_]: Async](
      config: Config,
      logHandler: Option[LogHandler[M]] = None,
      dataSource: Option[DataSource] = None,
      dataSourceProperties: Option[Properties] = None,
      healthCheckProperties: Option[Properties] = None,
      healthCheckRegistry: Option[Object] = None,
      metricRegistry: Option[Object] = None,
      metricsTrackerFactory: Option[MetricsTrackerFactory] = None,
      scheduledExecutor: Option[ScheduledExecutorService] = None,
      threadFactory: Option[ThreadFactory] = None
  ): Resource[M, HikariTransactor[M]] = {
    fromConfigWithResEffect[M, M](
      config = config,
      logHandler = logHandler,
      dataSource = dataSource,
      dataSourceProperties = dataSourceProperties,
      healthCheckProperties = healthCheckProperties,
      healthCheckRegistry = healthCheckRegistry,
      metricRegistry = metricRegistry,
      metricsTrackerFactory = metricsTrackerFactory,
      scheduledExecutor = scheduledExecutor,
      threadFactory = threadFactory
    )
  }

  /** Similar to [[fromConfig]], but with a separate effect for the construction of the Transactor
    *
    * @tparam M0
    *   the effect to construct the [[HikariTransactor]] in
    * @tparam M
    *   the effect under which the [[HikariTransactor]] runs
    */
  def fromConfigWithResEffect[M0[_]: Sync, M[_]: Async](
      config: Config,
      logHandler: Option[LogHandler[M]] = None,
      dataSource: Option[DataSource] = None,
      dataSourceProperties: Option[Properties] = None,
      healthCheckProperties: Option[Properties] = None,
      healthCheckRegistry: Option[Object] = None,
      metricRegistry: Option[Object] = None,
      metricsTrackerFactory: Option[MetricsTrackerFactory] = None,
      scheduledExecutor: Option[ScheduledExecutorService] = None,
      threadFactory: Option[ThreadFactory] = None
  ): Resource[M0, HikariTransactor[M]] = {
    Resource
      .liftK(
        Config.makeHikariConfig[M0](
          config = config,
          dataSource = dataSource,
          dataSourceProperties = dataSourceProperties,
          healthCheckProperties = healthCheckProperties,
          healthCheckRegistry = healthCheckRegistry,
          metricRegistry = metricRegistry,
          metricsTrackerFactory = metricsTrackerFactory,
          scheduledExecutor = scheduledExecutor,
          threadFactory = threadFactory
        )
      )
      .flatMap(fromHikariConfigWithResEffect[M0, M](_, logHandler))
  }

  /** Resource yielding a new `HikariTransactor` configured with the given HikariConfig. Unless you have a good reason,
    * consider using [[fromHikariConfig]], it will be created automatically for you.
    */
  def fromHikariConfigCustomEc[M[_]: Async](
      hikariConfig: HikariConfig,
      connectEC: ExecutionContext,
      logHandler: Option[LogHandler[M]] = None
  ): Resource[M, HikariTransactor[M]] =
    fromHikariConfigCustomEcWithResEffect[M, M](hikariConfig, connectEC, logHandler)

  /** Similar to [[fromHikariConfigCustomEc]], but with a separate effect for the construction of the Transactor
    *
    * @tparam M0
    *   the effect to construct the [[HikariTransactor]] in
    * @tparam M
    *   the effect under which the [[HikariTransactor]] runs
    */
  def fromHikariConfigCustomEcWithResEffect[M0[_]: Sync, M[_]: Async](
      hikariConfig: HikariConfig,
      connectEC: ExecutionContext,
      logHandler: Option[LogHandler[M]] = None
  ): Resource[M0, HikariTransactor[M]] = Resource
    .fromAutoCloseable(Sync[M0].delay(new HikariDataSource(hikariConfig)))
    .map(Transactor.fromDataSource[M](_, connectEC, logHandler))

  /** Resource yielding a new `HikariTransactor` configured with the given HikariConfig. The connection ExecutionContext
    * (used for waiting for a connection from the connection pool) is created automatically, with the same size as the
    * Hikari connection pool.
    */
  def fromHikariConfig[M[_]: Async](
      hikariConfig: HikariConfig,
      logHandler: Option[LogHandler[M]] = None
  ): Resource[M, HikariTransactor[M]] =
    fromHikariConfigWithResEffect[M, M](hikariConfig, logHandler)

  /** Similar to [[fromHikariConfig]], but with a separate effect for the construction of the Transactor
    *
    * @tparam M0
    *   the effect to construct the [[HikariTransactor]] in
    * @tparam M
    *   the effect under which the [[HikariTransactor]] runs
    */
  def fromHikariConfigWithResEffect[M0[_]: Sync, M[_]: Async](
      hikariConfig: HikariConfig,
      logHandler: Option[LogHandler[M]] = None
  ): Resource[M0, HikariTransactor[M]] =
    for {
      // to populate unset fields with default values, like `maximumPoolSize`
      _ <- Sync[M0].delay(hikariConfig.validate()).toResource
      // Note that the number of JDBC connections is usually limited by the underlying JDBC pool.
      // You may therefore want to limit your connection pool to the same size as the underlying JDBC pool
      // as any additional threads are guaranteed to be blocked.
      // https://tpolecat.github.io/doobie/docs/14-Managing-Connections.html#about-threading
      connectEC <- ExecutionContexts.fixedThreadPool[M0](hikariConfig.getMaximumPoolSize)
      result <- fromHikariConfigCustomEcWithResEffect[M0, M](hikariConfig, connectEC, logHandler)
    } yield result

  /** Resource yielding a new `HikariTransactor` configured with the given info. Consider using `fromConfig` for better
    * configurability.
    */
  def newHikariTransactor[M[_]: Async](
      driverClassName: String,
      url: String,
      user: String,
      pass: String,
      connectEC: ExecutionContext,
      logHandler: Option[LogHandler[M]] = None
  ): Resource[M, HikariTransactor[M]] =
    for {
      _ <- Resource.eval(Sync[M].delay(Class.forName(driverClassName)))
      t <- initial[M](connectEC, logHandler)
      _ <- Resource.eval[M, Unit] {
        t.configure { ds =>
          Sync[M].delay[Unit] {
            ds setJdbcUrl url
            ds setUsername user
            ds setPassword pass
          }
        }
      }
    } yield t

}
