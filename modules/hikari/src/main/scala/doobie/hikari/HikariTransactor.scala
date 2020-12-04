// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie
package hikari

import java.util.Properties
import java.util.concurrent.{ScheduledExecutorService, ThreadFactory}

import cats.effect._
import com.zaxxer.hikari.metrics.MetricsTrackerFactory
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import javax.sql.DataSource

import scala.concurrent.ExecutionContext

object HikariTransactor {

  /** Construct a `HikariTransactor` from an existing `HikariDatasource`. */
  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply[M[_]: Async: ContextShift](
    hikariDataSource : HikariDataSource,
    connectEC:         ExecutionContext,
    blocker:           Blocker
  ): HikariTransactor[M] =
    Transactor.fromDataSource[M](hikariDataSource, connectEC, blocker)

  private def createDataSourceResource[M[_]: Sync](factory: => HikariDataSource): Resource[M, HikariDataSource] = {
    val alloc = Sync[M].delay(factory)
    val free = (ds: HikariDataSource) => Sync[M].delay(ds.close())
    Resource.make(alloc)(free)
  }

  /** Resource yielding an unconfigured `HikariTransactor`. */
  def initial[M[_]: Async: ContextShift](
    connectEC: ExecutionContext,
    blocker: Blocker
  ): Resource[M, HikariTransactor[M]] = {
    createDataSourceResource(new HikariDataSource)
      .map(Transactor.fromDataSource[M](_, connectEC, blocker))
  }

  /** Resource yielding a new `HikariTransactor` configured with the given Config. */
  def fromConfig[M[_]: Async: ContextShift](
    config: Config,
    connectEC: ExecutionContext,
    blocker: Blocker,
    dataSource: Option[DataSource] = None,
    dataSourceProperties: Option[Properties] = None,
    healthCheckProperties: Option[Properties] = None,
    healthCheckRegistry: Option[Object] = None,
    metricRegistry: Option[Object] = None,
    metricsTrackerFactory: Option[MetricsTrackerFactory] = None,
    scheduledExecutor: Option[ScheduledExecutorService] = None,
    threadFactory: Option[ThreadFactory] = None,
  ): Resource[M, HikariTransactor[M]] = {
    Resource
      .liftF(
        Config.makeHikariConfig(
          config,
          dataSource,
          dataSourceProperties,
          healthCheckProperties,
          healthCheckRegistry,
          metricRegistry,
          metricsTrackerFactory,
          scheduledExecutor,
          threadFactory
        )
      )
      .flatMap { hikariConfig =>
        fromHikariConfig(hikariConfig, connectEC, blocker)
      }
  }

  /** Resource yielding a new `HikariTransactor` configured with the given HikariConfig. */
  def fromHikariConfig[M[_]: Async: ContextShift](
    hikariConfig: HikariConfig,
    connectEC: ExecutionContext,
    blocker: Blocker
  ): Resource[M, HikariTransactor[M]] = {
    createDataSourceResource(new HikariDataSource(hikariConfig))
      .map(Transactor.fromDataSource[M](_, connectEC, blocker))
  }

  /** Resource yielding a new `HikariTransactor` configured with the given info. */
  def newHikariTransactor[M[_]: Async: ContextShift](
    driverClassName: String,
    url:             String,
    user:            String,
    pass:            String,
    connectEC:       ExecutionContext,
    blocker:         Blocker
  ): Resource[M, HikariTransactor[M]] =
    for {
      _ <- Resource.liftF(Async[M].delay(Class.forName(driverClassName)))
      t <- initial[M](connectEC, blocker)
      _ <- Resource.liftF {
            t.configure { ds =>
              Async[M].delay {
                ds setJdbcUrl  url
                ds setUsername user
                ds setPassword pass
              }
            }
          }
    } yield t

}
