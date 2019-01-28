// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie
package h2

import cats.effect._
import org.h2.jdbcx.JdbcConnectionPool
import scala.concurrent.ExecutionContext

object H2Transactor {

  /** Resource yielding a new H2Transactor. */
  def newH2Transactor[M[_]: Async: ContextShift](
    url:        String,
    user:       String,
    pass:       String,
    connectEC:  ExecutionContext,
    transactEC: ExecutionContext
  ): Resource[M, H2Transactor[M]] = {
    val alloc = Async[M].delay(JdbcConnectionPool.create(url, user, pass))
    val free  = (ds: JdbcConnectionPool) => Async[M].delay(ds.dispose())
    Resource.make(alloc)(free).map(Transactor.fromDataSource[M](_, connectEC, transactEC))
  }

}
