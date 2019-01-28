// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.bench

import cats.implicits._
import doobie._
import doobie.implicits._
import doobie.postgres.implicits._
import org.openjdk.jmh.annotations._


final case class Person(name: String, age: Int)

class text {
  import shared._

  def people(n: Int): List[Person] =
    List.fill(n)(Person("Bob", 42))

  def ddl: ConnectionIO[Unit] =
    sql"drop table if exists bench_person".update.run *>
    sql"create table bench_person (name varchar not null, age integer not null)".update.run.void

  def naive(n: Int): ConnectionIO[Int] =
    ddl *> HC.prepareStatement("insert into bench_person (name, age) values (?, ?)")(
      people(n).foldRight(HPS.executeBatch)((p, k) => HPS.set(p) *> HPS.addBatch *> k)
    ).map(_.combineAll)

  def optimized(n: Int): ConnectionIO[Int] =
    ddl *> HC.prepareStatement("insert into bench_person (name, age) values (?, ?)")(
      FPS.raw { e =>
        people(n).foreach { p =>
          e.jdbc.setString(1, p.name)
          e.jdbc.setInt(2, p.age)
          e.jdbc.addBatch
        }
        e.jdbc.executeBatch.sum
      }
    )

  def copyin(n: Int): ConnectionIO[Long] =
    ddl *> sql"COPY bench_person (name, age) FROM STDIN".copyIn(people(n))

  @Benchmark
  @OperationsPerInvocation(10000)
  def naive_copyin: Int = naive(10000).transact(xa).unsafeRunSync

  @Benchmark
  @OperationsPerInvocation(10000)
  def jdbc_copyin: Int = optimized(10000).transact(xa).unsafeRunSync

  @Benchmark
  @OperationsPerInvocation(10000)
  def fast_copyin: Long = copyin(10000).transact(xa).unsafeRunSync

}
