// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.bench

import cats.syntax.all.*
import doobie.{ConnectionIO, FPS, HC, HPS}
import doobie.implicits.*
import doobie.postgres.implicits.*
import fs2.*
import org.openjdk.jmh.annotations.*

final case class Person(name: String, age: Int)

class text {
  import shared.*
  import cats.effect.unsafe.implicits.global

  def people(n: Int): List[Person] =
    List.fill(n)(Person("Bob", 42))

  def ddl: ConnectionIO[Unit] =
    sql"drop table if exists bench_person".update.run *>
      sql"create table bench_person (name varchar not null, age integer not null)".update.run.void

  def naive(n: Int): ConnectionIO[Int] =
    ddl *> HC.prepareStatementPrimitive("insert into bench_person (name, age) values (?, ?)")(
      people(n).foldRight(HPS.executeBatch)((p, k) => HPS.set(p) *> FPS.addBatch *> k)
    ).map(_.combineAll)

  def optimized(n: Int): ConnectionIO[Int] =
    ddl *> HC.prepareStatementPrimitive("insert into bench_person (name, age) values (?, ?)")(
      FPS.raw { ps =>
        people(n).foreach { p =>
          ps.setString(1, p.name)
          ps.setInt(2, p.age)
          ps.addBatch
        }
        ps.executeBatch.sum
      }
    )

  def copyin_stream(n: Int): ConnectionIO[Long] =
    ddl *> sql"COPY bench_person (name, age) FROM STDIN".copyIn(Stream.emits[ConnectionIO, Person](people(n)), 10000)

  def copyin_foldable(n: Int): ConnectionIO[Long] =
    ddl *> sql"COPY bench_person (name, age) FROM STDIN".copyIn(people(n))

  @Benchmark
  @OperationsPerInvocation(10000)
  def naive_copyin: Int = naive(10000).transact(xa).unsafeRunSync()

  @Benchmark
  @OperationsPerInvocation(10000)
  def jdbc_copyin: Int = optimized(10000).transact(xa).unsafeRunSync()

  @Benchmark
  @OperationsPerInvocation(10000)
  def fast_copyin_stream: Long = copyin_stream(10000).transact(xa).unsafeRunSync()

  @Benchmark
  @OperationsPerInvocation(10000)
  def fast_copyin_foldable: Long = copyin_foldable(10000).transact(xa).unsafeRunSync()
}
