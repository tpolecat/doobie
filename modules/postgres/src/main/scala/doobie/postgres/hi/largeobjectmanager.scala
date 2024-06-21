// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres.hi

import cats.syntax.all._
import doobie.postgres.implicits._
import java.io.{File, OutputStream, InputStream}
import doobie.postgres.free.{largeobjectmanager => IPFLOM, largeobject => IPFLO}
import doobie.postgres.hi.{largeobjectmanager => IPHLOM, largeobject => IPHLO}

object largeobjectmanager {

  val createLO: LargeObjectManagerIO[Long] =
    IPFLOM.createLO

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def createLO(a: Int): LargeObjectManagerIO[Long] =
    IPFLOM.createLO(a)

  def delete(a: Long): LargeObjectManagerIO[Unit] =
    IPFLOM.delete(a)

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def open[A](a: Long, b: Int)(k: LargeObjectIO[A]): LargeObjectManagerIO[A] =
    IPFLOM.open(a, b) >>= (IPFLOM.embed(_, k <* IPFLO.close))

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def open[A](a: Long)(k: LargeObjectIO[A]): LargeObjectManagerIO[A] =
    IPFLOM.open(a) >>= (IPFLOM.embed(_, k <* IPFLO.close))

  def unlink(a: Long): LargeObjectManagerIO[Unit] =
    IPFLOM.unlink(a)

  def createLOFromFile(blockSize: Int, file: File): LargeObjectManagerIO[Long] =
    createLO >>= { oid => open(oid)(IPHLO.copyFromFile(blockSize, file)).as(oid) }

  def createFileFromLO(blockSize: Int, oid: Long, file: File): LargeObjectManagerIO[Unit] =
    open(oid)(IPHLO.copyToFile(blockSize, file))

  def createLOFromStream(blockSize: Int, is: InputStream): LargeObjectManagerIO[Long] =
    IPHLOM.createLO >>= { oid =>
      IPHLOM.open(oid)(IPHLO.copyFromStream(blockSize, is)).as(oid)
    }

  def createStreamFromLO(blockSize: Int, oid: Long, os: OutputStream): LargeObjectManagerIO[Unit] =
    open(oid)(IPHLO.copyToStream(blockSize, os))
}
