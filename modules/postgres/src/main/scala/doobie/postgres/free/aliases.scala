// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres.free

import doobie.WeakAsync

trait Types {
  type CopyInIO[A] = copyin.CopyInIO[A]
  type CopyManagerIO[A] = copymanager.CopyManagerIO[A]
  type CopyOutIO[A] = copyout.CopyOutIO[A]
  type LargeObjectIO[A] = largeobject.LargeObjectIO[A]
  type LargeObjectManagerIO[A] = largeobjectmanager.LargeObjectManagerIO[A]
  type PGConnectionIO[A] = pgconnection.PGConnectionIO[A]
}

trait Modules {
  val PFCI = copyin
  val PFCM = copymanager
  val PFCO = copyout
  val PFLO = largeobject
  val PFLOM = largeobjectmanager
  val PFPC = pgconnection
}

trait Instances {

  implicit val WeakAsyncCopyInIO: WeakAsync[copyin.CopyInIO] =
    copyin.WeakAsyncCopyInIO

  implicit val WeakAsyncCopyManagerIO: WeakAsync[copymanager.CopyManagerIO] =
    copymanager.WeakAsyncCopyManagerIO

  implicit val WeakAsyncCopyOutIO: WeakAsync[copyout.CopyOutIO] =
    copyout.WeakAsyncCopyOutIO

  implicit val WeakAsyncLargeObjectIO: WeakAsync[largeobject.LargeObjectIO] =
    largeobject.WeakAsyncLargeObjectIO

  implicit val WeakAsyncLargeObjectManagerIO: WeakAsync[largeobjectmanager.LargeObjectManagerIO] =
    largeobjectmanager.WeakAsyncLargeObjectManagerIO

  implicit val WeakAsyncPGConnectionIO: WeakAsync[pgconnection.PGConnectionIO] =
    pgconnection.WeakAsyncPGConnectionIO

}
