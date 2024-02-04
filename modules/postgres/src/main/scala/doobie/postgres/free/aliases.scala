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
  lazy val PFCI = copyin
  lazy val PFCM = copymanager
  lazy val PFCO = copyout
  lazy val PFLO = largeobject
  lazy val PFLOM = largeobjectmanager
  lazy val PFPC = pgconnection
}

trait Instances {

  implicit lazy val WeakAsyncCopyInIO: WeakAsync[copyin.CopyInIO] =
    copyin.WeakAsyncCopyInIO

  implicit lazy val WeakAsyncCopyManagerIO: WeakAsync[copymanager.CopyManagerIO] =
    copymanager.WeakAsyncCopyManagerIO

  implicit lazy val WeakAsyncCopyOutIO: WeakAsync[copyout.CopyOutIO] =
    copyout.WeakAsyncCopyOutIO

  implicit lazy val WeakAsyncLargeObjectIO: WeakAsync[largeobject.LargeObjectIO] =
    largeobject.WeakAsyncLargeObjectIO

  implicit lazy val WeakAsyncLargeObjectManagerIO: WeakAsync[largeobjectmanager.LargeObjectManagerIO] =
    largeobjectmanager.WeakAsyncLargeObjectManagerIO

  implicit lazy val WeakAsyncPGConnectionIO: WeakAsync[pgconnection.PGConnectionIO] =
    pgconnection.WeakAsyncPGConnectionIO

}
