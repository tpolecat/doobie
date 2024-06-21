// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

// format: off

package doobie.postgres.free

import cats.free.Free

import copyin.CopyInIO
import copymanager.CopyManagerIO
import copyout.CopyOutIO
import largeobject.LargeObjectIO
import largeobjectmanager.LargeObjectManagerIO
import pgconnection.PGConnectionIO

// A pair (J, Free[F, A]) with constructors that tie down J and F.
sealed trait Embedded[A]

object Embedded {
  final case class CopyIn[A](j: org.postgresql.copy.CopyIn, fa: CopyInIO[A]) extends Embedded[A]
  final case class CopyManager[A](j: org.postgresql.copy.CopyManager, fa: CopyManagerIO[A]) extends Embedded[A]
  final case class CopyOut[A](j: org.postgresql.copy.CopyOut, fa: CopyOutIO[A]) extends Embedded[A]
  final case class LargeObject[A](j: org.postgresql.largeobject.LargeObject, fa: LargeObjectIO[A]) extends Embedded[A]
  final case class LargeObjectManager[A](j: org.postgresql.largeobject.LargeObjectManager, fa: LargeObjectManagerIO[A]) extends Embedded[A]
  final case class PGConnection[A](j: org.postgresql.PGConnection, fa: PGConnectionIO[A]) extends Embedded[A]
}

// Typeclass for embeddable pairs (J, F)
trait Embeddable[F[_], J] {
  def embed[A](j: J, fa: Free[F, A]): Embedded[A]
}

