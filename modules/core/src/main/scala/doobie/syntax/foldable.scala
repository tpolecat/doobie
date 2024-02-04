// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.syntax

import cats._
import doobie.util.{foldable => F}

class FoldableOps[F[_]: Foldable, A: Monoid](self: F[A]) {
  def foldSmash1(prefix: A, delim: A, suffix: A): A = F.foldSmash1(self)(prefix, delim, suffix)
}

trait ToFoldableOps {
  implicit def toDoobieFoldableOps[F[_]: Foldable, A: Monoid](fa: F[A]): FoldableOps[F, A] =
    new FoldableOps(fa)
}

object foldable extends ToFoldableOps
