// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.syntax

import cats.data.Ior
import scala.annotation.tailrec

final class AlignSyntax[A](as: List[A]) {
  def align[B](bs: List[B]): List[A Ior B] = {
    @tailrec
    def go(as: List[A], bs: List[B], acc: List[A Ior B]): List[A Ior B] =
      (as, bs) match {
        case (a :: as, b :: bs) => go(as, bs, Ior.Both(a, b) :: acc)
        case (a :: as, Nil)     => go(as, Nil, Ior.Left(a) :: acc)
        case (Nil, b :: bs)     => go(Nil, bs, Ior.Right(b) :: acc)
        case (Nil, Nil)         => acc.reverse
      }
    go(as, bs, Nil)
  }
}

trait ToAlignSyntax {
  implicit def toDoobieAlignSyntax[A](as: List[A]): AlignSyntax[A] =
    new AlignSyntax(as)
}

object align extends ToAlignSyntax
