// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import shapeless.{HList, HNil, ::, Generic, Lazy}

trait TextPlatform { this: Text.type =>

  // HNil isn't a valid Text but a single-element HList is
  implicit def single[A](
      implicit csv: Text[A]
  ): Text[A :: HNil] =
    csv.contramap(_.head)

  // HLists of more that one element
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  implicit def multiple[H, T <: HList](
      implicit
      h: Text[H],
      t: Text[T]
  ): Text[H :: T] =
    (h product t).contramap(l => (l.head, l.tail))

  // Generic
  implicit def generic[A, B](
      implicit
      gen: Generic.Aux[A, B],
      csv: Lazy[Text[B]]
  ): Text[A] =
    csv.value.contramap(gen.to)

}
