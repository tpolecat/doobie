// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import doobie.util.lens._

import cats.data.State

class LensSuite extends munit.FunSuite {

  case class Name(first: String, last: String)
  object Name {
    val first: Name @> String = Lens(_.first, (a, b) => a.copy(first = b))
    val last: Name @> String = Lens(_.last, (a, b) => a.copy(last = b))
  }

  case class Address(name: Name, street: String)
  object Address {
    val name: Address @> Name = Lens(_.name, (a, b) => a.copy(name = b))
    val street: Address @> String = Lens(_.street, (a, b) => a.copy(street = b))
    val first: Address @> String = name >=> Name.first
    val last: Address @> String = name >=> Name.last
  }

  val bob = Address(Name("Bob", "Dole"), "123 Foo St.")

  def exec[S](st: State[S, _], s: S): S =
    st.runS(s).value

  import Address._

  test("Lens should modify ok") {
    val prog: State[Address, Unit] =
      for {
        _ <- first %= (_.toUpperCase)
        _ <- last %= (_.toLowerCase)
        _ <- street %= (_.replace('o', '*'))
      } yield ()
    assertEquals(exec(prog, bob), Address(Name("BOB", "dole"), "123 F** St."))
  }

  test("Lens should set ok") {
    val prog: State[Address, Unit] =
      for {
        _ <- first := "Jimmy"
        _ <- last := "Carter"
        _ <- street := "12 Peanut Dr."
      } yield ()
    assertEquals(exec(prog, bob), Address(Name("Jimmy", "Carter"), "12 Peanut Dr."))
  }

}
