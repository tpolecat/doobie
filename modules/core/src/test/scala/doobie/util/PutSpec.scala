// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.effect.{ ContextShift, IO }
import doobie._
import doobie.enum.JdbcType.{ Array => _ }
import org.specs2.mutable.Specification
import scala.concurrent.ExecutionContext
import shapeless.test._

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.Equals"))
object PutSpec extends Specification {
  final case class X(x: Int)
  final case class Y(x: String) extends AnyVal
  final case class P(x: Int) extends AnyVal
  final case class Q(x: String)

  final case class Z(i: Int, s: String)
  object S

  final case class Reg1(x: Int)
  final case class Reg2(x: Int)

  implicit def contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  val xa = Transactor.fromDriverManager[IO](
    "org.h2.Driver",
    "jdbc:h2:mem:queryspec;DB_CLOSE_DELAY=-1",
    "sa", ""
  )

  final case class Foo(s: String)
  final case class Bar(n: Int)

  "Put" should {

    "exist for primitive types" in {
      Put[Int]
      Put[String]
      true
    }

    "be derived for unary products" in {
      Put[X]
      Put[Y]
      Put[P]
      Put[Q]
      true
    }

    "not be derived for non-unary products" in {
      illTyped("Put[Z]")
      illTyped("Put[(Int, Int)]")
      illTyped("Put[S.type]")
      true
    }

    // TODO: check null-handing when writig

  }


}
