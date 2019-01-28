// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.effect.{ ContextShift, IO }
import doobie._, doobie.implicits._
import doobie.enum.JdbcType.{ Array => _ }
import org.specs2.mutable.Specification
import scala.concurrent.ExecutionContext
import shapeless.test._

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.Equals"))
object GetSpec extends Specification {

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

  "Get" should {

    "exist for primitive types" in {
      Get[Int]
      Get[String]
      true
    }

    "be derived for unary products" in {
      Get[X]
      Get[Y]
      Get[P]
      Get[Q]
      true
    }

    "not be derived for non-unary products" in {
      illTyped("Get[Z]")
      illTyped("Get[(Int, Int)]")
      illTyped("Get[S.type]")
      true
    }

    // Both of these will fail at runtime if called with a null value, we check that this is
    // avoided below.
    implicit def FooMeta: Get[Foo] = Get[String].map(s => Foo(s.toUpperCase))
    implicit def barMeta: Get[Bar] = Get[Int].map(n => if (n == 0) sys.error("") else Bar(n))

    "not allow map to observe null on the read side (AnyRef)" in {
      val x = sql"select null".query[Option[Foo]].unique.transact(xa).unsafeRunSync
      x must_== None
    }

    "read non-null value (AnyRef)" in {
      val x = sql"select 'abc'".query[Foo].unique.transact(xa).unsafeRunSync
      x must_== Foo("ABC")
    }

    "throw when reading a NULL into an unlifted Scala type (AnyRef)" in {
      def x = sql"select null".query[Foo].unique.transact(xa).unsafeRunSync
      x must throwA[doobie.util.invariant.NonNullableColumnRead]
    }

    "not allow map to observe null on the read side (AnyVal)" in {
      val x = sql"select null".query[Option[Bar]].unique.transact(xa).unsafeRunSync
      x must_== None
    }

    "read non-null value (AnyVal)" in {
      val x = sql"select 1".query[Bar].unique.transact(xa).unsafeRunSync
      x must_== Bar(1)
    }

    "throw when reading a NULL into an unlifted Scala type (AnyVal)" in {
      def x = sql"select null".query[Bar].unique.transact(xa).unsafeRunSync
      x must throwA[doobie.util.invariant.NonNullableColumnRead]
    }

  }


}
