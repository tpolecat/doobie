// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.implicits._
import doobie._, doobie.implicits._
import cats.data.NonEmptyList
import org.specs2.mutable.Specification
import cats.effect.ContextShift
import scala.concurrent.ExecutionContext
import cats.effect.IO

class fragmentsspec extends Specification {
  import Fragments._

  implicit def contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  val xa = Transactor.fromDriverManager[IO](
    "org.h2.Driver",
    "jdbc:h2:mem:queryspec;DB_CLOSE_DELAY=-1",
    "sa",
    ""
  )

  "Fragments" >> {

    val fs = List(1, 2, 3).map(n => fr"$n")
    val fsNel = NonEmptyList.of(1, 2, 3).map(n => fr"$n")
    val ofs = List(1, 2, 3).map(n => Some(fr"$n").filter(_ => n % 2 =!= 0))
    val fsEmpty = List[Fragment]()
    val ofsEmpty = List[Option[Fragment]](None, None)

    "inNe" in {
      inNe(fr"foo", fsNel).query[Unit].sql must_=== "(foo IN (? , ? , ? ) ) "
    }

    "in (many)" in {
      in(fr"foo", fs: _*).map(_.query[Unit].sql) must
        beSome("(foo IN (? , ? , ? ) ) ")
    }

    "in (single)" in {
      in(fr"foo", fs(0)).map(_.query[Unit].sql) must
        beSome("(foo IN (? ) ) ")
    }

    "in (empty)" in {
      in(fr"foo").map(_.query[Unit].sql) must
        beNone
    }

    "inOpt (many)" in {
      inOpt(fr"foo", ofs: _*).map(_.query[Unit].sql) must
        beSome("(foo IN (? , ? ) ) ")
    }

    "inOpt (one)" in {
      inOpt(fr"foo", ofs(0)).map(_.query[Unit].sql) must
        beSome("(foo IN (? ) ) ")
    }

    "inOpt (none)" in {
      inOpt(fr"foo", None, None).map(_.query[Unit].sql) must
        beNone
    }

    "notInNe" in {
      notInNe(fr"foo", fsNel)
        .query[Unit]
        .sql must_=== "(foo NOT IN (? , ? , ? ) ) "
    }

    "notIn (many)" in {
      notIn(fr"foo", fs: _*).map(_.query[Unit].sql) must
        beSome("(foo NOT IN (? , ? , ? ) ) ")
    }

    "notIn (single)" in {
      notIn(fr"foo", fs(0)).map(_.query[Unit].sql) must
        beSome("(foo NOT IN (? ) ) ")
    }

    "notIn (empty)" in {
      notIn(fr"foo").map(_.query[Unit].sql) must
        beNone
    }

    "notInOpt (many)" in {
      notInOpt(fr"foo", ofs: _*).map(_.query[Unit].sql) must
        beSome("(foo NOT IN (? , ? ) ) ")
    }

    "notInOpt (one)" in {
      notInOpt(fr"foo", ofs(0)).map(_.query[Unit].sql) must
        beSome("(foo NOT IN (? ) ) ")
    }

    "notInOpt (none)" in {
      notInOpt(fr"foo", None, None).map(_.query[Unit].sql) must
        beNone
    }

    "andNe" in {
      andNe(fsNel).query[Unit].sql must_=== "(? AND ? AND ? ) "
    }

    "and (many)" in {
      and(fs: _*).map(_.query[Unit].sql) must beSome("(? AND ? AND ? ) ")
    }

    "and (single)" in {
      and(fs(0)).map(_.query[Unit].sql) must beSome("(? ) ")
    }

    "and (empty)" in {
      and().map(_.query[Unit].sql) must beNone
    }

    "andOpt (many)" in {
      andOpt(ofs: _*).map(_.query[Unit].sql) must beSome("(? AND ? ) ")
    }

    "andOpt (one)" in {
      andOpt(ofs(0)).map(_.query[Unit].sql) must beSome("(? ) ")
    }

    "andOpt (none)" in {
      andOpt(None, None).map(_.query[Unit].sql) must beNone
    }

    "orNe" in {
      orNe(fsNel).query[Unit].sql must_=== "(? OR ? OR ? ) "
    }

    "or (many)" in {
      or(fs: _*).map(_.query[Unit].sql) must beSome("(? OR ? OR ? ) ")
    }

    "or (single)" in {
      or(fs(0)).map(_.query[Unit].sql) must beSome("(? ) ")
    }

    "or (empty)" in {
      or().map(_.query[Unit].sql) must beNone
    }

    "orOpt (many)" in {
      orOpt(ofs: _*).map(_.query[Unit].sql) must beSome("(? OR ? ) ")
    }

    "orOpt (one)" in {
      orOpt(ofs(0)).map(_.query[Unit].sql) must beSome("(? ) ")
    }

    "orOpt (none)" in {
      orOpt(None, None).map(_.query[Unit].sql) must beNone
    }

    "andOpt(or) (many)" in {
      andOpt(or(fs: _*), or(fs: _*)).map(_.query[Unit].sql) must beSome(
        "((? OR ? OR ? ) AND (? OR ? OR ? ) ) "
      )
    }

    "andOpt(orOpt) (many)" in {
      andOpt(orOpt(ofs: _*), orOpt(ofs: _*)).map(_.query[Unit].sql) must beSome(
        "((? OR ? ) AND (? OR ? ) ) "
      )
    }

    "andOpt(or) (some)" in {
      andOpt(or(fs: _*), or(fsEmpty: _*))
        .map(_.query[Unit].sql) must beSome("((? OR ? OR ? ) ) ")
    }

    "andOpt(orOpt) (some)" in {
      andOpt(orOpt(ofsEmpty: _*), orOpt(ofs: _*))
        .map(_.query[Unit].sql) must beSome("((? OR ? ) ) ")
    }

    "andOpt(or) (one)" in {
      andOpt(or(fs(0)), or(fs: _*)).map(_.query[Unit].sql) must beSome(
        "((? ) AND (? OR ? OR ? ) ) "
      )
    }

    "andOpt(orOpt) (one)" in {
      andOpt(orOpt(ofs: _*), orOpt(ofs(0))).map(_.query[Unit].sql) must beSome(
        "((? OR ? ) AND (? ) ) "
      )
    }

    "andOpt(or) (empty)" in {
      andOpt(or(fsEmpty: _*), or(fsEmpty: _*))
        .map(_.query[Unit].sql) must beNone
    }

    "andOpt(orOpt) (empty)" in {
      andOpt(orOpt(ofsEmpty: _*), orOpt(ofsEmpty: _*))
        .map(_.query[Unit].sql) must beNone
    }

    "whereAnd (many)" in {
      whereAnd(fs: _*).query[Unit].sql must_== "WHERE (? AND ? AND ? ) "
    }

    "whereAnd (single)" in {
      whereAnd(fs(0)).query[Unit].sql must_== "WHERE (? ) "
    }

    "whereAnd (empty)" in {
      whereAnd().query[Unit].sql must_== ""
    }

    "whereAndOpt (many)" in {
      whereAndOpt(ofs: _*).query[Unit].sql must_== "WHERE (? AND ? ) "
    }

    "whereAndOpt (one)" in {
      whereAndOpt(ofs(0)).query[Unit].sql must_== "WHERE (? ) "
    }

    "whereAndOpt (none)" in {
      whereAndOpt(None, None).query[Unit].sql must_== ""
    }

    "whereOr (many)" in {
      whereOr(fs: _*).query[Unit].sql must_== "WHERE (? OR ? OR ? ) "
    }

    "whereOr (single)" in {
      whereOr(fs(0)).query[Unit].sql must_== "WHERE (? ) "
    }

    "whereOr (empty)" in {
      whereOr().query[Unit].sql must_== ""
    }

    "whereOrOpt (many)" in {
      whereOrOpt(ofs: _*).query[Unit].sql must_== "WHERE (? OR ? ) "
    }

    "whereOrOpt (one)" in {
      whereOrOpt(ofs(0)).query[Unit].sql must_== "WHERE (? ) "
    }

    "whereOrOpt (none)" in {
      whereOrOpt(None, None).query[Unit].sql must_== ""
    }

    case class Person(name: String, age: Int)
    case class Contact(person: Person, address: Option[String])

    "values (1)" in {
      val c = Contact(Person("Bob", 42), Some("addr"))
      val f = fr"select" ++ Fragments.values(c)
      f.query[Contact].unique.transact(xa).unsafeRunSync must_== c
    }

    "values (2)" in {
      val c = Contact(Person("Bob", 42), None)
      val f = fr"select" ++ Fragments.values(c)
      f.query[Contact].unique.transact(xa).unsafeRunSync must_== c
    }

  }

}
