// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats.*
import cats.data.Chain
import doobie.free.connection.ConnectionIO
import doobie.free.preparedstatement.PreparedStatementIO
import doobie.util.pos.Pos
import doobie.hi.connection as IHC
import doobie.util.query.{Query, Query0}
import doobie.util.update.{Update, Update0}

/** Module defining the `Fragment` data type. */
object fragment {

  /** A statement fragment, which may include interpolated values. Fragments can be composed by concatenation, which
    * maintains the correct offset and mappings for interpolated values. Once constructed a `Fragment` is opaque; it has
    * no externally observable properties. Fragments are eventually used to construct a [[Query0]] or [[Update0]].
    */
  final class Fragment(
      protected val sql: String,
      protected val elems: Chain[Elem],
      protected val pos: Option[Pos]
  ) {

    // Unfortunately we need to produce a Write for our list of elems, which is a bit of a grunt
    // but straightforward nonetheless. And it's stacksafe!
    private implicit lazy val write: Write[elems.type] = {
      import Elem.*

      val writes: List[Write[?]] =
        elems.map {
          case Arg(_, p) => Write.Single(p)
          case Opt(_, p) => Write.OptSingle(p)
        }.toList

      Write.Composite(
        writes,
        a =>
          a.map {
            case Arg(a, _) => a
            case Opt(a, _) => a
          }.toList
      )
    }

    /** Construct a program in ConnectionIO that constructs and prepares a PreparedStatement, with further handling
      * delegated to the provided program.
      */
    def execWith[B](fa: PreparedStatementIO[B]): ConnectionIO[B] =
      IHC.prepareStatementPrimitive(sql)(write.set(1, elems).flatMap(_ => fa))

    /** Concatenate this fragment with another, yielding a larger fragment. */
    def ++(fb: Fragment): Fragment =
      new Fragment(sql + fb.sql, elems ++ fb.elems, pos orElse fb.pos)

    def stripMargin(marginChar: Char): Fragment =
      new Fragment(sql.stripMargin(marginChar), elems, pos)

    def stripMargin: Fragment = stripMargin('|')

    /** Construct a [[Query0]] from this fragment, with asserted row type `B`. */
    def query[B: Read]: Query0[B] =
      queryWithLabel(unlabeled)

    /** Construct a [[Query0]] from this fragment, with asserted row type `B` and the given label.
      */
    def queryWithLabel[B](label: String)(implicit cb: Read[B]): Query0[B] =
      Query[elems.type, B](sql, pos, label).toQuery0(elems)

    /** Construct an [[Update0]] from this fragment. */
    def update: Update0 =
      updateWithLabel(unlabeled)

    /** Construct an [[Update0]] from this fragment with the given `LogHandler`. */
    def updateWithLabel(label: String): Update0 =
      Update[elems.type](sql, pos, label)(implicitly[Write[elems.type]]).toUpdate0(elems)

    override def toString =
      s"""Fragment("$sql")"""

    /** Used only for testing; this pulls out the arguments as an untyped list. */
    private def args: List[Any] =
      elems.toList.map {
        case Elem.Arg(a, _) => a
        case Elem.Opt(a, _) => a
      }

    /** Used only for testing; this uses universal equality on the captured arguments. */
    private[util] def unsafeEquals(fb: Fragment): Boolean =
      sql == fb.sql && args == fb.args

    /** Internals of this fragment. */
    def internals: Fragment.Internals =
      new Fragment.Internals {
        type A = elems.type
        val arg = elems
        val write = Fragment.this.write
        val elements = args
        val pos = Fragment.this.pos
        val sql = Fragment.this.sql
      }

  }
  object Fragment {

    /** Internals of a `Fragment`, available for diagnostic purposes. Monoidal structure is *not* preserved for the
      * elements of this object.
      */
    trait Internals {

      /** Existential type of this Fragment's argument (typically an HList). */
      type A

      /** This Fragment's argument, as an opaque value (typically an HList). */
      def arg: A

      /** A `Write` instance for `A`. */
      def write: Write[A]

      /** The elements of `a` as an untyped list. */
      def elements: List[Any]

      /** Source code position where this Fragment was constructed, if known. */
      def pos: Option[Pos]

      /** This `Fragment`'s SQL string. */
      def sql: String

    }

    /** Construct a statement fragment with the given SQL string, which must contain sufficient `?` placeholders to
      * accommodate the given list of interpolated elements. This is normally accomplished via the string interpolator
      * rather than direct construction.
      */
    def apply(sql: String, elems: List[Elem], pos: Option[Pos] = None): Fragment =
      new Fragment(sql, Chain.fromSeq(elems), pos)

    /** Construct a statement fragment with no interpolated values and no trailing space; the passed SQL string must not
      * contain `?` placeholders.
      */
    def const0(sql: String, pos: Option[Pos] = None): Fragment =
      new Fragment(sql, Chain.empty, pos)

    /** Construct a statement fragment with no interpolated values and a trailing space; the passed SQL string must not
      * contain `?` placeholders.
      */
    def const(sql: String, pos: Option[Pos] = None): Fragment =
      const0(sql + " ", pos)

    /** The empty fragment. Adding this to another fragment has no effect. */
    val empty: Fragment =
      const0("")

    /** Statement fragments form a monoid. */
    implicit val FragmentMonoid: Monoid[Fragment] =
      new Monoid[Fragment] {
        val empty = Fragment.empty
        def combine(a: Fragment, b: Fragment) = a ++ b
      }

  }

  sealed trait Elem
  object Elem {
    final case class Arg[A](a: A, p: Put[A]) extends Elem
    final case class Opt[A](a: Option[A], p: Put[A]) extends Elem
  }

}
