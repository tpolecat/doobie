// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats._
import cats.data.Chain
import cats.implicits._
import com.github.ghik.silencer.silent
import doobie._, doobie.implicits._
import doobie.util.pos.Pos
import doobie.util.param.Param.Elem
import doobie.enum.Nullability._
import java.sql.{ PreparedStatement, ResultSet }
import scala.Predef.augmentString

/** Module defining the `Fragment` data type. */
object fragment {

  /**
   * A statement fragment, which may include interpolated values. Fragments can be composed by
   * concatenation, which maintains the correct offset and mappings for interpolated values. Once
   * constructed a `Fragment` is opaque; it has no externally observable properties. Fragments are
   * eventually used to construct a [[Query0]] or [[Update0]].
   */
  final class Fragment(
    protected val sql: String,
    protected val elems: Chain[Elem],
    protected val pos: Option[Pos]
  ) {

    // Unfortunately we need to produce a Write for our list of elems, which is a bit of a grunt
    // but straightforward nonetheless. And it's stacksafe!
    private implicit lazy val write: Write[elems.type] = {
      import Elem._

      val puts: List[(Put[_], NullabilityKnown)] =
        elems.map {
          case Arg(_, p) => (p, NoNulls)
          case Opt(_, p) => (p, Nullable)
        } .toList

      val toList: elems.type => List[Any] = elems =>
        elems.map {
          case Arg(a, _) => a
          case Opt(a, _) => a
        } .toList

      @SuppressWarnings(Array("org.wartremover.warts.Var"))
      val unsafeSet: (PreparedStatement, Int, elems.type) => Unit = { (ps, n, elems) =>
        var index = n
        elems.iterator.foreach { e =>
          e match {
            case Arg(a, p) => p.unsafeSetNonNullable(ps, index, a)
            case Opt(a, p) => p.unsafeSetNullable(ps, index, a)
          }
          index += 1
        }
      }

      @SuppressWarnings(Array("org.wartremover.warts.Var"))
      val unsafeUpdate: (ResultSet, Int, elems.type) => Unit = { (ps, n, elems) =>
        var index = n
        elems.iterator.foreach { e =>
          e match {
            case Arg(a, p) => p.unsafeUpdateNonNullable(ps, index, a)
            case Opt(a, p) => p.unsafeUpdateNullable(ps, index, a)
          }
          index += 1
        }
      }

      new Write(puts, toList, unsafeSet, unsafeUpdate)

    }

    /**
     * Construct a program in ConnectionIO that constructs and prepares a PreparedStatement, with
     * further handling delegated to the provided program.
     */
    def execWith[B](fa: PreparedStatementIO[B]): ConnectionIO[B] =
      HC.prepareStatement(sql)(write.set(1, elems) *> fa)

    /** Concatenate this fragment with another, yielding a larger fragment. */
    def ++(fb: Fragment): Fragment =
      new Fragment(sql + fb.sql, elems ++ fb.elems, pos orElse fb.pos)

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def stripMargin(marginChar: Char): Fragment =
      new Fragment(sql.stripMargin(marginChar), elems, pos)

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def stripMargin: Fragment = stripMargin('|')

    /** Construct a [[Query0]] from this fragment, with asserted row type `B`. */
    @silent // deprecation
    @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
    def query[B: Read](implicit h: LogHandler = LogHandler.nop): Query0[B] =
      queryWithLogHandler(h)

    /**
     * Construct a [[Query0]] from this fragment, with asserted row type `B` and the given
     * `LogHandler`.
     */
    @silent // deprecation
    def queryWithLogHandler[B](h: LogHandler)(implicit cb: Read[B]): Query0[B] =
      Query[elems.type, B](sql, pos, h).toQuery0(elems)

    /** Construct an [[Update0]] from this fragment. */
    @silent // deprecation
    @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
    def update(implicit h: LogHandler = LogHandler.nop): Update0 =
      updateWithLogHandler(h)

    /** Construct an [[Update0]] from this fragment with the given `LogHandler`. */
    @silent // deprecation
    def updateWithLogHandler(h: LogHandler): Update0 =
      Update[elems.type](sql, pos, h).toUpdate0(elems)

    override def toString =
      s"""Fragment("$sql")"""

    /** Used only for testing; this pulls out the arguments as an untyped list. */
    private def args: List[Any] =
      elems.toList.map {
        case Elem.Arg(a, _) => a
        case Elem.Opt(a, _) => a
      }

    /** Used only for testing; this uses universal equality on the captured arguments. */
    @SuppressWarnings(Array("org.wartremover.warts.Equals"))
    private[util] def unsafeEquals(fb: Fragment): Boolean =
      sql == fb.sql && args == fb.args

  }
  object Fragment {

    /**
     * Construct a statement fragment with the given SQL string, which must contain sufficient `?`
     * placeholders to accommodate the given list of interpolated elements. This is normally
     * accomplished via the string interpolator rather than direct construction.
     */
    @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
    def apply(sql: String, elems: List[Elem], pos: Option[Pos] = None): Fragment =
      new Fragment(sql, Chain.fromSeq(elems), pos)

    /**
     * Construct a statement fragment with no interpolated values and no trailing space; the
     * passed SQL string must not contain `?` placeholders.
     */
    @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
    def const0(sql: String, pos: Option[Pos] = None): Fragment =
      new Fragment(sql, Chain.empty, pos)

    /**
     * Construct a statement fragment with no interpolated values and a trailing space; the
     * passed SQL string must not contain `?` placeholders.
     */
     @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
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

}
