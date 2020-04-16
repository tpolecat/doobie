// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats._
import cats.data.Chain
import cats.implicits._
import doobie._
import doobie.implicits._
import doobie.enum.Nullability._
import doobie.util.pos.Pos
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
  final class Fragment[-R](
    protected val sql: String,
    protected val elems: Chain[Elem[R]],
    protected val pos: Option[Pos]
  ) {

    /** Concatenate this fragment with another, yielding a larger fragment. */
    def ++[R2 <: R](fb: Fragment[R2]): Fragment[R2] =
      new Fragment(sql + fb.sql, elems ++ fb.elems, pos orElse fb.pos)

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def stripMargin(marginChar: Char): Fragment[R] =
      new Fragment(sql.stripMargin(marginChar), elems, pos)

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def stripMargin: Fragment[R] = stripMargin('|')

    override def toString =
      s"""Fragment("$sql")"""

    /** Used only for testing; this pulls out the arguments as an untyped list. */
    private def args: List[Any] =
      elems.toList.map {
        case Elem.Arg(a, _)      => a
        case Elem.Opt(a, _)      => a
        case Elem.FunArg(fun, _) => fun
        case Elem.FunOpt(fun, _) => fun
          }

    /** Used only for testing; this uses universal equality on the captured arguments. */
    @SuppressWarnings(Array("org.wartremover.warts.Equals"))
    private[util] def unsafeEquals[R2 <: R](fb: Fragment[R2]): Boolean =
      sql == fb.sql && args == fb.args

  }

  object Fragment {

    implicit class Fragment0Syntax(private val fragment: Fragment[Any])
        extends AnyVal {
      private implicit def write: Write[Any] = Fragment.write(fragment.elems)

      /**
       * Construct a program in ConnectionIO that constructs and prepares a PreparedStatement, with
       * further handling delegated to the provided program.
       */
      def execWith[B](fa: PreparedStatementIO[B]): ConnectionIO[B] =
        HC.prepareStatement(fragment.sql)(write.set(1, ()) *> fa)

      /** Construct a [[Query0]] from this fragment, with asserted row type `B`. */
      @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
      def query[B: Read](implicit h: LogHandler = LogHandler.nop): Query0[B] =
        queryWithLogHandler(h)

      /**
        * Construct a [[Query0]] from this fragment, with asserted row type `B` and the given
        * `LogHandler`.
        */
      def queryWithLogHandler[B](h: LogHandler)(implicit cb: Read[B]): Query0[B] =
        Query[Any, B](fragment.sql, fragment.pos, h).toQuery0(())

      /** Construct an [[Update0]] from this fragment. */
      @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
      def update(implicit h: LogHandler = LogHandler.nop): Update0 =
        updateWithLogHandler(h)

      /** Construct an [[Update0]] from this fragment with the given `LogHandler`. */
      def updateWithLogHandler(h: LogHandler): Update0 =
        Update[Any](fragment.sql, fragment.pos, h).toUpdate0(())

    }

    implicit class FragmentSyntax[R](private val fragment: Fragment[R]) extends AnyVal {
      private implicit def write: Write[R] = Fragment.write(fragment.elems)

      /**
        * Construct a program in ConnectionIO that constructs and prepares a PreparedStatement, with
        * further handling delegated to the provided program.
        */
      def execWith[B](fa: PreparedStatementIO[B], r: R): ConnectionIO[B] =
        HC.prepareStatement(fragment.sql)(write.set(1, r) *> fa)

      /** Construct a [[Query0]] from this fragment, with asserted row type `B`. */
      @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
      def query[B: Read](implicit h: LogHandler = LogHandler.nop): Query[R, B] =
        queryWithLogHandler(h)

    /**
     * Construct a [[Query0]] from this fragment, with asserted row type `B` and the given
     * `LogHandler`.
     */
      def queryWithLogHandler[B](h: LogHandler)(implicit cb: Read[B]): Query[R, B] =
        Query[R, B](fragment.sql, fragment.pos, h)

      /** Construct an [[Update0]] from this fragment. */
      @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
      def update(implicit h: LogHandler = LogHandler.nop): Update[R] =
        updateWithLogHandler(h)

      /** Construct an [[Update0]] from this fragment with the given `LogHandler`. */
      def updateWithLogHandler(h: LogHandler): Update[R] =
        Update[R](fragment.sql, fragment.pos, h)

    }

    // Unfortunately we need to produce a Write for our list of elems, which is a bit of a grunt
    // but straightforward nonetheless. And it's stacksafe!
    private[Fragment] def write[R](elems: Chain[Elem[R]]): Write[R] = {
      import Elem._

      val puts: List[(Put[_], NullabilityKnown)] =
        elems.map {
          case Arg(_, p)    => (p, NoNulls)
          case Opt(_, p)    => (p, Nullable)
          case FunArg(_, p) => (p, NoNulls)
          case FunOpt(_, p) => (p, Nullable)
        }.toList

      val toList: R => List[Any] = r =>
        elems.map {
          case Arg(a, _)      => a
          case Opt(a, _)      => a
          case FunArg(get, _) => get(r)
          case FunOpt(get, _) => get(r)
        }.toList

      @SuppressWarnings(Array("org.wartremover.warts.Var"))
      val unsafeSet: (PreparedStatement, Int, R) => Unit = { (ps, n, r) =>
        var index = n
        elems.iterator.foreach { e =>
          e match {
            case Arg(a, p)      => p.unsafeSetNonNullable(ps, index, a)
            case Opt(a, p)      => p.unsafeSetNullable(ps, index, a)
            case FunArg(get, p) => p.unsafeSetNonNullable(ps, index, get(r))
            case FunOpt(get, p) => p.unsafeSetNullable(ps, index, get(r))
          }
          index += 1
        }
      }

      @SuppressWarnings(Array("org.wartremover.warts.Var"))
      val unsafeUpdate: (ResultSet, Int, R) => Unit = { (ps, n, r) =>
        var index = n
        elems.iterator.foreach { e =>
          e match {
            case Arg(a, p)      => p.unsafeUpdateNonNullable(ps, index, a)
            case Opt(a, p)      => p.unsafeUpdateNullable(ps, index, a)
            case FunArg(get, p) => p.unsafeUpdateNonNullable(ps, index, get(r))
            case FunOpt(get, p) => p.unsafeUpdateNullable(ps, index, get(r))
          }
          index += 1
        }
      }

      new Write(puts, toList, unsafeSet, unsafeUpdate)

  }

    /**
     * Construct a statement fragment with the given SQL string, which must contain sufficient `?`
     * placeholders to accommodate the given list of interpolated elements. This is normally
     * accomplished via the string interpolator rather than direct construction.
     */
    @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
    def apply[R](sql: String,
                 elems: List[Elem[R]],
                 pos: Option[Pos] = None): Fragment[R] =
      new Fragment(sql, Chain.fromSeq(elems), pos)

    /**
     * Construct a statement fragment with no interpolated values and no trailing space; the
     * passed SQL string must not contain `?` placeholders.
     */
    @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
    def const0(sql: String, pos: Option[Pos] = None): Fragment[Any] =
      new Fragment(sql, Chain.empty, pos)

    /**
     * Construct a statement fragment with no interpolated values and a trailing space; the
     * passed SQL string must not contain `?` placeholders.
     */
     @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
    def const(sql: String, pos: Option[Pos] = None): Fragment[Any] =
      const0(sql + " ", pos)

    /** The empty fragment. Adding this to another fragment has no effect. */
    val empty: Fragment[Any] =
      const0("")

    /** Statement fragments form a monoid. */
    implicit def FragmentMonoid[R]: Monoid[Fragment[R]] =
      new Monoid[Fragment[R]] {
        val empty: Fragment[R] = Fragment.empty
        def combine(a: Fragment[R], b: Fragment[R]) = a ++ b
      }

  }

  sealed trait Elem[-R]
  object Elem {
    final case class Arg[A](a: A, p: Put[A]) extends Elem[Any]
    final case class Opt[A](a: Option[A], p: Put[A]) extends Elem[Any]
    final case class FunArg[R, A](fun: R => A, p: Put[A]) extends Elem[R]
    final case class FunOpt[R, A](fun: R => Option[A], p: Put[A]) extends Elem[R]
  }
}
