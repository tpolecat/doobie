// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import cats.{ContravariantSemigroupal, Foldable}
import cats.syntax.foldable._

/** Typeclass for types that can be written as Postgres literal text, using the default DELIMETER and NULL values, for
  * use with `COPY`. If you wish to implement an instance it's worth reading the documentation at the link below.
  * @see
  *   [[https://www.postgresql.org/docs/9.6/static/sql-copy.html Postgres `COPY` command]]
  */

trait Text[A] { outer =>

  /** Construct an encoder for `A` that appends to the provided `StringBuilder.
    * @param a
    *   the value to encode
    */
  def unsafeEncode(a: A, sb: StringBuilder): Unit

  /** Variant of unsafeEncode, called for array members which require more escaping. Same as unsafeEncode unless
    * overridden.
    */
  def unsafeArrayEncode(a: A, sb: StringBuilder): Unit =
    unsafeEncode(a, sb)

  /** Encode `a`. */
  final def encode(a: A): String = {
    val sb = new StringBuilder
    unsafeEncode(a, sb)
    sb.toString
  }

  /** `Text` is a contravariant functor. */
  final def contramap[B](f: B => A): Text[B] =
    Text.instance((b, sb) => outer.unsafeEncode(f(b), sb))

  /** `Text` is semigroupal. */
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def product[B](fb: Text[B]): Text[(A, B)] =
    new Text[(A, B)] {
      def unsafeEncode(ab: (A, B), sb: StringBuilder) = {
        outer.unsafeEncode(ab._1, sb)
        sb.append(Text.DELIMETER)
        fb.unsafeEncode(ab._2, sb)
      }
    }

}
object Text extends TextInstances with TextPlatform {
  def apply[A](implicit ev: Text[A]): ev.type = ev

  val DELIMETER: Char = '\t'
  val NULL: String = "\\N"

  /** Construct an instance, given a function matching the `unsafeEncode` signature.
    * @param f
    *   a function from `(A, DELIMETER, NULL) => StringBuilder => StringBuilder`
    */
  def instance[A](f: (A, StringBuilder) => Unit): Text[A] =
    new Text[A] {
      def unsafeEncode(a: A, sb: StringBuilder) = f(a, sb)
    }

}

trait TextInstances extends TextInstances0 { this: Text.type =>

  /** `Text` is both contravariant and semigroupal. */
  implicit val CsvContravariantSemigroupal: ContravariantSemigroupal[Text] =
    new ContravariantSemigroupal[Text] {
      def contramap[A, B](fa: Text[A])(f: B => A) = fa.contramap(f)
      def product[A, B](fa: Text[A], fb: Text[B]): Text[(A, B)] = fa.product(fb)
    }

  // String encoder escapes any embedded `QUOTE` characters.
  implicit val stringInstance: Text[String] =
    new Text[String] {

      // Standard char encodings that don't differ in array context
      def stdChar(c: Char, sb: StringBuilder): StringBuilder =
        c match {
          case '\b' => sb.append("\\b")
          case '\f' => sb.append("\\f")
          case '\n' => sb.append("\\n")
          case '\r' => sb.append("\\r")
          case '\t' => sb.append("\\t")
          case 0x0b => sb.append("\\v")
          case c    => sb.append(c.toChar)
        }

      def unsafeEncode(s: String, sb: StringBuilder) =
        s.foreach {
          case '\\' => sb.append("\\\\") // backslash must be doubled
          case c    => stdChar(c, sb)
        }

      // I am not confident about this encoder. Postgres seems not to be able to cope with low
      // control characters or high whitespace characters so these are simply filtered out in the
      // tests. It should accommodate arrays of non-pathological strings but it would be nice to
      // have a complete specification of what's actually happening.
      override def unsafeArrayEncode(s: String, sb: StringBuilder) = {
        sb.append('"')
        s.foreach {
          case '\"' => sb.append("\\\\\"")
          case '\\' => sb.append("\\\\\\\\") // srsly
          case c    => stdChar(c, sb)
        }
        sb.append('"')
        ()
      }
    }

  // Char
  implicit val charInstance: Text[Char] = instance((n, sb) => { sb.append(n.toString); () })

  // Primitive Numerics
  implicit val intInstance: Text[Int] = instance((n, sb) => { sb.append(n); () })
  implicit val shortInstance: Text[Short] = instance((n, sb) => { sb.append(n); () })
  implicit val longInstance: Text[Long] = instance((n, sb) => { sb.append(n); () })
  implicit val floatInstance: Text[Float] = instance((n, sb) => { sb.append(n); () })
  implicit val doubleInstance: Text[Double] = instance((n, sb) => { sb.append(n); () })

  // Big Numerics
  implicit val bigDecimalInstance: Text[BigDecimal] = instance { (n, sb) => { sb.append(n.toString); () } }

  // Boolean
  implicit val booleanInstance: Text[Boolean] =
    instance((b, sb) => { sb.append(b); () })

  // Date, Time, etc.

  // Byte arrays in \\x01A3DD.. format.
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  implicit val byteArrayInstance: Text[Array[Byte]] =
    instance { (bs, sb) =>
      sb.append("\\\\x")
      if (bs.length > 0) {
        val hex = BigInt(1, bs).toString(16)
        val pad = bs.length * 2 - hex.length
        (0 until pad).foreach(_ => sb.append("0"))
        sb.append(hex)
        ()
      }
    }

  // Any non-option Text can be lifted to Option
  implicit def option[A](
      implicit csv: Text[A]
  ): Text[Option[A]] =
    instance {
      case (Some(a), sb) => { csv.unsafeEncode(a, sb); () }
      case (None, sb)    => { sb.append(Text.NULL); () }
    }

}

trait TextInstances0 extends TextInstances1 { this: Text.type =>

  // Iterable and views thereof, as [nested] ARRAY
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.Var"))
  implicit def iterableInstance[F[_], A](
      implicit
      ev: Text[A],
      f: F[A] => Iterable[A]
  ): Text[F[A]] =
    instance { (fa, sb) =>
      var first = true
      sb.append("{")
      f(fa).foreach { a =>
        if (first) first = false
        else sb.append(',')
        ev.unsafeArrayEncode(a, sb)
      }
      sb.append('}')
      ()
    }

}

trait TextInstances1 { this: Text.type =>

  // Foldable, not as fast
  implicit def foldableInstance[F[_]: Foldable, A](
      implicit ev: Text[A]
  ): Text[F[A]] =
    iterableInstance[List, A].contramap(_.toList)

}
