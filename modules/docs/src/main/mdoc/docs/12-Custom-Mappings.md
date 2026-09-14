## Custom Mappings

**doobie** provides two families of abstractions that define mappings between Scala types and schema types. These are used when we pass query arguments to the database, and when we interpret results that come back. Many such mappings are provided for free but it is sometimes necessary to define your own, and this is the subject of this chapter.

The most common kind of custom mapping operates on single column values, so we will examine this kind of mapping first. We will then talk about column vector mappings for "wide" structures.

## Setup

In this chapter we're importing the essentials from Cats and **doobie**, as well as some other odds and ends we'll discuss below.

```scala mdoc:silent
import cats._, cats.data._, cats.implicits._
import org.typelevel.doobie._
import org.typelevel.doobie.implicits._
import io.circe._, io.circe.jawn._, io.circe.syntax._
import java.awt.Point
import org.postgresql.util.PGobject
```

## When do I need a custom type mapping?

Your first evidence that you need a new type mapping will likely be a type error. There are two common cases. The first case appears when you try to use an unmapped type as a statement parameter.

```scala mdoc:fail
def nope(msg: String, ex: Exception): ConnectionIO[Long] =
  sql"INSERT INTO log (message, detail) VALUES ($msg, $ex)".update.run
```

The second common case is when we try to read rows into a data type that includes an unmapped member type, such as this one.

```scala mdoc:silent
case class LogEntry(msg: String, ex: Exception)
```

When we attempt to define a `Query0[LogEntry]` we get a type error similar to the one above.

```scala
sql"SELECT message, detail FROM log".query[LogEntry]
```

In both cases some hints are provided and refer you to this very chapter! So let's keep on going and look at type mappings in detail.

## Single-Column Type Mappings

JDBC defines mappings between JVM types like `Int` and `String` and standard schema types like `INTEGER` and `VARCHAR`. These suffice for most cases, and **doobie** provides them out of the box. We abstract over such mappings via the `Get` and `Put` typeclasses.

- `Get[A]` describes a mapping from some non-nullable schema type to Scala type `A`, and from the equivalent nullable schema type to `Option[A]`. This lets us read column values and statement return values.
- `Put[A]` describes a mapping from Scala type `A` to some non-nullable schema type, and from `Option[A]` to the equivalent nullable schema type. This lets us set statement parameters and update columns.

Instances are provided for the following Scala types:

- JVM numeric types `Byte`, `Short`, `Int`, `Long`, `Float`, and `Double`;
- `BigDecimal` (both Java and Scala versions);
- `Boolean`, `String`, and `Array[Byte]`;
- `Date`, `Time`, and `Timestamp` from the `java.sql` package;
- `Date` from the `java.util` package;
- `Instant`, `LocalDate`, `LocalTime`, `LocalDateTime`, `OffsetTime` and `OffsetDateTime` from the `java.time` package; and
- single-element case classes wrapping one of the above types.

The `java.time` instances may require a separate import , dependent on your Database Driver . See the [doobie-faq](https://typelevel.org/doobie/docs/17-FAQ.html#how-do-i-use-java-time-types-with-doobie-) for details

The above cases are defined by the JDBC specification. See later chapters on vendor-specific additions, which provide mappings for some non-standard types such as `UUID`s and network addresses.

#### Deriving Get and Put from Existing Instances

If we don't have the `Get` or `Put` instance we need, we can often make one from an existing instance. Consider here a type `Nat` of natural numbers, along with a conversion to and from `Int`.

```scala mdoc:silent
object NatModule {

  sealed trait Nat
  case object Zero extends Nat
  case class  Succ(n: Nat) extends Nat

  def toInt(n: Nat): Int = {
    def go(n: Nat, acc: Int): Int =
      n match {
        case Zero    => acc
        case Succ(n) => go(n, acc + 1)
      }
    go(n, 0)
  }

  def fromInt(n: Int): Nat = {
    def go(n: Int, acc: Nat): Nat =
      if (n <= 0) acc else go(n - 1, Succ(acc))
    go(n, Zero)
  }

}
import NatModule._
```

There is no direct schema mapping for `Nat`, but there *is* a schema mapping for `Int` that we get out of the box, and we can use it to define our mapping for `Nat`.

```scala mdoc:silent
// Bidirectional schema mapping for Nat, in terms of Int
implicit val natGet: Get[Nat] = Get[Int].map(fromInt)
implicit val natPut: Put[Nat] = Put[Int].contramap(toInt)
```

The `.map` and `.contramap` methods conform with the signatures of `Functor` and `Contravariant`, and indeed `Get` and `Put` are instances, respectively. However it's best to use the tagged versions `.tmap` and `.tcontramap` when possible because it makes the name of the type ("Nat" in this case) available to the query checker by requiring a `TypeTag` for the mapped type. This isn't always practical but it can result in better diagnostic messages so it should be preferred.

```scala mdoc:silent
// Prefer .tmap and .tcontramap when possible.
implicit val natGet2: Get[Nat] = Get[Int].tmap(fromInt)
implicit val natPut2: Put[Nat] = Put[Int].tcontramap(toInt)
```

#### Deriving Get and Put from Meta

Because it is common to define bidirectional mappings there is also a `Meta` typeclass which serves to introduce both a `Get` and `Put` into implicit scope. If you have an implicit `Meta[A]` then you get an implicit `Put[A]` and a `Get[A]` for free.

Because a `Meta` instance exists for `Int` and other base types this is often the most convenient way to define a bidirectional mapping.

```scala mdoc:silent
// Bidirectional schema mapping for Nat, in terms of Int
implicit val natMeta: Meta[Nat] = Meta[Int].imap(fromInt)(toInt)
```

And as above, prefer `.timap` when possible.

```scala mdoc:silent
// Prefer .timap when possible.
implicit val natMeta2: Meta[Nat] = Meta[Int].timap(fromInt)(toInt)
```

**Note:** it is important to understand that `Meta` exists only to introduce `Get`/`Put` pairs into implicit scope. You should never demand `Meta` as evidence in user code: instead demand `Get`, `Put`, or both.

```scala
def foo[A: Meta](...)     // don't do this
def foo[A: Get: Put](...) // ok
```

#### Defining Get and Put for Exotic Types

In rare cases it is not possible to define a new mapping in terms of primitive JDBC types because the underlying schema type is vendor-specific or otherwise not part of the JDBC specification. In these cases it is necessary to define mappings explicitly.

In this example we will create a mapping for PostgreSQL's `json` type, which is not part of the JDBC specification. On the Scala side we will use the `Json` type from [Circe](https://github.com/circe/circe). The PostgreSQL JDBC driver transfers `json` values via the JDBC type `OTHER`, with an uwrapped payload type `PGobject`. The only way to know this is by experimentation. You can expect to get this kind of mapping wrong a few times before it starts working. In any case the `OTHER` type is commonly used for nonstandard types and **doobie** provides a way to construct such mappings.

```scala mdoc:silent
implicit val showPGobject: Show[PGobject] = Show.show(_.getValue.take(250))

implicit val jsonGet: Get[Json] =
  Get.Advanced.other[PGobject](NonEmptyList.of("json")).temap[Json] { o =>
    parse(o.getValue).leftMap(_.show)
  }
```

In the instance above we read a value via JDBC's `getOther` with schema type `json`, cast the result to `PGobject`, then parse its value (a `String`), returning the parse exception as a string on failure. Consider for a moment how comically optimistic this is. Many things have to work in order to get a `Json` value in hand, and if anything fails it's an unrecoverable error. Effective testing is essential when defining new mappings like this.

The `Put` instance is less error-prone since we know the `Json` we start with is valid. Here we construct and return a new `PGobject` whose schema type and string value are filled in explicitly.

```scala mdoc:silent
implicit val jsonPut: Put[Json] =
  Put.Advanced.other[PGobject](NonEmptyList.of("json")).tcontramap[Json] { j =>
      val o = new PGobject
      o.setType("json")
      o.setValue(j.noSpaces)
      o
  }
```

As above, with bidirectional mappings it's usually more convenient to use `Meta`, which provides an `other` constructor allowing the operations above to be combined.

```scala mdoc:silent
implicit val jsonMeta: Meta[Json] =
  Meta.Advanced.other[PGobject]("json").timap[Json](
    a => parse(a.getValue).leftMap[Json](e => throw e).merge)(
    a => {
      val o = new PGobject
      o.setType("json")
      o.setValue(a.noSpaces)
      o
    }
  )
```

There are similar constructors for array types and other possibilities, but `other` is by far the most common in user code and we won't discuss the others here. See the Scaladoc for more information.

## Derivation for Scala 3 enums

You can derive `Get` and `Put` instances for Scala 3 enums using the `Get.deriveEnumString` or `Put.deriveEnumString`.

```scala
enum Color:
  case Red, Green, Blue

object Color:
  given Get[Color] = Get.deriveEnumString[Color]
  given Put[Color] = Put.deriveEnumString[Color]

fr"SELECT 'Red'".query[Color].unique
```

As shown above, 

- The instances need to be explicitly derived
- The string values converts from/to are the literal name of the enum cases

## Column Vector Mappings

The `Get` and `Put` typeclasses described above define mappings between Scala types and single-column schema types, however in general we need more than this. Queries return **heterogeneous vectors** of values that we wish to map to composite Scala data types, and similarly we may wish to map a composite Scala data type to a heterogeneous vector of schema values (when setting a `VALUES` clause in an update, for instance). Mappings for these "wide" data types are provided by the `Read` and `Write` typeclasses.

- `Read[A]` describes a mapping from some vector of schema types to Scala type `A`. This lets us read rows as composite values.
- `Write[A]` describes a mapping from Scala type `A` to some vector of schema types. This lets us set multiple statement parameters.

As `Read` and `Write` instances are [logically] built from vectors of `Get` and `Put` instances we can construct them automatically in almost all cases. The base cases are:

- We can `Read` and `Write` the zero-width types `Unit` and `HNil`.
- We can `Read` or `Write` single-column types that have `Get` or `Put` instance, respectively; as well as `Option`s thereof.

The inductive cases that build on the base cases above are:

- We can `Read` or `Write` a shapeless `HList` if its elements can be read or written, respectively.
- We can `Read` or `Write` a shapeless record if its values can be read or written, respectively.
- We can `Read` or `Write` a product type (case class or tuple) if its shapeless `Generic` representation (i.e., its fields as an `HList`) can be read or written, respectively.

In addition, **doobie** provides `Read[Option[A]]` and `Write[Option[A]]` in the three cases above, mapping all columns to *nullable* schema types. This allows you to map the columns from an `OUTER JOIN` to an *optional* data type. For instance, reading parent/child pairs we might map output rows to the type `(Parent, Option[Child])`.

The above rules allow you to map between column/parameter vectors and [nested] tuples and case classes, which covers most use cases.

#### Deriving Read and Write from Existing Instances

Although automatic derivation will suffice in most cases, it does not work with traits and non-case classes. In these cases we must provide a mapping between the unruly data type and a type that has a defined mapping.

Consider the `Point` class from Java AWT, which is logically a pair of `Int`s but is not a case class and is thus not eligible for automatic derivation of `Read` and `Write` instances. We can define these by hand by mapping to and from the Scala type `(Int, Int)` which *does* have automatically-derived instances.

```scala mdoc
implicit val pointRead: Read[Point] =
  Read[(Int, Int)].map { case (x, y) => new Point(x, y) }

implicit val pointWrite: Write[Point] =
  Write[(Int, Int)].contramap(p => (p.x, p.y))
```

There is no equivalent to `Meta` for bidirectional column vector mappings.
