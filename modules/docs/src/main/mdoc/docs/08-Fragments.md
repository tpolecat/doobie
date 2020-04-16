## Statement Fragments

In this chapter we discuss how to construct SQL statements at runtime.

### Setting Up

Same as last chapter, so if you're still set up you can skip this section. Otherwise let's set up a `Transactor` and YOLO mode.

```scala mdoc:silent
import doobie._
import doobie.implicits._
import doobie.util.ExecutionContexts
import cats._
import cats.data._
import cats.effect._
import cats.implicits._

// We need a ContextShift[IO] before we can construct a Transactor[IO]. The passed ExecutionContext
// is where nonblocking operations will be executed. For testing here we're using a synchronous EC.
implicit val cs = IO.contextShift(ExecutionContexts.synchronous)

// A transactor that gets connections from java.sql.DriverManager and executes blocking operations
// on an our synchronous EC. See the chapter on connection handling for more info.
val xa = Transactor.fromDriverManager[IO](
  "org.postgresql.Driver",     // driver classname
  "jdbc:postgresql:world",     // connect URL (driver-specific)
  "postgres",                  // user
  "",                          // password
  Blocker.liftExecutionContext(ExecutionContexts.synchronous) // just for testing
)

val y = xa.yolo
import y._
```

```scala mdoc:invisible
implicit val mdocColors: doobie.util.Colors = doobie.util.Colors.None
```

We're still playing with the `country` table, shown here for reference.

```sql
CREATE TABLE country (
  code       character(3)  NOT NULL,
  name       text          NOT NULL,
  population integer       NOT NULL,
  gnp        numeric(10,2)
  -- more columns, but we won't use them here
)
```

### Composing SQL literals

You can construct a SQL `Fragment` using the `fr` interpolator, which behaves just like the `sql` interpolator. Fragments are concatenated with `++`.

```scala mdoc
val a = fr"select name from country"
val b = fr"where code = 'USA'"
val c = a ++ b // concatenation by ++
c.query[String].unique.quick.unsafeRunSync
```

Fragments can capture arguments of any type with a `Put` instance, just as the `sql` interpolator does.

```scala mdoc
def whereCode(s: String) = fr"where code = $s"
val fra = whereCode("FRA")
(fr"select name from country" ++ fra).query[String].quick.unsafeRunSync
```

You can lift an arbitrary string value via `Fragment.const`, which allows you to parameterize on things that aren't valid SQL parameters.

```scala mdoc
def count(table: String) = (fr"select count(*) from" ++ Fragment.const(table)).query[Int].unique
count("city").quick.unsafeRunSync
```

> Note that `Fragment.const` performs no escaping of passed strings. Passing user-supplied data is an **injection risk**.

### Whitespace handling

The rendered SQL string for a `fr` or `const` fragment will have a single space character appended, which is usually what you want. Normally you don't need to worry about whitespace when composing fragments.

If you do *not* want a fragment to have trailing space you can use the `fr0` interpolator or `const0` constructor. This is used here and there in the `Fragments` module to yield prettier SQL strings.

```scala mdoc
fr"IN (" ++ List(1, 2, 3).map(n => fr"$n").intercalate(fr",") ++ fr")"
fr0"IN (" ++ List(1, 2, 3).map(n => fr0"$n").intercalate(fr",") ++ fr")"
```
Note that the `sql` interpolator is simply an alias for `fr0`.

### The `Fragments` Module

The `Fragments` module provides some combinators for common patterns when working with fragments. The following example illustrates a few of them. See the Scaladoc or source for more information.

Here we define a query with a three optional filter conditions.

```scala mdoc:silent
// Import some convenience combinators.
import Fragments.{ in, whereAndOpt }

// Country Info
case class Info(name: String, code: String, population: Int)

// Construct a Query0 with some optional filter conditions and a configurable LIMIT.
def select(name: Option[String], pop: Option[Int], codes: List[String], limit: Long) = {

  // Three Option[Fragment] filter conditions.
  val f1 = name.map(s => fr"name LIKE $s")
  val f2 = pop.map(n => fr"population > $n")
  val f3 = codes.toNel.map(cs => in(fr"code", cs))

  // Our final query
  val q: Fragment[Any] =
    fr"SELECT name, code, population FROM country" ++
    whereAndOpt(f1, f2, f3)                         ++
    fr"LIMIT $limit"

  // Construct a Query0
  q.query[Info]

}
```

We first construct three optional filters, the third of which uses the `in` combinator to construct an SQL `IN` clause. The final statement uses the `whereAndOpt` combinator that constructs a `WHERE` clause with the passed sequence of `Option[Fragment]` joined with `AND` if any are defined, otherwise it evaluates to the empty fragment. The end result is that the `WHERE` clause appears only if at least one filter is defined.

Let's look at a few possibilities.

```scala mdoc
select(None, None, Nil, 10).check.unsafeRunSync // no filters
select(Some("U%"), None, Nil, 10).check.unsafeRunSync // one filter
select(Some("U%"), Some(12345), List("FRA", "GBR"), 10).check.unsafeRunSync // three filters
```
