## Selecting Data

In this chapter we will write some some programs to read from the database, mapping rows to Scala types on the way. We also introduce YOLO mode for experimenting with **doobie** in the REPL.

### Setting Up

First let's get our imports out of the way and set up a `Transactor` as we did before. You can skip this step if you still have your REPL running from last chapter.

```scala mdoc:silent
import doobie._
import doobie.implicits._
import doobie.util.ExecutionContexts
import cats._
import cats.data._
import cats.effect._
import cats.implicits._
import fs2.Stream

// This is just for testing. Consider using cats.effect.IOApp instead of calling
// unsafe methods directly.
import cats.effect.unsafe.implicits.global

// A transactor that gets connections from java.sql.DriverManager and executes blocking operations
// on our synchronous EC. See the chapter on connection handling for more info.
val xa = Transactor.fromDriverManager[IO](
  driver = "org.postgresql.Driver",  // JDBC driver classname
  url = "jdbc:postgresql:world",     // Connect URL
  user = "postgres",                 // Database user name
  password = "password",             // Database password
  logHandler = None                  // Don't setup logging for now. See Logging page for how to log events in detail
)
```

```scala mdoc:invisible
implicit val mdocColors: doobie.util.Colors = doobie.util.Colors.None
```

We will be playing with the `country` table, shown here for reference. If you don't have the `world` database set up, go back to the [Introduction](01-Introduction.html) for instructions.

```sql
CREATE TABLE country (
  code       character(3)  NOT NULL,
  name       text          NOT NULL,
  population integer       NOT NULL,
  gnp        numeric(10,2)
  -- more columns, but we won't use them here
)
```

### Reading Rows into Collections

For our first query let's aim low and select some country names into a `List`, then print out the first few. There are several steps here so we have noted the types along the way.

```scala mdoc
sql"select name from country"
  .query[String]    // Query0[String]
  .to[List]         // ConnectionIO[List[String]]
  .transact(xa)     // IO[List[String]]
  .unsafeRunSync()    // List[String]
  .take(5)          // List[String]
  .foreach(println) // Unit
```

Let's break this down a bit.

- `sql"select name from country".query[String]` defines a `Query0[String]`, which is a one-column query that maps each returned row to a `String`. We will get to more interesting row types soon.
- `.to[List]` is a convenience method that accumulates rows into a `List`, in this case yielding a `ConnectionIO[List[String]]`. It works with any collection type that has a `CanBuildFrom`. Similar methods are:
    - `.unique` which returns a single value, raising an exception if there is not exactly one row returned.
    - `.option` which returns an `Option`, raising an exception if there is more than one row returned.
    - `.nel` which returns an `NonEmptyList`, raising an exception if there are no rows returned.
    - See the Scaladoc for `Query0` for more information on these and other methods.
- The rest is familar; `transact(xa)` yields a `IO[List[String]]` which we run, giving us a normal Scala `List[String]` that we print out.

### Internal Streaming

The example above is ok, but there's not much point reading all the results from the database when we only want the first five rows. So let's try a different approach.

```scala mdoc
sql"select name from country"
  .query[String]    // Query0[String]
  .stream           // Stream[ConnectionIO, String]
  .take(5)          // Stream[ConnectionIO, String]
  .compile.toList   // ConnectionIO[List[String]]
  .transact(xa)     // IO[List[String]]
  .unsafeRunSync()    // List[String]
  .foreach(println) // Unit
```

The difference here is that `stream` gives us an [fs2](https://github.com/functional-streams-for-scala/fs2) `Stream[ConnectionIO, String]`
that emits rows as they arrive from the database. By applying `take(5)` we instruct the stream to shut everything down (and clean everything up) after five elements have been emitted. This is much more efficient than pulling all 239 rows and then throwing most of them away.

Of course a server-side `LIMIT` would be an even better way to do this (for databases that support it), but in cases where you need client-side filtering or other custom postprocessing, `Stream` is a very general and powerful tool.
For more information see the [fs2](https://github.com/functional-streams-for-scala/fs2) repo, which has a good list of learning resources.

### YOLO Mode

The API we have seen so far is ok, but it's tiresome to keep saying `transact(xa)` and doing `foreach(println)` to see what the results look like. So **just for REPL exploration** there is a module of extra syntax provided on your `Transactor` that you can import.

```scala mdoc:silent
import doobie.util.yolo.Yolo
val y: Yolo[IO] = xa.yolo // a stable reference is required
import y._
```

We can now run our previous query in an abbreviated form.

```scala mdoc
sql"select name from country"
  .query[String] // Query0[String]
  .stream        // Stream[ConnectionIO, String]
  .take(5)       // Stream[ConnectionIO, String]
  .quick         // IO[Unit]
  .unsafeRunSync()
```

This syntax allows you to quickly run a `Query0[A]` or `Stream[ConnectionIO, A]` and see the results printed to the console. This isn't a huge deal but it can save you some keystrokes when you're just messing around.

- The `.quick` method sinks the stream to standard out (adding ANSI coloring for fun) and then calls `.transact`, yielding a `IO[Unit]`.
- The `.check` method returns a `IO[Unit]` that performs a metadata analysis on the provided query and asserted types and prints out a report. This is covered in detail in the chapter on typechecking queries.

### Multi-Column Queries

We can select multiple columns, of course, and map them to a tuple. The `gnp` column in our table is nullable so we'll select that one into an `Option[Double]`. In a later chapter we'll see how to check the types to be sure they're sensible.

```scala mdoc
sql"select code, name, population, gnp from country"
  .query[(String, String, Int, Option[Double])]
  .stream
  .take(5)
  .quick
  .unsafeRunSync()
```
**doobie** supports row mappings for atomic column types, as well as options, tuples, `HList`s, shapeless records, and case classes thereof. So let's try the same query with an `HList`:

```scala mdoc
import shapeless._

sql"select code, name, population, gnp from country"
  .query[String :: String :: Int :: Option[Double] :: HNil]
  .stream
  .take(5)
  .quick
  .unsafeRunSync()
```

And with a shapeless record:

```scala mdoc
import shapeless.record.Record

type Rec = Record.`'code -> String, 'name -> String, 'pop -> Int, 'gnp -> Option[Double]`.T

sql"select code, name, population, gnp from country"
  .query[Rec]
  .stream
  .take(5)
  .quick
  .unsafeRunSync()
```

And again, mapping rows to a case class.

```scala mdoc:silent
case class Country(code: String, name: String, pop: Int, gnp: Option[Double])
```

```scala mdoc
sql"select code, name, population, gnp from country"
  .query[Country]
  .stream
  .take(5)
  .quick
  .unsafeRunSync()
```

You can also nest case classes, `HList`s, shapeless records, and/or tuples arbitrarily as long as the eventual members are of supported columns types. For instance, here we map the same set of columns to a tuple of two case classes:

```scala mdoc:silent
case class Code(code: String)
case class Country2(name: String, pop: Int, gnp: Option[Double])
```

```scala mdoc
sql"select code, name, population, gnp from country"
  .query[(Code, Country2)]
  .stream
  .take(5)
  .quick
  .unsafeRunSync()
```

And just for fun, since the `Code` values are constructed from the primary key, let's turn the results into a `Map`. Trivial but useful.

```scala mdoc
sql"select code, name, population, gnp from country"
  .query[(Code, Country2)]
  .stream.take(5)
  .compile.toList
  .map(_.toMap)
  .quick
  .unsafeRunSync()
```

### Final Streaming

In the examples above we construct a `Stream[ConnectionIO, A]` and discharge it via `.compile.toList`, yielding a `ConnectionIO[List[A]]` which eventually becomes a `IO[List[A]]`. So the construction and execution of the `Stream` is entirely internal to the **doobie** program.

However in some cases a stream is what we want as our "top level" type. For example, [http4s](https://github.com/http4s/http4s) can use a `Stream[IO, A]` directly as a response type, which could allow us to stream a resultset directly to the network socket. We can achieve this in **doobie** by calling `transact` directly on the `Stream[ConnectionIO, A]`.

```scala mdoc
val p: Stream[IO, Country2] = {
  sql"select name, population, gnp from country"
    .query[Country2] // Query0[Country2]
    .stream          // Stream[ConnectionIO, Country2]
    .transact(xa)    // Stream[IO, Country2]
}

p.take(5).compile.toVector.unsafeRunSync().foreach(println)
```

### Diving Deeper

The `sql` interpolator is sugar for constructors defined in the `doobie.hi.connection` module, aliased as `HC` if you use the standard imports. Using these constructors directly, the above program would look like this:

```scala mdoc
import doobie.hi.HC
import doobie.free.{FC, FPS}
import doobie.util.log.{LoggingInfo, Parameters}

val sql = "select code, name, population, gnp from country"
val proc = HC.stream[(Code, Country2)](
  create = FC.prepareStatement(sql), // Create prepared statement
  prep = FPS.unit,                   // prepare steps (none)
  exec = FPS.executeQuery,           // execute the query
  chunkSize = 512,                   // chunk size
  loggingInfo = LoggingInfo(
    sql = sql,
    params = Parameters.NonBatch(List.empty), // No parameters
    label = "unlabeled"
  )
)

proc.take(5)        // Stream[ConnectionIO, (Code, Country2)]
    .compile.toList // ConnectionIO[List[(Code, Country2)]]
    .map(_.toMap)   // ConnectionIO[Map[Code, Country2]]
    .quick
    .unsafeRunSync()
```

The `stream` combinator is parameterized on the element type and consumes a statement and a program in `PreparedStatementIO` that sets input parameters and any other pre-execution configuration. In this case the "prepare" program is a no-op.
