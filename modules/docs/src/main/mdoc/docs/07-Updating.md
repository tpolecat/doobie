## DDL, Inserting, and Updating

In this chapter we examine operations that modify data in the database, and ways to retrieve the results of these updates.

### Setting Up

Again we set up a transactor and pull in YOLO mode, but this time we're not using the world database.

```scala mdoc:silent
import org.typelevel.doobie._
import org.typelevel.doobie.implicits._
import org.typelevel.doobie.util.ExecutionContexts
import cats._
import cats.data._
import cats.effect._
import cats.implicits._

// This is just for testing. Consider using cats.effect.IOApp instead of calling
// unsafe methods directly.
import cats.effect.unsafe.implicits.global

// A transactor that gets connections from java.sql.DriverManager and executes blocking operations
// on an our synchronous EC. See the chapter on connection handling for more info.
val xa = Transactor.fromDriverManager[IO](
  driver = "org.postgresql.Driver",  // JDBC driver classname
  url = "jdbc:postgresql:world",     // Connect URL - Driver specific
  user = "postgres",                 // Database user name
  password = "password",             // Database password
  logHandler = None                  // Don't setup logging for now. See Logging page for how to log events in detail

)

val y = xa.yolo
import y._
```

```scala mdoc:invisible
implicit val mdocColors: org.typelevel.doobie.util.Colors = org.typelevel.doobie.util.Colors.None
```

### Data Definition

It is uncommon to define database structures at runtime, but **doobie** handles it just fine and treats such operations like any other kind of update. And it happens to be useful here!

Let's create a new table, which we will use for the examples to follow. This looks a lot like our prior usage of the `sql` interpolator, but this time we're using `update` rather than `query`. The `.run` method gives a `ConnectionIO[Long]` that yields the total number of rows modified, and the YOLO-mode `.quick` gives a `IO[Unit]` that prints out the row count.

```scala mdoc:silent
val drop =
  sql"""
    DROP TABLE IF EXISTS person
  """.update.run

val create =
  sql"""
    CREATE TABLE person (
      id   SERIAL,
      name VARCHAR NOT NULL UNIQUE,
      age  SMALLINT
    )
  """.update.run
```

We can compose these and run them together, yielding the total number of affected rows.

```scala mdoc
(drop, create).mapN(_ + _).transact(xa).unsafeRunSync()
```


### Inserting


Inserting is straightforward and works just as with selects. Here we define a method that constructs an `Update0` that inserts a row into the `person` table.

```scala mdoc:silent
def insert1(name: String, age: Option[Short]): Update0 =
  sql"insert into person (name, age) values ($name, $age)".update
```

Let's insert a few rows.

```scala mdoc
insert1("Alice", Some(12)).run.transact(xa).unsafeRunSync()
insert1("Bob", None).quick.unsafeRunSync() // switch to YOLO mode
```

And read them back.

```scala mdoc:silent
case class Person(id: Long, name: String, age: Option[Short])
```

```scala mdoc
sql"select id, name, age from person".query[Person].quick.unsafeRunSync()
```


### Updating


Updating follows the same pattern. Here we update Alice's age.

```scala mdoc
sql"update person set age = 15 where name = 'Alice'".update.quick.unsafeRunSync()
sql"select id, name, age from person".query[Person].quick.unsafeRunSync()
```

### Retrieving Results

When we insert we usually want the new row back, so let's do that. First we'll do it the hard way, by inserting, getting the last used key via `lastVal()`, then selecting the indicated row.

```scala mdoc:silent
def insert2(name: String, age: Option[Short]): ConnectionIO[Person] =
  for {
    _  <- sql"insert into person (name, age) values ($name, $age)".update.run
    id <- sql"select lastval()".query[Long].unique
    p  <- sql"select id, name, age from person where id = $id".query[Person].unique
  } yield p
```

```scala mdoc
insert2("Jimmy", Some(42)).quick.unsafeRunSync()
```

This is irritating but it is supported by all databases (although the "get the last used id" function will vary by vendor).

Some database (like H2) allow you to return [only] the inserted id, allowing the above operation to be reduced to two statements (see below for an explanation of `withUniqueGeneratedKeys`).

```scala mdoc:silent
def insert2_H2(name: String, age: Option[Short]): ConnectionIO[Person] =
  for {
    id <- sql"insert into person (name, age) values ($name, $age)"
            .update
            .withUniqueGeneratedKeys[Int]("id")
    p  <- sql"select id, name, age from person where id = $id"
            .query[Person]
            .unique
  } yield p
```

```scala mdoc
insert2_H2("Ramone", Some(42)).quick.unsafeRunSync()
```

Other databases (including PostgreSQL) provide a way to do this in one shot by returning multiple specified columns from the inserted row.

```scala mdoc:silent
def insert3(name: String, age: Option[Short]): ConnectionIO[Person] = {
  sql"insert into person (name, age) values ($name, $age)"
    .update
    .withUniqueGeneratedKeys("id", "name", "age")
}
```

The `withUniqueGeneratedKeys` specifies that we expect exactly one row back (otherwise an exception will be raised), and requires a list of columns to return. This isn't the most beautiful API but it's what JDBC gives us. And it does work.

```scala mdoc
insert3("Elvis", None).quick.unsafeRunSync()
```

This mechanism also works for updates, for databases that support it. In the case of multiple row updates we omit `unique` and get a `Stream[ConnectionIO, Person]` back.


```scala mdoc:silent
val up = {
  sql"update person set age = age + 1 where age is not null"
    .update
    .withGeneratedKeys[Person]("id", "name", "age")
}
```

Running this process updates all rows with a non-`NULL` age and returns them.

```scala mdoc
up.quick.unsafeRunSync()
up.quick.unsafeRunSync() // and again!
```

### Batch Updates

**doobie** supports batch updating via the `updateMany` and `updateManyWithGeneratedKeys` operations on the `Update` data type (which we haven't seen before). An `Update0`, which is the type of an `sql"...".update` expression, represents a parameterized statement where the arguments are known. An `Update[A]` is more general, and represents a parameterized statement where the composite argument of type `A` is *not* known.

```scala mdoc:silent
// Given some values ...
val a = 1; val b = "foo"

// this expression ...
sql"... $a $b ..."

// is syntactic sugar for this one, which is an Update applied to (a, b)
Update[(Int, String)]("... ? ? ...").run((a, b))
```

By using an `Update` directly we can apply *many* sets of arguments to the same statement, and execute it as a single batch operation.

```scala mdoc:silent
type PersonInfo = (String, Option[Short])

def insertMany(ps: List[PersonInfo]): ConnectionIO[Long] = {
  val sql = "insert into person (name, age) values (?, ?)"
  Update[PersonInfo](sql).updateMany(ps)
}

// Some rows to insert
val data = List[PersonInfo](
  ("Frank", Some(12)),
  ("Daddy", None))
```

Running this program yields the number of updated rows.

```scala mdoc
insertMany(data).quick.unsafeRunSync()
```

For databases that support it (such as PostgreSQL) we can use `updateManyWithGeneratedKeys` to return a stream of updated rows.

```scala mdoc:silent
import fs2.Stream

def insertMany2(ps: List[PersonInfo]): Stream[ConnectionIO, Person] = {
  val sql = "insert into person (name, age) values (?, ?)"
  Update[PersonInfo](sql).updateManyWithGeneratedKeys[Person]("id", "name", "age")(ps)
}

// Some rows to insert
val data2 = List[PersonInfo](
  ("Banjo",   Some(39)),
  ("Skeeter", None),
  ("Jim-Bob", Some(12)))
```

Running this program yields the updated instances.

```scala mdoc
insertMany2(data2).quick.unsafeRunSync()
```
