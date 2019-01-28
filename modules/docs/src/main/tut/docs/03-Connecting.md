---
layout: docs
number: 3
title: Connecting to a Database
---

## {{page.title}}

In this chapter we start from the beginning. First we write a program that connects to a database and returns a value, and then we run that program in the REPL. We also touch on composing small programs to construct larger ones.

### Our First Program

Before we can use **doobie** we need to import some symbols. We will use package imports here as a convenience; this will give us the most commonly-used symbols when working with the high-level API.

```tut:silent
import doobie._
import doobie.implicits._
```

Let's also bring in Cats.

```tut:silent
import cats._
import cats.effect._
import cats.implicits._
```

In the **doobie** high level API the most common types we will deal with have the form `ConnectionIO[A]`, specifying computations that take place in a context where a `java.sql.Connection` is available, ultimately producing a value of type `A`.

So let's start with a `ConnectionIO` program that simply returns a constant.

```tut
val program1 = 42.pure[ConnectionIO]
```

This is a perfectly respectable **doobie** program, but we can't run it as-is; we need a `Connection` first. There are several ways to do this, but here let's use a `Transactor`.

```tut:silent
import scala.concurrent.ExecutionContext

// We need a ContextShift[IO] before we can construct a Transactor[IO]. The passed ExecutionContext
// is where nonblocking operations will be executed.
implicit val cs = IO.contextShift(ExecutionContext.global)

// A transactor that gets connections from java.sql.DriverManager and executes blocking operations
// on an unbounded pool of daemon threads. See the chapter on connection handling for more info.
val xa = Transactor.fromDriverManager[IO](
  "org.postgresql.Driver", // driver classname
  "jdbc:postgresql:world", // connect URL (driver-specific)
  "postgres",              // user
  ""                       // password
)
```

A `Transactor` is a data type that knows how to connect to a database, hand out connections, and clean them up; and with this knowledge it can transform `ConnectionIO ~> IO`, which gives us a program we can run. Specifically it gives us an `IO` that, when run, will connect to the database and execute single transaction.

We are using `cats.effect.IO` as our final effect type, but you can use any monad `M[_]` given `cats.effect.Async[M]`. See *Using Your Own Target Monad* at the end of this chapter for more details.

The `DriverManagerTransactor` simply delegates to the `java.sql.DriverManager` to allocate connections, which is fine for development but inefficient for production use. In a later chapter we discuss other approaches for connection management.

And here we go.

```tut
val io = program1.transact(xa)
io.unsafeRunSync
```

Hooray! We have computed a constant. It's not very interesting because we never ask the database to perform any work, but it's a first step.

> Keep in mind that all the code in this book is pure *except* the calls to `IO.unsafeRunSync`, which is the "end of the world" operation that typically appears only at your application's entry points. In the REPL we use it to force a computation to "happen".

Right. Now let's try something more interesting.

### Our Second Program

Now let's use the `sql` string interpolator to construct a query that asks the *database* to compute a constant. We will cover this construction in great detail later on, but the meaning of `program2` is "run the query, interpret the resultset as a stream of `Int` values, and yield its one and only element."

```tut
val program2 = sql"select 42".query[Int].unique
val io2 = program2.transact(xa)
io2.unsafeRunSync
```

Ok! We have now connected to a database to compute a constant. Considerably more impressive.

### Our Third Program

What if we want to do more than one thing in a transaction? Easy! `ConnectionIO` is a monad, so we can use a `for` comprehension to compose two smaller programs into one larger program.

```tut:silent
val program3: ConnectionIO[(Int, Double)] =
  for {
    a <- sql"select 42".query[Int].unique
    b <- sql"select random()".query[Double].unique
  } yield (a, b)
```

And behold!

```tut
program3.transact(xa).unsafeRunSync
```

The astute among you will note that we don't actually need a monad to do this; an applicative functor is all we need here. So we could also write `program3` as:

```tut:silent
val program3a = {
  val a: ConnectionIO[Int] = sql"select 42".query[Int].unique
  val b: ConnectionIO[Double] = sql"select random()".query[Double].unique
  (a, b).tupled
}
```

And lo, it was good:

```tut
program3a.transact(xa).unsafeRunSync
```

And of course this composition can continue indefinitely.

```tut
val valuesList = program3a.replicateA(5)
val result = valuesList.transact(xa)
result.unsafeRunSync.foreach(println)
```

#### Using Your Own Target Monad

As mentioned earlier, you can use any monad `M[_]` given `cats.effect.Async[M]`. For example, here we use [Monix](https://monix.io/) `Task`.
```
import monix.eval.Task

val mxa = Transactor.fromDriverManager[Task](
  "org.postgresql.Driver", "jdbc:postgresql:world", "postgres", ""
)

sql"select 42".query[Int].unique.transact(xa)
```
