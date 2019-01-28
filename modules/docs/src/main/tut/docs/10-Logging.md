---
layout: docs
number: 10
title: Logging
---

## {{page.title}}

In this chapter we discuss how to log statement execution and timing.

### Setting Up

Once again we will set up our REPL with a transactor.

```tut:silent
import doobie._
import doobie.implicits._
import cats._
import cats.data._
import cats.effect.IO
import cats.implicits._
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

### TODO

Demonstrate the new logging stuff
