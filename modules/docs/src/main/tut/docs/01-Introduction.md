---
layout: docs
number: 1
title: Introduction
---

<div style="margin-bottom: 20px; text-align: center"><img src="../img/yeah.png"/></div>

## {{page.title}}

This is a very short book about **doobie**, which is a pure-functional JDBC layer for Scala.

**doobie** provides low-level access to everything in `java.sql` (as of Java 8), allowing you to write any JDBC program in a pure functional style. However the focus of this book is the **high-level API**, which is where most users will spend their time.

This book is organized cookbook-style: we demonstrate a common task and then explain how it works, perhaps in more detail than you want right now. The goal is to get you up and running quickly, but give you a handle on the deeper stuff if you need it later.


### Target Audience

This library is designed for people who are interested in typed, pure functional programming. If you are not a [Cats](https://github.com/typelevel/cats) user or are not familiar with functional I/O and monadic effects, you may need to go slowly and may want to spend some time reading [Functional Programming in Scala](http://manning.com/bjarnason/), which introduces all of the ideas that you will find when exploring **doobie**.

Having said this, if you find yourself confused or frustrated by this documentation or the **doobie** API, *please* ask a question on [Gitter](https://gitter.im/tpolecat/doobie), file an [issue](https://github.com/tpolecat/doobie/issues) and ask for help. Both the library and the documentation are young and are changing quickly, and it is inevitable that some things will be unclear. Accordingly, **this book is updated continuously** to address problems and omissions.

### The Setup

This book is compiled as part of the build using the [tut](https://github.com/tpolecat/tut) tutorial generator, so the code examples are guaranteed to compile (and with luck should also work correctly). Each page stands on its own: if you copy and paste code samples starting from the top, it will work in your REPL as long as you have the proper setup, described here.

#### Sample Database Setup

The example code assumes a local [PostgreSQL](http://www.postgresql.org/) server with a `postgres` user with no password, [PostGIS](http://postgis.net/) extensions (optional), and the sample `world` database loaded up. If you're on a Mac you might check out the excellent [Postgres.app](http://postgresapp.com/) if you don't want to install PostgreSQL as a service. You can set up the user and sample database (and an `enum` we use in a few examples) as follows:

```
$ curl -O https://raw.githubusercontent.com/tpolecat/doobie/series/0.7.x/world.sql
$ psql -c 'create user postgres createdb' postgres
$ psql -c 'create database world;' -U postgres
$ psql -c '\i world.sql' -d world -U postgres
$ psql -d world -c "create type myenum as enum ('foo', 'bar')" -U postgres
$ psql -d world -c "create extension postgis" -U postgres
```

Skip the last statement if you don't have PostGIS installed. Note that the final `ANALYZE` comand in the import will emit a few errors for system tables. This is expected and is fine. Try a query or two to double-check your setup:

```
$ psql -d world -U postgres
psql (9.3.5)
Type "help" for help.

world=> select name, continent, population from country where name like 'U%';
                 name                 |   continent   | population
--------------------------------------+---------------+------------
 United Arab Emirates                 | Asia          |    2441000
 United Kingdom                       | Europe        |   59623400
 Uganda                               | Africa        |   21778000
 Ukraine                              | Europe        |   50456000
 Uruguay                              | South America |    3337000
 Uzbekistan                           | Asia          |   24318000
 United States                        | North America |  278357000
 United States Minor Outlying Islands | Oceania       |          0
(8 rows)

world=> \q
$
```

You can of course change this setup if you like, but you will need to adjust your JDBC connection information accordingly. Most examples will work with any compliant database, but in a few cases (noted in the text) we rely on vendor-specific behavior.

#### Scala Setup

On the Scala side you just need a console with the proper dependencies. A minimal `build.sbt` would look something like this.

```scala
scalaVersion := "{{site.scalaVersion}}" // Scala {{site.scalaVersions}}

scalacOptions += "-Ypartial-unification" // if scalaVersion < 2.13 

lazy val doobieVersion = "{{site.doobieVersion}}"

libraryDependencies ++= Seq(
  "org.tpolecat" %% "doobie-core"     % doobieVersion,
  "org.tpolecat" %% "doobie-postgres" % doobieVersion,
  "org.tpolecat" %% "doobie-specs2"   % doobieVersion
)
```

For scalaVersion < 2.13, the `-Ypartial-unification` compiler flag enables a bug fix that makes working with functional code significantly easier. See the Cats [Getting Started](https://github.com/typelevel/cats#getting-started) for more info on this if it interests you. The latest version of scala [enabled this by default](https://github.com/typelevel/cats/issues/2948).

If you are not using PostgreSQL you can omit `doobie-postgres` and will need to add the appropriate JDBC driver as a dependency. Note that there is a `doobie-h2` add-on if you happen to be using [H2](http://www.h2database.com/).

### Conventions

Each page begins with some imports, like this.

```scala mdoc:silent
import cats._, cats.data._, cats.implicits._
import doobie._
```

After that there is text interspersed with code examples. Sometimes definitions will stand alone.

```scala mdoc:silent
case class Person(name: String, age: Int)
val nel = NonEmptyList.of(Person("Bob", 12), Person("Alice", 14))
```

And sometimes they will appear with output.

```scala mdoc
nel.head
nel.tail
```
Sometimes we demonstrate that something doesn't compile. In such cases it will be clear from the context that this is expected, and not a problem with the documentation.

```scala mdoc:fail
woozle(nel) // doesn't compile
```

### Feedback and Contributions

Feedback on **doobie** or this book is genuinely welcome. Please feel free to file a [pull request](https://github.com/tpolecat/doobie) if you have a contribution, or file an [issue](https://github.com/tpolecat/doobie/issues), or chat with us on [Gitter](https://gitter.im/tpolecat/doobie).
