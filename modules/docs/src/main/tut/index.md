---
layout: home

---
# doobie

<img align="right" src="https://cdn.rawgit.com/tpolecat/doobie/series/0.5.x/doobie_logo.svg" height="150px" style="padding-left: 20px"/>

[![Travis CI](https://travis-ci.org/tpolecat/doobie.svg?branch=series%2F0.5.x)](https://travis-ci.org/tpolecat/doobie)
[![Join the chat at https://gitter.im/tpolecat/doobie](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/tpolecat/doobie?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Maven Central](https://img.shields.io/maven-central/v/org.tpolecat/doobie-core_2.12.svg)](https://maven-badges.herokuapp.com/maven-central/org.tpolecat/doobie-core_2.12)
[![Javadocs](https://javadoc.io/badge/org.tpolecat/doobie-core_2.12.svg)](https://javadoc.io/doc/org.tpolecat/doobie-core_2.12)


**doobie** is a pure functional JDBC layer for Scala and [**Cats**](http://typelevel.org/cats/). It is not an ORM, nor is it a relational algebra; it simply provides a functional way to construct programs (and higher-level libraries) that use JDBC. For common use cases **doobie** provides a minimal but expressive high-level API:

```scala mdoc:silent
import doobie._
import doobie.implicits._
import cats.effect.IO
import scala.concurrent.ExecutionContext

implicit val cs = IO.contextShift(ExecutionContext.global)

val xa = Transactor.fromDriverManager[IO](
  "org.postgresql.Driver", "jdbc:postgresql:world", "postgres", ""
)

case class Country(code: String, name: String, population: Long)

def find(n: String): ConnectionIO[Option[Country]] =
  sql"select code, name, population from country where name = $n".query[Country].option
```

And then …

```scala mdoc
find("France").transact(xa).unsafeRunSync
```

**doobie** is a [**Typelevel**](http://typelevel.org/) project. This means we embrace pure, typeful, functional programming, and provide a safe and friendly environment for teaching, learning, and contributing as described in the Scala [**Code of Conduct**](http://scala-lang.org/conduct.html).

## Quick Start

The current version is **{{site.doobieVersion}}** for **Scala {{site.scalaVersions}}** with

- [**cats**](http://typelevel.org/cats/) {{site.catsVersion}}
- [**fs2**](https://github.com/functional-streams-for-scala/fs2) {{site.fs2Version}}
- [**shapeless**](https://github.com/milessabin/shapeless) {{site.shapelessVersion}}

To use **doobie** you need to add the following to your `build.sbt`. If you're not using the Postgres or H2 add-ons you'll also need to provide a JDBC driver for the database you're using.

```scala
scalacOptions += "-Ypartial-unification" // 2.11.9+

libraryDependencies ++= Seq(

  // Start with this one
  "org.tpolecat" %% "doobie-core"      % "{{site.doobieVersion}}",

  // And add any of these as needed
  "org.tpolecat" %% "doobie-h2"        % "{{site.doobieVersion}}",          // H2 driver {{site.h2Version}} + type mappings.
  "org.tpolecat" %% "doobie-hikari"    % "{{site.doobieVersion}}",          // HikariCP transactor.
  "org.tpolecat" %% "doobie-postgres"  % "{{site.doobieVersion}}",          // Postgres driver {{site.postgresVersion}} + type mappings.
  "org.tpolecat" %% "doobie-quill"     % "{{site.doobieVersion}}",          // Support for Quill {{site.quillVersion}}
  "org.tpolecat" %% "doobie-specs2"    % "{{site.doobieVersion}}" % Test,   // Specs2 support for typechecking statements.
  "org.tpolecat" %% "doobie-scalatest" % "{{site.doobieVersion}}" % Test    // ScalaTest support for typechecking statements.

)
```

See the [**documentation**](docs/01-Introduction.html) for more information on these add-ons.

Note that **doobie** is pre-1.0 software and is still undergoing active development. New versions are **not** binary compatible with prior versions, although in most cases user code will be source compatible. Starting with the 0.5.x we're trying to be a bit more careful about versioning.

## Documentation and Support

- Behold the sparkly [**documentation**](docs/01-Introduction.html) ← start here
- The [**Scaladoc**](https://www.javadoc.io/doc/org.tpolecat/doobie-core_2.12) will be handy once you get your feet wet.
- See the [**changelog**](https://github.com/tpolecat/doobie/blob/series/0.5.x/CHANGELOG.md) for an overview of changes in this and previous versions.
- The [**Gitter Channel**](https://gitter.im/tpolecat/doobie) is a great place to chat!
- There is a [**Scala Exercises**](https://www.scala-exercises.org/) module, courtesy of our friends at 47 Degrees!
- There is also the [**source**](https://github.com/tpolecat/doobie). Check out the examples too.
- If you have comments or run into trouble, please file an issue.

## Presentations, Blog Posts, etc.

Listed newest first. If you have given a presentation or have written a blog post that includes **doobie**, let me know and I'll add it to this list.

- [Typechecking SQL queries with doobie](https://blog.godatadriven.com/doobie-monix-jdbc-example) by Bas Beelen, GoDataDriven, 13-Feb 2018
- [Doobie - Feedback from the Trenches](http://fr.slideshare.net/normation/doobie-feedbacks-from-the-trenches-scalaio-2016) by François Armand, ScalaIO, October 2016
- [Pure Functional Database Programming with Fixpoint Types](https://www.youtube.com/watch?v=7xSfLPD6tiQ) by Rob Norris - Scala World, 2016 - [slides](http://tpolecat.github.io/presentations/sw2016/slides.html#1)
- [The Functional Web Stack](http://www.lyranthe.org/presentations/http4s_doobie_circe.pdf) by Gary Coady - Dublin Scala Users Group, April 2016
- [End to End and On The Level](https://www.youtube.com/watch?v=lMW_yMkxX4Q&list=PL_5uJkfWNxdkQd7FbN1whrTOsJPMgHgLg&index=2) by Dave Gurnell - Typelevel Summit, Philadelphia, March 2016
- [Programs as Values: JDBC Programming with doobie](https://www.youtube.com/watch?v=M5MF6M7FHPo) by Rob Norris - Scala by the Bay, 2015 - [slides](http://tpolecat.github.io/assets/sbtb-slides.pdf)
- [Typechecking SQL in Slick and doobie](http://underscore.io/blog/posts/2015/05/28/typechecking-sql.html) by Richard Dallaway
- [DB to JSON with a Microservice](http://da_terry.bitbucket.org/slides/presentation-scalasyd-functional-jdbc-http/#/) by Da Terry - [code](https://bitbucket.org/da_terry/scalasyd-doobie-http4s)

## Testing

If you want to build and run the tests for yourself, you'll need a local postgresql database. Tests are run as the default **postgres** user, which should have no password for access in the local environment. You can see the `before_script` section of the [.travis.yml](./.travis.yml) file for an up-to-date list of steps for preparing the test database.

<!-- This stuff isn't in the Home layout anymore so I pasted it here. -->
<script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.7/js/bootstrap.min.js"></script>
<script src="{{site.url}}{{site.baseurl}}/highlight/highlight.pack.js"></script>
<script>hljs.configure({languages:['scala','java','bash']}); hljs.initHighlighting();</script>
<script src="{{ site.baseurl }}/js/main.js"></script>
