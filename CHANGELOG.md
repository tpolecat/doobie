# changelog

This file summarizes **notable** changes for each release, but does not describe internal changes unless they are particularly exciting. For complete details please see the corresponding [milestones](https://github.com/tpolecat/doobie/milestones?state=closed) and their associated issues.

----

### <a name="0.7.0"></a>Work in Progress for Version 0.7.0

0.7 is probably the last version of doobie prior to shifting work to a tagless implementation. This version will be source-compatible for most users but is not binary compatible with 0.6.x or any earlier version.

- Added `TRACE`-level logging for low-level operations, sent to slf4j `Logger` specified via the `logger` member on `Transactor`, by default a logger called `"doobie.transactor"`. If you wish to associate correlation ids or otherwise mess with logging you can do this by swapping out the logger prior to calling `transact`. See `CorrelationId.scala` in the `example` project for an example.
- Deprecated existing logging machinery.
- Added support for Scala 2.13.0-M5, many thanks to **Sam Guymer** for setting this up.
- Replaced `MonadError`-based `guarantee` with `bracket`, which prevents potential resource leakage when using cancelable IO. Thanks **Sam Guymer** for this update.
- `Fragment` concatenation and derived combinators like `Fragments.in` are now stacksafe, so you can now have gigantic `IN` clauses. This required API changes in `Param`, `Fragment`, `Update[0]` and `Query[0]` but these should not affect most users.

____
### <a name="0.6.0"></a>New and Noteworthy for Version 0.6.0

Many thanks to **Arber Shabhasa**, **Bjørn Madsen**, **Chris Davenport**, **Cody Allen**, **Dmitry Polienko**, **Kai(luo) Wang**, **Kevin Walter**, **Mark Canlas**, and **Quang Le Hong** for their contributions to this release.

:exclamation: This is a major update with **breaking changes**. Please read the following notes carefully.

#### Transactors and Threading

Prior to 0.6.x **doobie** had nothing to say about threading; it was up to users to shift interpreted `IO` programs onto dedicated pools if desired. This has changed. We now identify three distinct execution contexts that are relevant to database applications.

1. All *non-blocking* work is performed on the execution context identified by [`ContextShift[F]`](https://typelevel.org/cats-effect/datatypes/contextshift.html). If you are using [`IOApp`](https://typelevel.org/cats-effect/datatypes/ioapp.html) (which you should) this instance is provided for you. All interpreters (and thus all transactors) need this instance on construction.
1. Requesting a JDBC connection is a blocking operation, so to avoid deadlock these requests cannot be placed in competition with running programs (which need to make progress in order to finish up and return their connections to the pool). Therefore we need a distinct, bounded, blocking execution context for the single purpose of awaiting database connections. All transactors that use a connection pool will require this execution context to be specified.
1. All JDBC primitive operations are [potentially] blocking, so we need a distinct, typically unbounded (since the connection context above provides a logical bound) execution context for scheduling these operations. All transactors that use a connection pool will require this execution context to be specified.

For convenience we provide an `ExecutionContexts` module that provides [`Resource`]()s yielding the kinds of `ExecutionContext`s that will tend to be useful for the scenarios above. Note that `DriverManagerTransactor` provides an unbounded number of connections and is unsuitable for production use anyway, so we do not require the blocking pools here (it uses an unbounded pool internally). Fine for testing but don't use it in real life.

See the book chapter on **Managing Connections** for more information and examples.

#### Streams and Resources

In 0.5.x we provided some single-element `Stream`s that would emit values and guarantee resource cleanup. This has been generalized as `Resource` in cats-effect 1.x, and all such constructors (`H2Transactor.newH2Transactor` for example) now use `Resource` rather than `Stream`. You can use `Stream.resource(rsrc)` to regain the old functionality if desired.

#### Splitting of read/write functionality.

Prior to the 0.6.x series we provided two typeclasses for **bidirectional** type mapping:
- `Meta` defined nullable mappings between column/parameter values and scala types.
- `Composite` defined null-safe mappings betwen column/parameter **vectors** and scala types.

Starting with version 0.6.0 type mappings are **unidirectional**:
- `Meta` has been split into `Get` and `Put` typeclasses, for reads and writes of column/parameter values, respectively.
- `Composite` has been split into `Read` and `Write` typeclasses, for reads and writes of column/parameter vectors, respecitively.

Note that `Meta` does still exist, but only as a mechanism for introducing `Get/Put` pairs. An implicit `Meta[A]` induces both an implicit `Get[A]` and an implicit `Put[A]`, and the old mechanism of `Meta[A].imap(...)(...)` is still supported for this purpose. The `xmap` method has been replaced with parametric `imap` and `TypeTag`-constrained `timap`. Prefer `timap` when possible because it yields better diagnostic information when typechecking queries.

To summarize:

| 0.5.x                | 0.6.x                    | Notes |
|----------------------|--------------------------------|--|
| `[A: Meta]`      | `[A: Get : Put]`      | Or just one, depending on usage. |
| `[A: Composite]` | `[A: Read : Write]`   | Or just one, depending on usage. |
| `Meta[A].xmap(..)` | `Meta[A].timap(...)` | Or `imap` when a `TypeTag` is unavailable. |
| `Composite[A].xmap(...)` | `Read[A].map(...)` <br> `Write[A].contramap(...)` | This takes two steps now. |

Please refer to book chapter on **Custom Mappings** and the `examples` project for more details.

#### Other Changes

- postgres-circe module created exposing Get and Put instances for `io.circe.Json`
- upgraded to cats-effect 1.0 and fs2 1.0

---
### <a name="0.5.3"></a>New and Noteworthy for Version 0.5.3

Minor updates, see below.

- Updated to **refined 0.9**, **fs2 0.10.4**, **Hikari 3.1.0**, **ScalaCheck 1.14.0**, and **Specs2 4.2.0**.
- Added `.execWith` method to `Fragment`, allowing for custom handling of the associated statement.
- Added `pure` to generated algebras, so you can now say `FC.pure(1)` instead of `1.pure[ConnectionIO]` if you like. Thanks wedens!
- Added an example of generic DAOs for simple schemas. See `Orm.scala` in the `example` project.
- Minor doc updates.

This is the last planned release in the 0.5.x series. Work will start soon on 0.6!

---
### <a name="0.5.2"></a>New and Noteworthy for Version 0.5.2

Minor updates, see below.

- Added experimental support for Blazing Fast&trade; inserts in Postgres via `COPY ... FROM STDIN`. See book Chapter 15 for an example.
- Introduced [MiMa](https://github.com/lightbend/migration-manager) for checking binary compatibility. Note that for the 0.5.x series we will guarantee compatibility for Scala 2.12 only.
- `FirstExample` now actually shows output! Thanks Aaron Hawley.
- Added pgEnumStringOpt for transparently partial pgEnum decoding. Thanks Christopher Davenport!
- Added `Composite.deriveComposite[A]()` which creates a "semi-automatic" derivation of `Composite[]` for a given type `A`. While instances are usually derived automatically, this can be used to speed up compilation by only deriving once. It can also be used to avoid needing `Meta[]` instances in scope or derivable at every site where a given `Composite[A]` is used. Thanks Scott Parish!

---
### <a name="0.5.1"></a>New and Noteworthy for Version 0.5.1

Minor update to get dependencies up to date.

- Updated to **fs2 0.10.2**, **hikari-cp 2.7.8**, and **specs2 4.0.3**.
- You can now do `.stripMargin` on fragments, thanks to Arne Claassen!

---
### <a name="0.5.0"></a>New and Noteworthy for Version 0.5.0

This introduces the **0.5.x** series which standardizes on [**cats**](http://typelevel.org/cats/), [**cats-effect**](https://github.com/typelevel/cats-effect), and [**fs2**](https://github.com/functional-streams-for-scala/fs2). This is a big release that will make life much simpler for people who were using 0.4.x with cats. See the **migration** document on the microsite for more information.

**Many thanks** to Andreas Svanberg, Bjørn Madsen, Charles Hunt, Christopher Davenport, Dale Wijnand, Devin Ekins, Dmitry Polienko, Earl St Sauver, fabio labella, Frank S. Thomas, Hossam Karim, Jisoo Park, Keir Lawson, Mads Hartmann, nigredo-tori, Radu Gancea, sh0hei, Stephen Lazaro, tgalappathth, wedens, and x1- for their contributions to this release!

Notable changes:

##### Cats Standardization

- 🎵 *Ding, dong the yax is dead!* 🎵 The new codebase is based on cats!
- The `-cats` segment of artifact names is gone. `doobie-core` uses cats now, as does everything else.

##### API Changes

- Rather than `foo.imports._` for both names and implicits, there are now distinct imports `foo._, foo.implicits._`. The old `foo.imports._` still works but is deprecated.
- Syntax classes are now organized as in cats. Much cleaner but end users probably won't notice.
- `Composite[A]` now implies `Composite[Option[A]]` which is a very useful change. It means joins can be expressed much more easily. See `Join.scala` in the `example` project.

##### Project Structure, Build, Etc.

- The doc has been ported to [sbt-microsites](https://github.com/47deg/sbt-microsites).
- `FreeGen2` code generator now generates all effect types with `cats.effect.Async` instances, in preparation for transactors that can make use of distinct thread pools for certain operations (JDBC primitives for instance). Free algebras and interpreters for Postgres are also generated now.
- Added [WartRemover](http://www.wartremover.org/) finally.
- The release process is much better, so releases can be more frequent. Version numbers appearing in the doc are now supplied automatically.

---
### <a name="0.4.4"></a>New and Noteworthy for Version 0.4.4

This release fixes an [issue](https://github.com/tpolecat/doobie/pull/569) with HikariTransactor (thanks Naoki Aoyama) and supersedes the botched 0.4.3 release.

---
### <a name="0.4.3"></a>New and Noteworthy for Version 0.4.3

This was a failed attempt to add back support for 2.10. Do not use this release.

---
### <a name="0.4.2"></a>New and Noteworthy for Version 0.4.2

Sparkly contributors for this release are :sparkles: n4to4, :sparkles: Alexa DeWit, :sparkles: wedens, :sparkles: Colt Frederickson, :sparkles: Benjamin Trenker, :sparkles: nigredo-tori, :sparkles: Suhas Gaddam, :sparkles: Christopher Davenport, :sparkles: Damir Vandic, :sparkles: Jacob Barber, and :chicken: tpolecat. Noteworthy changes:

- Dropped support for 2.10 because I can't figure out how to publish it.
- Replaced all the `doobie.free` internals with a new design that makes it practical to write your own interpreter (or, more commonly, subclass the default one) which is very useful for testing and who knows what else. For most users this will not be an observable change.
- Switched to a new transactor design that makes it simple to customize behavior, and combined with new interpreter design makes it practical to use **doobie** types in free coproducts (see `coproduct.scala` in the `example` project). This is a **minor breaking change**:
  - The `yolo` member on `Transactor` is no longer stable, so you cannot `import xa.yolo._` anymore; instead you must say `val y = xa.yolo; import y._`. Because this is typically done with `initialCommands` in sbt it's unlikely to be a big deal.
  - `Transactor` is now a final case class with an extra type member for the underlying pool or other connection source.
- Note that the interpreter/transactor changes require `Monad` instances at a few more call sites, which should be transparent in most cases but may require Cats users to `import fs2.interop.cats._` here and there … if scalac is claiming there's no instance available after upgrading that's probably why.
- Added support for type refinements (refined library). See the `doobie-refined` and `doobie-refined-cats` modules.
- Added a `fail` constructor to all the `F*` modules.
- Made `Meta.nxmap` unnecessary and fixed issues with mappings that are undefined for zero values of underlying unboxed types.
- Added mapping for Postgres `hstore` type.
- Make postgres enums nullable (change `Atom` instance to `Meta`).
- Generalized `QueryChecker` and `AnalysisSpec` to allow any effect type.
- Added `stream` and `streamWithChunkSize` as fs2 friendly aliases on `Query`.
- Fix parsing of JDBC type TimestampWithTimezone (introduced in JDK 8).
- Added `Suspendable` instance for `PGConnectionIO`
- Added an `Unknown` constructor to `JdbcType` for JDBC type constants outside the spec and known extensions.
- Generalized the underlying structure of `Meta`/`Composite` and eliminated `Atom`. Client code that uses `Atom` will need to be re-implemented in terms of `Meta` or `Composite`.
- Added `Fragment.const0` for constant fragments with no trailing space, while `const` now *does* add a trailing space. Users of `Fragment.const` may need/wish to change to `const0`.
- `Manifest` replaced with `TypeTag` in `doobie.util.invariant`.
- Improved H2 UUID Support.  Query analysis involving H2 and UUIDs should no longer show a type mismatch.

In addition the following libraries were updated:

- sbt 0.13.15
- Scala 2.10.6, 2.11.11, 2.12.3
- scalaz 7.2.9
- PostgreSQL JDBC driver 42.1.1
- Hikari 2.6.1
- Circe 0.8.0

---
### <a name="0.4.1"></a>New and Noteworthy for Version 0.4.1

This release updates **doobie** to Cats 0.9 and the associated fs2-cats interop layer to 0.3, courtesy of mighty space robot Adelbert Chang. There are no other changes.

---
### <a name="0.4.0"></a>New and Noteworthy for Version 0.4.0

This was intended to be a quick follow-up to 0.3.0 but it got a little out of hand and turned into a major release. **Please read these notes carefully** because there are some breaking changes relative to 0.3.0.

Eighteen people contributed to this release, of whom seventeen were not tpolecat. They are sparkly and awesome. In no particular order, many many thanks to :sparkles: Channing Walton :sparkles: Daniel Wunsch :sparkles: Gary Coady :sparkles: Tristan Lohman :sparkles: Jisoo Park :sparkles: Yury Liavitski :sparkles: Ikhoon Eom :sparkles: Marek Kadek :sparkles: Leif Wickland :sparkles: Zack Powers :sparkles: Kris Nuttycombe :sparkles: Pepe García :sparkles: Peter Neyens :sparkles: ritschwumm :sparkles: ronanM :sparkles: Sam Ritchie :sparkles: and wedens. :sparkles:

##### Cats Support

This is probably the most important development for 0.4.0 and it is due in large part to the hard work of :sparkles: Jisoo Park :sparkles:. Impossibly huge thanks for his work throughout this process.

<img align="right" height="150" style="padding-left: 20px" src="https://camo.githubusercontent.com/c7a1d594954b34a8277bce52343fe731b14870ad/687474703a2f2f706c61737469632d69646f6c617472792e636f6d2f6572696b2f63617473322e706e67"/>

- **doobie** is now built natively for [Cats](https://github.com/typelevel/cats) with [fs2](https://github.com/functional-streams-for-scala/fs2/tree/series/0.9), with no need for a shim/adapter layer. At the moment this is accomplished via a preprocessor.
- All artifacts are built for Cats. Names are suffixed with `-cats`, so the scalaz version is `doobie-core` and the Cats version is `doobie-core-cats`.
- The **book of doobie** is now published in separate editions for scalaz and Cats.

##### Changes to Core

- There is now support for simple **statement logging** with timing information. This is *not* the long-promised structured logging feature but it fits the common use case and fills a clear functional gap. See the book chapter for details and examples.
- The **dynamic SQL** story is now slightly better with the introduction of **composable statement fragments**. These allow you to build statements from smaller pieces without having to track parameter placeholders/offsets by hand. See the book chapter for details and examples.
- SQL `IN` clauses are now handled via `Fragments.in`, which is a **breaking change** relative to 0.3.0. See the book chapter on parameterized queries for an example.
- Methods on `Query[0]/Update[0]` that construct streams (`.process`) now have variants that allow you to specify the **chunk size**, which by default is 512 rows.
- There is now an `IO` data type that you can use if you're having a hard time settling on a target effect type. It works identically in Cats and scalaz and is what's used in the book.

##### Changes to Add-On Modules

We cleaned up the add-on modules a bit and made some naming simplifications that are **breaking** relative to 0.3.0.

- There is now a **ScalaTest** add-on. See the book chapter on unit testing for details and examples.
- The `contrib` segment has been removed from module names, so `doobie-contrib-h2` is now just `doobie-h2`. It has also been removed from *package* names. So `doobie.contrib.h2` is now just `doobie.h2`.
- In both cases the `postgresql` segment has been shortened to `postgres`.
- All modules now have a consistent import story. `import doobie.<module>.imports._` should get you everything you need.

That's it! Enjoy the release. Once again many thanks to our contributors.

---
### <a name="0.3.0"></a>New and Noteworthy for Version 0.3.0

This release brings **doobie** up to date with major dependencies, almost entirely due to the hard work of **@guersam**. The release is otherwise equivalent to 0.2.4 from a feature standpoint.

Upgrades:
- Updated to scalaz 7.2
- Updated to shapeless 2.3
- Updated to JDK 1.8
- Added build support Scala 2.12

---
### <a name="0.2.4"></a>New and Noteworthy for Version 0.2.4

This is a minor release with a few odds and ends and an important library update.

Improvements:
- You can now construct a `HikariTransactor` from an existing `HikariDataSource` (thanks **@raulraja**).
- There is now a `.nel` accumulator for non-empty resultsets (thanks **@refried**).
- Arrays of `UUID`s are now supported for PostgreSQL (thanks **@veegee**).
- Published jarfiles now have OSGi headers.
- Some internal cleanup but nothing that should affect end users.

Upgrades:
- Updated to scalaz-stream 0.8 (thanks **@guersam**).


---
### <a name="0.2.3"></a>New and Noteworthy for Version 0.2.3

This release includes more performance work and some usability improvements, as well as documentation improvements here and there. This release should be source-compatible for most users, but is **not** binary compatible with 0.2.2 or any other release. Let me know if you run into source compatibilty problems. Special thanks to **@mdmoss** for build improvements, **@fommil** and **@non** for help with Sonatype, and everyone else for your continued interest and contributions!

Improvements:

- **doobie** is now published on Sonatype and no longer requires a Bintray resolver.
- The `free` modules now provide natural transformations of the form `FooIO ~> Kleisli[M, Foo, ?]` and `Foo => FooIO ~> M`, which should make life easier when using **doobie** with existing JDBC resources (your own `Connection` for example).
- New optimized column-vector reads and accumulators for `IList` and standard library collections via `CanBuildFrom` yield performance generally indistinguishable from raw JDBC. The `Query/Query0` operations `to`, `list`, and `vector` are now very fast. Result handling via `Process` benefits to a lesser extent.
- `Composite` instances are now available for shapeless record types.
- `Atom` instances are now available for single-element product types (thanks **@wedens** and **@refried**).
- `DriverManagerTransactor` now includes constructors corresponding to all `getConnection` methods on `DriverManager`.
- `free` algebras and interpreters have been re-implemented to use method dispatch rather than large `match` expressions, resulting in minor performance improvements throughout. This completes the work started in 0.2.2.
- The `sql` interpolator now supports interpolated sequences for SQL `IN` clauses. See [Chapter 5](http://tpolecat.github.io/doobie-0.2.3/05-Parameterized.html) for more information.
- The **book of doobie** now includes a [FAQ Chapter](http://tpolecat.github.io/doobie-0.2.3/15-FAQ.html).
- The `example` project now includes some PostgreSQL CopyManager examples (thanks **@wedens**).

Big Fixes:

- The PostGIS dependency was pulling in unnecessary transitive dependencies that caused problems with sbt-assembly. This has been fixed.

Upgrades:

- Updated to Scala 2.11.7
- Updated to shapeless 2.2.5
- Updated to scalaz-stream 0.7.2a
- Updated to specs2-core 3.6
- Updated to tut 0.4.0 (build-time dependency only)
- Updated to sbt 0.13.8 (build-time dependency only)
- Updated to kind-projector 0.7.1 (build-time dependency only)


---
### <a name="0.2.2"></a>New and Noteworthy for Version 0.2.2

This is another minor release that adds yet more support PostgreSQL-specific features, updates dependencies, and improves performance for resultset processing. Thanks everyone for your continued interest and contributions!

Additions:

- Added `HC.updateManyWithGeneratedKeys` and associated syntax on `Update` to allow batch updates to return updated rows. See [Chapter 7](http://tpolecat.github.io/doobie-0.2.2/07-Updating.html) for an example.
- Added algebras and free monads thereof for PostgreSQL vendor-specific driver APIs, which allows **doobie** to directly support `LISTEN/NOTIFY`, `COPY FROM STDIN` and a number of other interesting features. See [Chapter 13](http://tpolecat.github.io/doobie-0.2.2/13-Extensions-PostgreSQL.html) for details and examples.

Improvements:

- Huge improvements to the implementation of the `sql` interpolator, courtesy of [@milessabin](https://twitter.com/milessabin). This removes the last remaining arity limit.
- Added examples for PostgreSQL-specific error handling combinators. See [Chapter 13](http://tpolecat.github.io/doobie-0.2.2/13-Extensions-PostgreSQL.html) for more information.
- Added `Unapply` instances to make the `Monad` instance for `FreeC` and associated syntax conversions inferrable. For now these are on the `doobie.imports` module.
- Significant performance improvements for resultset processing, with `.list` and `.vector` now ~4.5x faster and `.process` ~2.5x faster. This work is ongoing.

Upgrades:

- Updated to Scala 2.10.5
- Updated to shapeless 2.2.0 (thanks [@milessabin](https://twitter.com/milessabin))
- Updated to scalaz-stream 0.7a
- Updated to PostgreSQL JDBC driver 9.4-1201-jdbc41
- Updated to Specs2 3.6 (thanks [@etorrebore](https://twitter.com/etorreborre))


---
### <a name="0.2.1"></a>New and Noteworthy for Version 0.2.1

This is a minor follow-up release, primarily to add support for some PostgreSQL features and other odds and ends reported by users. Thanks to users and contributors for their help!

Additions:

- Added `Transactor` to wrap an existing `DataSource`. See [Chapter 12](http://tpolecat.github.io/doobie-0.2.1/12-Managing-Connections.html) for more information.
- Added support for PostGIS and PostgreSQL `enum` types. See [Chapter 13](http://tpolecat.github.io/doobie-0.2.1/13-Extensions-PostgreSQL.html) for more information.
- Added combinators for batch updates. See [Chapter 7](http://tpolecat.github.io/doobie-0.2.1/07-Updating.html) for more information.
- Added `Composite` support for `HList`; anywhere you could map a product or tuple type in 0.2.0 you can now also use a [shapeless](https://github.com/milessabin/shapeless) `HList`.
- Added `Atom` support for `scalaz.Maybe`; anywhere you could map an `Option` type in 0.2.0 you can now also use `Maybe`.
- Added `.optionT` method on `Query` and `Query0`.
- Added an [example](https://github.com/tpolecat/doobie/blob/v0.2.1/example/src/main/scala/example/PostgresNotify.scala) that exposes a PostgreSQL `NOTIFY` channel as an scalaz-stream `Process`.

Improvements:

- The 22-parameter limit on the `sql` interpolator has been increased to 50, and should go away entirely by 0.3.0 at the latest. There are no other arity limits in **doobie**.
- All `Query` and `Update` constructions are now supported for typechecking with Specs2 and YOLO mode.
- Many improvements in **book of doobie**.
- Tidied up examples a bit.

Upgrades:

- Updated to Scala 2.11.6.
- Updated to scalaz 7.1.1 and scalaz-stream 0.6a
- Updated to [tut](https://github.com/tpolecat/tut) 0.3.1 (build only; not user-facing).
- Updated to Specs2 3.9.4

Bug Fixes:

- Fixed problem with typechecking `BOOLEAN` column mappings.
- Fixed driver classloading problem with `HikariTransactor`.
