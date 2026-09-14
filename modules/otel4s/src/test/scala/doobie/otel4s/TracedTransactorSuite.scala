// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package org.typelevel.doobie.otel4s

import cats.effect.IO
import org.typelevel.doobie.Transactor
import org.typelevel.doobie.Update
import org.typelevel.doobie.otel4s.syntax.fragment.*
import org.typelevel.doobie.syntax.all.*
import io.opentelemetry.sdk.resources.Resource as OTelResource
import munit.{Location, TestOptions}
import org.typelevel.otel4s.oteljava.testkit.{InstrumentationScopeExpectation, TelemetryResourceExpectation}
import org.typelevel.otel4s.oteljava.testkit.trace.{
  EventExpectation,
  SpanExpectation,
  StatusExpectation,
  TraceExpectation,
  TraceExpectations,
  TraceForestExpectation,
  TracesTestkit
}
import org.typelevel.otel4s.semconv.attributes.{DbAttributes, ErrorAttributes, ExceptionAttributes}
import org.typelevel.otel4s.trace.TracerProvider
import org.typelevel.otel4s.{Attribute, Attributes}

class TracedTransactorSuite extends munit.CatsEffectSuite {
  import QueryCaptureConfig.{QueryParametersPolicy, QueryTextPolicy}

  private val xa = Transactor.fromDriverManager[IO](
    driver = "org.h2.Driver",
    url = "jdbc:h2:mem:otel4s;DB_CLOSE_DELAY=-1",
    user = "sa",
    password = "",
    logHandler = None
  )

  testkitTest("record db operation name for prepared statements") { testkit =>
    val expected = expectedSpans(
      span(
        name = "executeQuery",
        attributes = Attributes(
          DbAttributes.DbOperationName("executeQuery")
        )
      )
    )

    for {
      tx <- testkit.tracedTransactor(tracedConfig())
      _ <- sql"select 1".query[Int].unique.transact(tx)
      _ <- testkit.assertFinishedSpans(expected)
    } yield ()
  }

  testkitTest("record constant attributes") { testkit =>
    val attributes = Attributes(
      Attribute("a", 1L),
      Attribute("b", "b")
    )

    val expected = expectedSpans(
      span(
        name = "executeQuery",
        attributes = Attributes(
          DbAttributes.DbOperationName("executeQuery")
        ) ++ attributes
      )
    )

    for {
      tx <- testkit.tracedTransactor(tracedConfig(constAttributes = attributes))
      _ <- sql"select 1".query[Int].unique.transact(tx)
      _ <- testkit.assertFinishedSpans(expected)
    } yield ()
  }

  testkitTest("record db operation outcome in case of an error") { testkit =>
    val expected = expectedSpans(
      span(
        name = "executeQuery",
        attributes = Attributes(
          ErrorAttributes.ErrorType("org.h2.jdbc.JdbcSQLDataException"),
          DbAttributes.DbResponseStatusCode("22018"),
          DbAttributes.DbOperationName("executeQuery")
        ),
        status = StatusExpectation.error.description(
          "Data conversion error converting \"text\" [22018-240]"
        ),
        events = List(
          event(
            name = "exception",
            attributes = Attributes(
              ExceptionAttributes.ExceptionType("org.h2.jdbc.JdbcSQLDataException"),
              ExceptionAttributes.ExceptionMessage("Data conversion error converting \"text\" [22018-240]")
            )
          )
        )
      )
    )

    for {
      tx <- testkit.tracedTransactor(tracedConfig())
      _ <- sql"select 'text'".query[Int].unique.transact(tx).attempt.assert(_.isLeft)
      _ <- testkit.assertFinishedSpans(expected)
    } yield ()
  }

  testkitTest("properly trace operations that happen within the same transaction") { testkit =>
    val expected = expectedSpans(
      span(
        name = "executeQuery",
        attributes = Attributes(
          DbAttributes.DbQueryText("select 1"),
          DbAttributes.DbOperationName("executeQuery")
        )
      ),
      span(
        name = "executeQuery",
        attributes = Attributes(
          DbAttributes.DbQueryText("select 2"),
          DbAttributes.DbOperationName("executeQuery")
        )
      )
    )

    val config = tracedConfig(
      captureQuery = QueryCaptureConfig(
        queryTextPolicy = QueryTextPolicy.Always,
        queryParametersPolicy = QueryParametersPolicy.None
      )
    )

    for {
      tx <- testkit.tracedTransactor(config)
      _ <- (for {
        _ <- sql"select 1".query[Int].unique
        _ <- sql"select 2".query[Int].unique
      } yield ()).transact(tx)
      _ <- testkit.assertFinishedSpans(expected)
    } yield ()
  }

  testkitTest("capture query text and parameters when enabled") { testkit =>
    val expected = expectedSpans(
      span(
        name = "executeQuery",
        attributes = Attributes(
          DbAttributes.DbQueryText("select ?"),
          DbAttributes.DbOperationName("executeQuery"),
          Attribute("db.query.parameter.0", "1")
        )
      )
    )

    val config = tracedConfig(
      captureQuery = QueryCaptureConfig(
        queryTextPolicy = QueryTextPolicy.Always,
        queryParametersPolicy = QueryParametersPolicy.All
      )
    )

    for {
      tx <- testkit.tracedTransactor(config)
      _ <- sql"select ${1}".query[Int].unique.transact(tx)
      _ <- testkit.assertFinishedSpans(expected)
    } yield ()
  }

  testkitTest("record db.operation.batch.size for batch operations") { testkit =>
    val expected = expectedSpans(
      span(
        name = "executeLargeUpdate",
        attributes = Attributes(
          DbAttributes.DbQueryText("CREATE LOCAL TEMPORARY TABLE TEST_BATCH (int_value INT)"),
          DbAttributes.DbOperationName("executeLargeUpdate")
        )
      ),
      span(
        name = "executeLargeBatch",
        attributes = Attributes(
          DbAttributes.DbQueryText("insert into TEST_BATCH (int_value) values (?)"),
          DbAttributes.DbOperationBatchSize(3L),
          DbAttributes.DbOperationName("executeLargeBatch")
        )
      )
    )

    val config = tracedConfig(
      captureQuery = QueryCaptureConfig(
        queryTextPolicy = QueryTextPolicy.Always,
        queryParametersPolicy = QueryParametersPolicy.None
      )
    )

    for {
      tx <- testkit.tracedTransactor(config)
      _ <- (for {
        _ <- sql"CREATE LOCAL TEMPORARY TABLE TEST_BATCH (int_value INT)".update.run
        _ <- Update[Int]("insert into TEST_BATCH (int_value) values (?)")
          .updateMany(List(1, 2, 3))
      } yield ()).transact(tx)
      _ <- testkit.assertFinishedSpans(expected)
    } yield ()
  }

  testkitTest("semconv config captures query text only for parameterized queries") { testkit =>
    val expected = expectedSpans(
      span(
        name = "executeQuery",
        attributes = Attributes(
          DbAttributes.DbSystemName(DbAttributes.DbSystemNameValue.Postgresql),
          DbAttributes.DbNamespace("doobie"),
          DbAttributes.DbQueryText("select ?"),
          DbAttributes.DbOperationName("executeQuery")
        )
      )
    )

    val config =
      TracingConfig.recommended(
        dbSystemName = DbAttributes.DbSystemNameValue.Postgresql,
        dbNamespace = "doobie"
      )

    for {
      tx <- testkit.tracedTransactor(config)
      _ <- sql"select ${1}".query[Int].unique.transact(tx)
      _ <- testkit.assertFinishedSpans(expected)
    } yield ()
  }

  testkitTest("semconv config does not capture query text for non-parameterized queries") { testkit =>
    val expected = expectedSpans(
      span(
        name = "executeQuery",
        attributes = Attributes(
          DbAttributes.DbSystemName(DbAttributes.DbSystemNameValue.Postgresql),
          DbAttributes.DbNamespace("doobie"),
          DbAttributes.DbOperationName("executeQuery")
        )
      )
    )

    val config =
      TracingConfig.recommended(
        dbSystemName = DbAttributes.DbSystemNameValue.Postgresql,
        dbNamespace = "doobie"
      )

    for {
      tx <- testkit.tracedTransactor(config)
      _ <- sql"select 1".query[Int].unique.transact(tx)
      _ <- testkit.assertFinishedSpans(expected)
    } yield ()
  }

  testkitTest("capture explicit summary with default parser and span namer") { testkit =>
    val summary = "summary via syntax"

    val expected = expectedSpans(
      span(
        name = summary,
        attributes = Attributes(
          DbAttributes.DbOperationName("executeQuery"),
          DbAttributes.DbQuerySummary(summary)
        )
      ),
      span(
        name = summary,
        attributes = Attributes(
          DbAttributes.DbOperationName("executeLargeUpdate"),
          DbAttributes.DbQuerySummary(summary)
        )
      )
    )

    for {
      tx <- testkit.tracedTransactor(tracedConfig())
      _ <- sql"select 1".queryWithSummary[Int](summary).unique.transact(tx)
      _ <- sql"CREATE LOCAL TEMPORARY TABLE TEST (int_value INT)".updateWithSummary(summary).run.transact(tx)
      _ <- testkit.assertFinishedSpans(expected)
    } yield ()
  }

  testkitTest("capture explicit attributes with default parser") { testkit =>
    val attrs = Attributes(Attribute("test.attr", "ok"))

    val expected = expectedSpans(
      span(
        name = "executeQuery",
        attributes = Attributes(
          DbAttributes.DbOperationName("executeQuery")
        ) ++ attrs
      ),
      span(
        name = "executeLargeUpdate",
        attributes = Attributes(
          DbAttributes.DbOperationName("executeLargeUpdate")
        ) ++ attrs
      )
    )

    for {
      tx <- testkit.tracedTransactor(tracedConfig())
      _ <- sql"select 1".queryWithAttributes[Int](attrs).unique.transact(tx)
      _ <- sql"CREATE LOCAL TEMPORARY TABLE TEST (int_value INT)".updateWithAttributes(attrs).run.transact(tx)
      _ <- testkit.assertFinishedSpans(expected)
    } yield ()
  }

  testkitTest("use custom span namer") { testkit =>
    val config = tracedConfig().withSpanNamer(new SpanNamer {
      def spanName(context: SpanNamer.Context): Option[String] =
        context.attributes
          .flatMap(_.get(DbAttributes.DbQuerySummary).map(_.value))
          .map(summary => s"sql:$summary")
    })

    val summary = "named"
    val expected = expectedSpans(
      span(
        name = "sql:named",
        attributes = Attributes(
          DbAttributes.DbOperationName("executeQuery"),
          DbAttributes.DbQuerySummary(summary)
        )
      )
    )

    for {
      tx <- testkit.tracedTransactor(config)
      _ <- sql"select 1".queryWithSummary[Int](summary).unique.transact(tx)
      _ <- testkit.assertFinishedSpans(expected)
    } yield ()
  }

  testkitTest("use query analyzer metadata for span naming") { testkit =>
    val analyzer = new QueryAnalyzer {
      def analyze(sql: String): Option[QueryAnalyzer.QueryMetadata] =
        Some(
          QueryAnalyzer.QueryMetadata(
            queryText = Some(sql),
            operationName = Some("SELECT"),
            collectionName = Some("users"),
            storedProcedureName = None,
            querySummary = Some("SELECT users")
          )
        )
    }

    val config = tracedConfig()
      .withQueryAnalyzer(analyzer)
      .withSpanNamer(SpanNamer.fromQueryMetadata)

    val expected = expectedSpans(
      span(
        name = "SELECT users",
        attributes = Attributes(
          DbAttributes.DbOperationName("SELECT"),
          DbAttributes.DbQuerySummary("SELECT users"),
          DbAttributes.DbCollectionName("users")
        )
      )
    )

    for {
      tx <- testkit.tracedTransactor(config)
      _ <- sql"select 1".query[Int].unique.transact(tx)
      _ <- testkit.assertFinishedSpans(expected)
    } yield ()
  }

  testkitTest("append collection from the query analyzer metadata to the preconfigured one") { testkit =>
    val analyzer = new QueryAnalyzer {
      def analyze(sql: String): Option[QueryAnalyzer.QueryMetadata] =
        Some(
          QueryAnalyzer.QueryMetadata(
            queryText = Some(sql),
            operationName = None,
            collectionName = Some("users"),
            storedProcedureName = None,
            querySummary = Some("SELECT users")
          )
        )
    }

    val config = tracedConfig(Attributes(DbAttributes.DbCollectionName("prod")))
      .withQueryAnalyzer(analyzer)
      .withSpanNamer(SpanNamer.fromQueryMetadata)

    val expected = expectedSpans(
      span(
        name = "SELECT users",
        attributes = Attributes(
          DbAttributes.DbQuerySummary("SELECT users"),
          DbAttributes.DbOperationName("executeQuery"),
          DbAttributes.DbCollectionName("prod|users")
        )
      )
    )

    for {
      tx <- testkit.tracedTransactor(config)
      _ <- sql"select 1".query[Int].unique.transact(tx)
      _ <- testkit.assertFinishedSpans(expected)
    } yield ()
  }

  testkitTest("user query summary prevails over the query analyzer metadata") { testkit =>
    val analyzer = new QueryAnalyzer {
      def analyze(sql: String): Option[QueryAnalyzer.QueryMetadata] =
        Some(
          QueryAnalyzer.QueryMetadata(
            queryText = Some(sql),
            operationName = Some("SELECT"),
            collectionName = Some("users"),
            storedProcedureName = None,
            querySummary = Some("SELECT users")
          )
        )
    }

    val config = tracedConfig()
      .withQueryAnalyzer(analyzer)
      .withSpanNamer(SpanNamer.fromQueryMetadata)

    val expected = expectedSpans(
      span(
        name = "SELECT users",
        attributes = Attributes(
          DbAttributes.DbOperationName("SELECT"),
          DbAttributes.DbCollectionName("users"),
          DbAttributes.DbQuerySummary("custom summary")
        )
      )
    )

    for {
      tx <- testkit.tracedTransactor(config)
      _ <- sql"select 1".queryWithSummary[Int]("custom summary").unique.transact(tx)
      _ <- testkit.assertFinishedSpans(expected)
    } yield ()
  }

  testkitTest("ignore plain fragment labels by default") { testkit =>
    val label = "some label"

    val expected = expectedSpans(
      span(
        name = "executeQuery",
        attributes = Attributes(
          DbAttributes.DbOperationName("executeQuery")
        )
      ),
      span(
        name = "executeLargeUpdate",
        attributes = Attributes(
          DbAttributes.DbOperationName("executeLargeUpdate")
        )
      )
    )

    for {
      tx <- testkit.tracedTransactor(tracedConfig())
      _ <- sql"select 1".queryWithLabel[Int](label).unique.transact(tx)
      _ <- sql"CREATE LOCAL TEMPORARY TABLE TEST (int_value INT)".updateWithLabel(label).run.transact(tx)
      _ <- testkit.assertFinishedSpans(expected)
    } yield ()
  }

  testkitTest("capture fragment attributes") { testkit =>
    val attrs = Attributes(Attribute("test.attr", "ok"))

    val expected = expectedSpans(
      span(
        name = "executeQuery",
        attributes = Attributes(
          DbAttributes.DbOperationName("executeQuery")
        ) ++ attrs
      ),
      span(
        name = "executeLargeUpdate",
        attributes = Attributes(
          DbAttributes.DbOperationName("executeLargeUpdate")
        ) ++ attrs
      )
    )

    for {
      tx <- testkit.tracedTransactor(tracedConfig())
      _ <- sql"select 1".queryWithAttributes[Int](attrs).unique.transact(tx)
      _ <- sql"CREATE LOCAL TEMPORARY TABLE TEST (int_value INT)".updateWithAttributes(attrs).run.transact(tx)
      _ <- testkit.assertFinishedSpans(expected)
    } yield ()
  }

  testkitTest("capture fragment attributes and use summary as span name") { testkit =>
    val summary = "the summary"
    val attrs = Attributes(
      DbAttributes.DbQuerySummary(summary),
      Attribute("test.attr", "ok")
    )

    val expected = expectedSpans(
      span(
        name = summary,
        attributes = Attributes(
          DbAttributes.DbOperationName("executeQuery")
        ) ++ attrs
      ),
      span(
        name = summary,
        attributes = Attributes(
          DbAttributes.DbOperationName("executeLargeUpdate")
        ) ++ attrs
      )
    )

    for {
      tx <- testkit.tracedTransactor(tracedConfig())
      _ <- sql"select 1".queryWithAttributes[Int](attrs).unique.transact(tx)
      _ <- sql"CREATE LOCAL TEMPORARY TABLE TEST (int_value INT)".updateWithAttributes(attrs).run.transact(tx)
      _ <- testkit.assertFinishedSpans(expected)
    } yield ()
  }

  testkitTest("ignore plain fragment labels when parser cannot parse") { testkit =>
    val label = "some label"

    val expected = expectedSpans(
      span(
        name = "executeQuery",
        attributes = Attributes(
          DbAttributes.DbOperationName("executeQuery")
        )
      ),
      span(
        name = "executeLargeUpdate",
        attributes = Attributes(
          DbAttributes.DbOperationName("executeLargeUpdate")
        )
      )
    )

    for {
      tx <- testkit.tracedTransactor(tracedConfig())
      _ <- sql"select 1".queryWithLabel[Int](label).unique.transact(tx)
      _ <- sql"CREATE LOCAL TEMPORARY TABLE TEST (int_value INT)".updateWithLabel(label).run.transact(tx)
      _ <- testkit.assertFinishedSpans(expected)
    } yield ()
  }

  private def testkitTest(name: TestOptions)(f: Testkit => IO[Unit])(implicit loc: munit.Location): Unit =
    test(name) {
      TracesTestkit
        .builder[IO]
        .addTracerProviderCustomizer(_.setResource(OTelResource.empty()))
        .build
        .use(testkit => f(new Testkit(testkit)))
    }

  private def tracedConfig(
      constAttributes: Attributes = Attributes.empty,
      captureQuery: QueryCaptureConfig = QueryCaptureConfig.disabled
  ): TracingConfig =
    TracingConfig(
      tracerScopeName = "doobie",
      defaultSpanName = "doobie:exec",
      constAttributes = constAttributes,
      captureQuery = captureQuery
    )

  private class Testkit(testkit: TracesTestkit[IO]) {
    implicit val tracerProvider: TracerProvider[IO] = testkit.tracerProvider

    def tracedTransactor(config: TracingConfig): IO[Transactor[IO]] =
      TracedTransactor.create(xa, config, None)

    def assertFinishedSpans(expected: TraceForestExpectation)(implicit loc: Location): IO[Unit] =
      testkit.finishedSpans.flatMap { spans =>
        TraceExpectations.check(spans, expected) match {
          case Right(_) =>
            IO.unit
          case Left(mismatches) =>
            IO(fail(TraceExpectations.format(mismatches)))
        }
      }
  }

  private def expectedSpans(spans: SpanExpectation*): TraceForestExpectation =
    spans.toList match {
      case Nil =>
        TraceForestExpectation.empty
      case head :: tail =>
        TraceForestExpectation.ordered(
          TraceExpectation.leaf(head),
          tail.map(TraceExpectation.leaf)*
        )
    }

  private def span(
      name: String,
      attributes: Attributes,
      status: StatusExpectation = StatusExpectation.unset,
      events: List[EventExpectation] = Nil
  ): SpanExpectation = {
    val base = SpanExpectation
      .client(name)
      .attributesExact(attributes)
      .resource(TelemetryResourceExpectation.exact(OTelResource.empty()))
      .scope(InstrumentationScopeExpectation.name(
        "doobie").version(org.typelevel.doobie.buildinfo.version).attributesEmpty)
      .status(status)
      .eventCount(events.size)
      .linkCount(0)

    events match {
      case Nil =>
        base
      case head :: tail =>
        base.exactlyEvents(head, tail*)
    }
  }

  private def event(
      name: String,
      attributes: Attributes
  ): EventExpectation =
    EventExpectation.any
      .name(name)
      .attributesSubset(attributes)

}
