import FreeGen2._
import scala.sys.process._
import org.typelevel.sbt.tpolecat.{DevMode, CiMode}

// Library versions all in one place, for convenience and sanity.
lazy val catsVersion = "2.12.0"
lazy val catsEffectVersion = "3.5.4"
lazy val circeVersion = "0.14.10"
lazy val fs2Version = "3.11.0"
lazy val h2Version = "1.4.200"
lazy val hikariVersion = "5.1.0" // N.B. Hikari v4 introduces a breaking change via slf4j v2
lazy val kindProjectorVersion = "0.11.2"
lazy val mysqlVersion = "9.0.0"
lazy val log4catsVersion = "2.7.0"
lazy val postGisVersion = "2024.1.0"
lazy val postgresVersion = "42.7.4"
lazy val refinedVersion = "0.11.2"
lazy val scalaCheckVersion = "1.15.4"
lazy val scalatestVersion = "3.2.18"
lazy val munitVersion = "1.0.2"
lazy val shapelessVersion = "2.3.12"
lazy val silencerVersion = "1.7.1"
lazy val specs2Version = "4.20.7"
lazy val scala212Version = "2.12.19"
lazy val scala213Version = "2.13.14"
lazy val scala3Version = "3.3.3"
// scala-steward:off
lazy val slf4jVersion = "1.7.36"
// scala-steward:on
lazy val weaverVersion = "0.8.4"

// Basic versioning and publishing stuff
ThisBuild / tlBaseVersion := "1.0"
ThisBuild / tlCiReleaseBranches := Seq("main") // publish snapshots on `main`
ThisBuild / tlCiScalafmtCheck := true
ThisBuild / scalaVersion := scala213Version
//ThisBuild / scalaVersion := scala3Version
ThisBuild / crossScalaVersions := Seq(scala212Version, scala213Version, scala3Version)
ThisBuild / developers += tlGitHubDev("tpolecat", "Rob Norris")
ThisBuild / tpolecatDefaultOptionsMode :=
  (if (sys.env.contains("CI")) CiMode else DevMode)
ThisBuild / tlSonatypeUseLegacyHost := false
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("11"))
ThisBuild / githubWorkflowBuildPreamble ++= Seq(
  WorkflowStep.Run(
    commands = List("docker compose up -d"),
    name = Some("Start up Postgres")
  ),
  WorkflowStep.Sbt(
    commands = List("headerCheckAll"),
    name = Some("Check Headers")
  )
)
ThisBuild / githubWorkflowBuild := {
  val current = (ThisBuild / githubWorkflowBuild).value
  current.map {
    // Assume step "Test" exists.
    // Prepend command "freeGen2" to the command list of that step.
    case testStep: WorkflowStep.Sbt if testStep.name.contains("Test") =>
      WorkflowStep.Sbt("freeGen2" :: testStep.commands, name = Some("Test"))
    case other => other
  }
}
ThisBuild / githubWorkflowBuildPostamble ++= Seq(
  WorkflowStep.Sbt(
    commands = List("checkGitNoUncommittedChanges"),
    name = Some(s"Check there are no uncommitted changes in git (to catch generated files that weren't committed)")
  ),
  WorkflowStep.Sbt(
    commands = List("docs/makeSite"),
    name = Some(s"Check Doc Site ($scala213Version only)"),
    cond = Some(s"matrix.scala == '$scala213Version'")
  )
)

ThisBuild / mergifyPrRules += MergifyPrRule(
  name = "merge-when-ci-pass",
  conditions = githubWorkflowGeneratedCI.value.flatMap {
    case job if mergifyRequiredJobs.value.contains(job.id) =>
      val buildSuccesses = for {
        os <- job.oses
        scalaVer <- job.scalas
        javaSpec <- job.javas
      } yield MergifyCondition.Custom(s"status-success=${job.name} ($os, $scalaVer, ${javaSpec.render})")
      buildSuccesses :+ MergifyCondition.Custom("label=merge-on-build-success")
    case _ => Nil
  }.toList,
  actions = List(MergifyAction.Merge())
)

// This is used in a couple places. Might be nice to separate these things out.
lazy val postgisDep = "net.postgis" % "postgis-jdbc" % postGisVersion

lazy val compilerFlags = Seq(
  Compile / console / scalacOptions ++= Seq(
    "-Ydelambdafy:inline" // http://fs2.io/faq.html
  ),
  Compile / doc / scalacOptions --= Seq(
    "-Xfatal-warnings"
  ),
//  Test / scalacOptions --= Seq(
//    "-Xfatal-warnings"
//  ),
  scalacOptions ++= (if (tlIsScala3.value)
                       // Handle irrefutable patterns in for comprehensions
                       Seq("-source:future", "-language:adhocExtensions")
                     else
                       Seq(
                         "-Xsource:3"
                       ))
)

lazy val buildSettings = Seq(
  organization := "org.tpolecat",
  licenses := Seq(License.MIT)
)

lazy val commonSettings =
  compilerFlags ++
    Seq(
      // These sbt-header settings can't be set in ThisBuild for some reason
      headerMappings := headerMappings.value + (HeaderFileType.scala -> HeaderCommentStyle.cppStyleLineComment),
      headerLicense := Some(HeaderLicense.Custom(
        """|Copyright (c) 2013-2020 Rob Norris and Contributors
           |This software is licensed under the MIT License (MIT).
           |For more information see LICENSE or https://opensource.org/licenses/MIT
           |""".stripMargin
      )),

      // Scaladoc options
      Compile / doc / scalacOptions ++= Seq(
        "-groups",
        "-sourcepath",
        (LocalRootProject / baseDirectory).value.getAbsolutePath,
        "-doc-source-url",
        "https://github.com/tpolecat/doobie/blob/v" + version.value + "€{FILE_PATH}.scala"
      ),

      // Kind Projector (Scala 2 only)
      libraryDependencies ++= Seq(
        compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.3" cross CrossVersion.full),
        compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
      ).filterNot(_ => tlIsScala3.value),

      // MUnit
      libraryDependencies ++= Seq(
        "org.typelevel" %% "scalacheck-effect-munit" % "1.0.4" % Test,
        "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test,
        "org.typelevel" %% "cats-effect-testkit" % catsEffectVersion % Test
      ),
      testFrameworks += new TestFramework("munit.Framework"),

      // For some reason tests started hanging with docker-compose so let's disable parallelism.
      Test / parallelExecution := false
    )

lazy val doobieSettings = buildSettings ++ commonSettings

lazy val doobie = project.in(file("."))
  .enablePlugins(NoPublishPlugin)
  .settings(doobieSettings)
  .settings(
    checkGitNoUncommittedChanges := {
      val gitDiffOutput = "git diff".!!
      if (gitDiffOutput.nonEmpty) {
        throw new Error(
          s"There are uncommitted changes in git. Perhaps some generated file from FreeGen2 were not committed?\n$gitDiffOutput")
      }
    }
  )
  .aggregate(
    bench,
    core,
    docs,
    example,
    free,
    h2,
    `h2-circe`,
    hikari,
    mysql,
    log4cats,
    postgres,
    `postgres-circe`,
    refined,
    scalatest,
    munit,
    specs2,
    weaver,
    testutils
  )

lazy val free = project
  .in(file("modules/free"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(doobieSettings)
  .settings(freeGen2Settings)
  .settings(
    name := "doobie-free",
    description := "Pure functional JDBC layer for Scala.",
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-core" % fs2Version,
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "cats-free" % catsVersion,
      "org.typelevel" %% "cats-effect" % catsEffectVersion
    ) ++ Seq(
      scalaOrganization.value % "scala-reflect" % scalaVersion.value // required for macros
    ).filterNot(_ => tlIsScala3.value),
    freeGen2Dir := (Compile / scalaSource).value / "doobie" / "free",
    freeGen2Package := "doobie.free",
    freeGen2Classes := {
      List[Class[_]](
        classOf[java.sql.NClob],
        classOf[java.sql.Blob],
        classOf[java.sql.Clob],
        classOf[java.sql.DatabaseMetaData],
        classOf[java.sql.Driver],
        classOf[java.sql.Ref],
        classOf[java.sql.SQLData],
        classOf[java.sql.SQLInput],
        classOf[java.sql.SQLOutput],
        classOf[java.sql.Connection],
        classOf[java.sql.Statement],
        classOf[java.sql.PreparedStatement],
        classOf[java.sql.CallableStatement],
        classOf[java.sql.ResultSet]
      )
    },
    freeGen2AllImportExcludes := Set[Class[_]](
      classOf[java.util.Map[_, _]]
    ),
    freeGen2KleisliInterpreterImportExcludes := Set[Class[_]](
      classOf[java.sql.DriverPropertyInfo],
      classOf[java.io.Writer],
      classOf[java.io.OutputStream]
    )
  )

lazy val core = project
  .in(file("modules/core"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(free)
  .dependsOn(testutils % "test->test")
  .settings(doobieSettings)
  .settings(
    name := "doobie-core",
    description := "Pure functional JDBC layer for Scala.",
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % shapelessVersion
    ).filterNot(_ => tlIsScala3.value) ++ Seq(
      "org.tpolecat" %% "typename" % "1.1.0",
      "com.h2database" % "h2" % h2Version % "test",
      "org.postgresql" % "postgresql" % postgresVersion % "test",
      "org.mockito" % "mockito-core" % "5.12.0" % Test,
      "com.softwaremill.sttp.tapir" % "tapir-core_2.13" % "1.11.4" % Test
    ),
    Compile / unmanagedSourceDirectories += {
      val sourceDir = (Compile / sourceDirectory).value
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, n)) if n <= 12 => sourceDir / "scala-2.13-"
        case _                       => sourceDir / "scala-2.13+"
      }
    },
    Compile / sourceGenerators += Def.task {
      val outDir = (Compile / sourceManaged).value / "scala" / "doobie"
      val outFile = new File(outDir, "buildinfo.scala")
      outDir.mkdirs
      val v = version.value
      val t = System.currentTimeMillis
      IO.write(
        outFile,
        s"""|package doobie
            |
            |/** Auto-generated build information. */
            |object buildinfo {
            |  /** Current version of doobie ($v). */
            |  val version = "$v"
            |  /** Build date (${new java.util.Date(t)}). */
            |  val date    = new java.util.Date(${t}L)
            |}
            |""".stripMargin
      )
      Seq(outFile)
    }.taskValue
  )

lazy val log4cats = project
  .in(file("modules/log4cats"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core)
  .settings(doobieSettings)
  .settings(
    name := "doobie-log4cats",
    description := "log4cats support for doobie.",
    libraryDependencies += "org.typelevel" %% "log4cats-core" % log4catsVersion
  )

lazy val example = project
  .in(file("modules/example"))
  .enablePlugins(NoPublishPlugin)
  .enablePlugins(AutomateHeaderPlugin)
  .settings(doobieSettings)
  .dependsOn(core, postgres, specs2, scalatest, hikari, h2, testutils % "test->test")
  .settings(
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-io" % fs2Version
    )
  )

lazy val mysql = project
  .in(file("modules/mysql"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core % "compile->compile;test->test")
  .settings(doobieSettings)
  .settings(
    name := "doobie-mysql",
    libraryDependencies ++= Seq(
      "com.mysql" % "mysql-connector-j" % mysqlVersion
    )
  )

lazy val postgres = project
  .in(file("modules/postgres"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core % "compile->compile;test->test")
  .dependsOn(testutils % "test->test")
  .settings(doobieSettings)
  .settings(freeGen2Settings)
  .settings(
    name := "doobie-postgres",
    description := "Postgres support for doobie.",
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-io" % fs2Version,
      "org.postgresql" % "postgresql" % postgresVersion,
      postgisDep % "provided",
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.12.0" % Test
    ),
    freeGen2Dir := (Compile / scalaSource).value / "doobie" / "postgres" / "free",
    freeGen2Package := "doobie.postgres.free",
    freeGen2Classes := {
      List[Class[_]](
        classOf[org.postgresql.copy.CopyIn],
        classOf[org.postgresql.copy.CopyManager],
        classOf[org.postgresql.copy.CopyOut],
        classOf[org.postgresql.largeobject.LargeObject],
        classOf[org.postgresql.largeobject.LargeObjectManager],
        classOf[org.postgresql.PGConnection]
      )
    },
    freeGen2Renames ++= Map(
      classOf[org.postgresql.copy.CopyDual] -> "PGCopyDual",
      classOf[org.postgresql.copy.CopyIn] -> "PGCopyIn",
      classOf[org.postgresql.copy.CopyManager] -> "PGCopyManager",
      classOf[org.postgresql.copy.CopyOut] -> "PGCopyOut"
    ),
    freeGen2AllImportExcludes := Set[Class[_]](
      classOf[java.util.Map[_, _]]
    ),
    freeGen2KleisliInterpreterImportExcludes := Set[Class[_]](
      classOf[java.sql.Array],
      classOf[org.postgresql.copy.CopyDual]
    ),
    initialCommands :=
      """
      import cats._, cats.data._, cats.implicits._, cats.effect._
      import doobie._, doobie.implicits._
      import doobie.postgres._, doobie.postgres.implicits._
      implicit val cs = IO.contextShift(scala.concurrent.ExecutionContext.global)
      val xa = Transactor.fromDriverManager[IO](driver = "org.postgresql.Driver", url = "jdbc:postgresql:world", user = "postgres", pass = "password", logHandler = None)
      val yolo = xa.yolo
      import yolo._
      import net.postgis._
      import org.postgresql.util._
      import org.postgresql.geometric._
      """,
    consoleQuick / initialCommands := ""
  )

lazy val `postgres-circe` = project
  .in(file("modules/postgres-circe"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core, postgres)
  .settings(doobieSettings)
  .settings(
    name := "doobie-postgres-circe",
    description := "Postgres circe support for doobie.",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion
    )
  )

lazy val h2 = project
  .in(file("modules/h2"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(doobieSettings)
  .dependsOn(core % "compile->compile;test->test")
  .settings(
    name := "doobie-h2",
    description := "H2 support for doobie.",
    libraryDependencies += "com.h2database" % "h2" % h2Version
  )

lazy val `h2-circe` = project
  .in(file("modules/h2-circe"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core, h2)
  .settings(doobieSettings)
  .settings(
    name := "doobie-h2-circe",
    description := "h2 circe support for doobie.",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion
    )
  )

lazy val hikari = project
  .in(file("modules/hikari"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core)
  .dependsOn(postgres % "test")
  .settings(doobieSettings)
  .settings(
    name := "doobie-hikari",
    description := "Hikari support for doobie.",
    scalacOptions --= Seq("-Xlint:unused", "-Wunused:nowarn"),
    libraryDependencies ++= Seq(
      // needs to be excluded, otherwise coursier may resolve slf4j-api 2 if > Java 11
      "com.zaxxer" % "HikariCP" % hikariVersion exclude ("org.slf4j", "slf4j-api"),
      "org.postgresql" % "postgresql" % postgresVersion % "test",
      "com.h2database" % "h2" % h2Version % "test",
      "org.slf4j" % "slf4j-api" % slf4jVersion,
      "org.slf4j" % "slf4j-nop" % slf4jVersion % "test"
    )
  )

lazy val specs2 = project
  .in(file("modules/specs2"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core, testutils % "test->test", h2 % "test")
  .settings(doobieSettings)
  .settings(
    name := "doobie-specs2",
    description := "Specs2 support for doobie.",
    libraryDependencies += "org.specs2" %% "specs2-core" % specs2Version
  )

lazy val scalatest = project
  .in(file("modules/scalatest"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core)
  .settings(doobieSettings)
  .settings(
    name := s"doobie-scalatest",
    description := "Scalatest support for doobie.",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % scalatestVersion,
      "com.h2database" % "h2" % h2Version % "test"
    )
  )

lazy val munit = project
  .in(file("modules/munit"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core)
  .settings(doobieSettings)
  .settings(
    name := s"doobie-munit",
    description := "MUnit support for doobie.",
    testFrameworks += new TestFramework("munit.Framework"),
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % munitVersion,
      "com.h2database" % "h2" % h2Version % "test"
    )
  )

lazy val weaver = project
  .in(file("modules/weaver"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core)
  .settings(doobieSettings)
  .settings(
    name := s"doobie-weaver",
    description := "Weaver support for doobie.",
    testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
    libraryDependencies ++= Seq(
      "com.disneystreaming" %% "weaver-cats" % weaverVersion,
      "com.h2database" % "h2" % h2Version % "test"
    )
  )

lazy val bench = project
  .in(file("modules/bench"))
  .enablePlugins(NoPublishPlugin)
  .enablePlugins(AutomateHeaderPlugin)
  .enablePlugins(JmhPlugin)
  .dependsOn(core, postgres)
  .settings(doobieSettings)

lazy val docs = project
  .in(file("modules/docs"))
  .dependsOn(core, postgres, specs2, munit, hikari, h2, scalatest, weaver)
  .enablePlugins(NoPublishPlugin)
  .enablePlugins(ParadoxPlugin)
  .enablePlugins(ParadoxSitePlugin)
  .enablePlugins(GhpagesPlugin)
  .enablePlugins(MdocPlugin)
  .settings(doobieSettings)
  .settings(
    scalacOptions := Nil,
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion
    ),
    Test / fork := true,

    // postgis is `provided` dependency for users, and section from book of doobie needs it
    libraryDependencies += postgisDep,
    git.remoteRepo := "git@github.com:tpolecat/doobie.git",
    ghpagesNoJekyll := true,
    publish / skip := true,
    paradoxTheme := Some(builtinParadoxTheme("generic")),
    version := version.value.takeWhile(_ != '+'), // strip off the +3-f22dca22+20191110-1520-SNAPSHOT business
    paradoxProperties ++= Map(
      "scala-versions" -> {
        val crossVersions = (core / crossScalaVersions).value.flatMap(CrossVersion.partialVersion)
        val scala2Versions = crossVersions.filter(_._1 == 2).map(_._2).mkString("2.", "/", "") // 2.12/13
        val scala3 = crossVersions.find(_._1 == 3).map(_ => "3") // 3
        List(Some(scala2Versions), scala3).flatten.filter(_.nonEmpty).mkString(" and ") // 2.12/13 and 3
      },
      "org" -> organization.value,
      "scala.binary.version" -> CrossVersion.binaryScalaVersion(scalaVersion.value),
      "version" -> version.value,
      "catsVersion" -> catsVersion,
      "fs2Version" -> fs2Version,
      "shapelessVersion" -> shapelessVersion,
      "h2Version" -> h2Version,
      "postgresVersion" -> postgresVersion,
      "scalaVersion" -> scalaVersion.value
    ),
    mdocIn := baseDirectory.value / "src" / "main" / "mdoc",
    Compile / paradox / sourceDirectory := mdocOut.value,
    makeSite := makeSite.dependsOn(mdoc.toTask("")).value
  )

lazy val refined = project
  .in(file("modules/refined"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core)
  .settings(doobieSettings)
  .settings(
    name := "doobie-refined",
    description := "Refined support for doobie.",
    libraryDependencies ++= Seq(
      "eu.timepit" %% "refined" % refinedVersion,
      "com.h2database" % "h2" % h2Version % "test"
    )
  )

lazy val testutils = project
  .in(file("modules/testutils"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(doobieSettings)
  .settings(publish / skip := true)

lazy val checkGitNoUncommittedChanges =
  taskKey[Unit]("Check git working tree is clean (no uncommitted changes) due to generated code not being committed")
