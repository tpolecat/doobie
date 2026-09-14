import FreeGen2._
import scala.sys.process._
import org.typelevel.sbt.gha.{PermissionValue, Permissions}
import org.typelevel.sbt.tpolecat.{DevMode, CiMode}

// Library versions all in one place, for convenience and sanity.
lazy val catsVersion = "2.13.0"
lazy val catsEffectVersion = "3.7.1"
lazy val circeVersion = "0.14.16"
lazy val fs2Version = "3.14.0"
lazy val h2Version = "2.5.250"
lazy val hikariVersion = "7.1.0" // N.B. Hikari v4 introduces a breaking change via slf4j v2
lazy val kindProjectorVersion = "0.11.2"
lazy val mysqlVersion = "26.7.0"
lazy val log4catsVersion = "2.8.0"
lazy val postGisVersion = "2025.1.1"
lazy val postgresVersion = "42.7.12"
lazy val refinedVersion = "0.11.4"
lazy val scalaCollectionCompatVersion = "2.14.0"
lazy val scalaCheckVersion = "1.15.4"
lazy val scalatestVersion = "3.2.20"
lazy val munitVersion = "1.3.6"
lazy val otel4sVersion = "1.1.0"
lazy val otelInstrumentationVersion = "2.26.1-alpha"
lazy val shapelessVersion = "2.3.13"
lazy val silencerVersion = "1.7.1"
lazy val specs2Version = "4.23.0"
lazy val scala212Version = "2.12.21"
lazy val scala213Version = "2.13.18"
lazy val scala3Version = "3.3.8"
lazy val allScalaVersions = List(scala212Version, scala213Version, scala3Version)
lazy val doobieGitRemote =
  if (sys.env.get("GITHUB_ACTIONS").contains("true"))
    sys.env
      .get("GITHUB_TOKEN")
      .filter(_.nonEmpty)
      .map(token => s"https://x-access-token:$token@github.com/typelevel/doobie.git")
      .getOrElse("https://github.com/typelevel/doobie.git")
  else
    "git@github.com:typelevel/doobie.git"
lazy val doobieGithubWorkflowPermissions =
  Permissions.Specify.defaultRestrictive
    .withContents(PermissionValue.Write)
// scala-steward:off
lazy val slf4jVersion = "1.7.36"
// scala-steward:on
lazy val weaverVersion = "0.13.0"

// Basic versioning and publishing stuff
ThisBuild / tlBaseVersion := "1.0"
ThisBuild / tlCiReleaseBranches := Seq("main") // publish snapshots on `main`
ThisBuild / tlCiScalafmtCheck := true
//ThisBuild / scalaVersion := scala212Version
ThisBuild / scalaVersion := scala213Version
// ThisBuild / scalaVersion := scala3Version
ThisBuild / developers += tlGitHubDev("tpolecat", "Rob Norris")
ThisBuild / tpolecatDefaultOptionsMode :=
  (if (sys.env.contains("CI")) CiMode else DevMode)
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("11"))
useReadableConsoleGit
ThisBuild / githubWorkflowPermissions := Some(doobieGithubWorkflowPermissions)
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
ThisBuild / githubWorkflowBuildSbtStepPreamble := Seq.empty
ThisBuild / githubWorkflowBuild := {
  val current = (ThisBuild / githubWorkflowBuild).value
  current.map {
    // Assume step "Test" exists.
    // Prepend command "freeGen2" to the command list of that step.
    // Also make sure we run headerCreateAll first and let it complete for all subprojects,
    // otherwise "test" might see half-written files
    case testStep: WorkflowStep.Sbt if testStep.name.contains("Test") =>
      WorkflowStep.Sbt("freeGen2" :: "headerCreateAll" :: testStep.commands, name = Some("Test"))
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
    name = Some(s"Check Doc Site (2.13 only)")
    // cond = Some(s"matrix.scala == '2.13'")
  )
)
ThisBuild / githubWorkflowJobSetup ~= { steps =>
  steps.map {
    // change "sbt +update" to "sbt update" as the former doesn't work with sbt-projectmatrix
    case s: WorkflowStep.Sbt if s.name == Some("sbt update") =>
      s.withCommands(List("update"))
    case s => s
  }
}
ThisBuild / githubWorkflowPublishPostamble ++= Seq(
  WorkflowStep.Run(
    commands = List("docker compose up -d"),
    name = Some("Start up Postgres for docs publish"),
    cond = Some("startsWith(github.ref, 'refs/tags/v')")
  ),
  WorkflowStep.Run(
    commands = List(
      "git config --global user.name \"github-actions[bot]\"",
      "git config --global user.email \"41898282+github-actions[bot]@users.noreply.github.com\""
    ),
    name = Some("Configure git for docs publish"),
    cond = Some("startsWith(github.ref, 'refs/tags/v')")
  ),
  WorkflowStep.Sbt(
    commands = List("docs/makeSite", "docs/ghpagesPushSite"),
    name = Some("Publish Doc Site"),
    cond = Some("startsWith(github.ref, 'refs/tags/v')"),
    env = Map("SBT_GHPAGES_COMMIT_MESSAGE" -> "Update docs for ${{ github.ref_name }}")
  )
)

ThisBuild / mergifyStewardConfig ~= { configOpt =>
  configOpt.map(config =>
    config
      .withAuthor("typelevel-steward[bot]")
      .withMergeMinors(true))
}
ThisBuild / mergifyPrRules += MergifyPrRule(
  "merge-when-ci-pass",
  List(
    MergifyCondition.Custom("status-success=Test (ubuntu-22.04, 2.13, temurin@11)"),
    MergifyCondition.Custom("label=merge-on-build-success")
  ),
  List(MergifyAction.Merge())
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
  // Disable warning when @nowarn annotation isn't suppressing a warning
  // to simplify cross-building
  // because 2.12 @nowarn doesn't actually do anything.. https://github.com/scala/bug/issues/12313
  scalacOptions ++= {
    if (scalaBinaryVersion.value == "3") Nil
    else Seq("-Wconf:cat=unused-nowarn:s")
  },
  scalacOptions ++= {
    scalaBinaryVersion.value match {
      case "3" =>
        // Handle irrefutable patterns in for comprehensions
        Seq("-source:future", "-language:adhocExtensions", "-Xmax-inlines", "64")
      case "2.13" =>
        Seq(
          "-Xsource:3-cross",
          // See https://github.com/scala/bug/issues/13128. Should no longer be need in 2.13.18 since change was reverted
          "-Wconf:cat=lint-infer-any&msg=kind-polymorphic:s"
        )
      case "2.12" =>
        Seq("-Xsource:3")
    }
  }
)

lazy val buildSettings = Seq(
  organization := "org.typelevel",
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
      // Because of our use of sbt-projectmatrix, we can potentially run into
      // multiple writers trampling over each other modifying the same files
      // The fix is to only let 2.13 modify the src/scala files:
      Compile / headerCreate / unmanagedSourceDirectories := {
        if (scalaVersion.value == scala213Version)
          (Compile / unmanagedSourceDirectories).value
        else
          (Compile / unmanagedSourceDirectories).value.filter(_ != (Compile / scalaSource).value)
      },

      // Scaladoc options
      Compile / doc / scalacOptions ++= Seq(
        "-groups",
        "-sourcepath",
        (LocalRootProject / baseDirectory).value.getAbsolutePath,
        "-doc-source-url",
        "https://github.com/typelevel/doobie/blob/v" + version.value + "€{FILE_PATH}.scala"
      ),

      // Kind Projector (Scala 2 only)
      libraryDependencies ++= Seq(
        compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.4" cross CrossVersion.full),
        compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
      ).filterNot(_ => tlIsScala3.value),

      // MUnit
      libraryDependencies ++= Seq(
        "org.typelevel" %% "scalacheck-effect-munit" % "2.1.0" % Test,
        "org.typelevel" %% "munit-cats-effect" % "2.2.0" % Test,
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
    bench.projectRefs ++
      core.projectRefs ++
      docs.projectRefs ++
      example.projectRefs ++
      free.projectRefs ++
      h2.projectRefs ++
      `h2-circe`.projectRefs ++
      hikari.projectRefs ++
      mysql.projectRefs ++
      log4cats.projectRefs ++
      postgres.projectRefs ++
      `postgres-circe`.projectRefs ++
      refined.projectRefs ++
      otel4s.projectRefs ++
      scalatest.projectRefs ++
      munit.projectRefs ++
      specs2.projectRefs ++
      weaver.projectRefs ++
      testutils.projectRefs: _*
  )

lazy val free = projectMatrix
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
    freeGen2Package := "org.typelevel.doobie.free",
    freeGen2Classes := {
      List[Class[?]](
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
    freeGen2AllImportExcludes := Set[Class[?]](
      classOf[java.util.Map[?, ?]]
    ),
    freeGen2KleisliInterpreterImportExcludes := Set[Class[?]](
      classOf[java.sql.DriverPropertyInfo],
      classOf[java.io.Writer],
      classOf[java.io.OutputStream]
    )
  )
  .jvmPlatform(scalaVersions = allScalaVersions)

lazy val core = projectMatrix
  .in(file("modules/core"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(free)
  .dependsOn(testutils % "test->test")
  .settings(doobieSettings)
  .settings(
    name := "doobie-core",
    description := "Pure functional JDBC layer for Scala.",
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "typename" % "1.1.2",
      "com.h2database" % "h2" % h2Version % "test",
      "org.postgresql" % "postgresql" % postgresVersion % "test"
    ),
    libraryDependencies ++=
      (if (tlIsScala3.value)
         Seq.empty
       else
         Seq("com.chuusai" %% "shapeless" % shapelessVersion)),
    libraryDependencies ++=
      (if (scalaVersion.value == scala212Version)
         Seq("org.scala-lang.modules" %% "scala-collection-compat" % scalaCollectionCompatVersion)
       else Seq.empty),
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
        s"""|package org.typelevel.doobie
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
  .jvmPlatform(scalaVersions = allScalaVersions)

lazy val log4cats = projectMatrix
  .in(file("modules/log4cats"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core)
  .settings(doobieSettings)
  .settings(
    name := "doobie-log4cats",
    description := "log4cats support for doobie.",
    libraryDependencies += "org.typelevel" %% "log4cats-core" % log4catsVersion
  )
  .jvmPlatform(scalaVersions = allScalaVersions)

lazy val example = projectMatrix
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
  .jvmPlatform(scalaVersions = allScalaVersions)

lazy val mysql = projectMatrix
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
  .jvmPlatform(scalaVersions = allScalaVersions)

lazy val postgres = projectMatrix
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
      "org.scala-lang.modules" %% "scala-collection-compat" % scalaCollectionCompatVersion % Test
    ),
    freeGen2Dir := (Compile / scalaSource).value / "doobie" / "postgres" / "free",
    freeGen2Package := "org.typelevel.doobie.postgres.free",
    freeGen2Classes := {
      List[Class[?]](
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
    freeGen2AllImportExcludes := Set[Class[?]](
      classOf[java.util.Map[?, ?]]
    ),
    freeGen2KleisliInterpreterImportExcludes := Set[Class[?]](
      classOf[java.sql.Array],
      classOf[org.postgresql.copy.CopyDual]
    ),
    initialCommands :=
      """
      import cats._, cats.data._, cats.implicits._, cats.effect._
      import org.typelevel.doobie._
      import org.typelevel.doobie.implicits._
      import org.typelevel.doobie.postgres._, 
      import org.typelevel.doobie.postgres.implicits._
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
  .jvmPlatform(scalaVersions = allScalaVersions)

lazy val `postgres-circe` = projectMatrix
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
  .jvmPlatform(scalaVersions = allScalaVersions)

lazy val h2 = projectMatrix
  .in(file("modules/h2"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(doobieSettings)
  .dependsOn(core % "compile->compile;test->test")
  .settings(
    name := "doobie-h2",
    description := "H2 support for doobie.",
    libraryDependencies += "com.h2database" % "h2" % h2Version
  )
  .jvmPlatform(scalaVersions = allScalaVersions)

lazy val `h2-circe` = projectMatrix
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
  .jvmPlatform(scalaVersions = allScalaVersions)

lazy val hikari = projectMatrix
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
  .jvmPlatform(scalaVersions = allScalaVersions)

lazy val specs2 = projectMatrix
  .in(file("modules/specs2"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core, testutils % "test->test", h2 % "test")
  .settings(doobieSettings)
  .settings(
    name := "doobie-specs2",
    description := "Specs2 support for doobie.",
    libraryDependencies += "org.specs2" %% "specs2-core" % specs2Version
  )
  .jvmPlatform(scalaVersions = allScalaVersions)

lazy val scalatest = projectMatrix
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
  .jvmPlatform(scalaVersions = allScalaVersions)

lazy val munit = projectMatrix
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
  .jvmPlatform(scalaVersions = allScalaVersions)

lazy val weaver = projectMatrix
  .in(file("modules/weaver"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core)
  .settings(doobieSettings)
  .settings(
    name := s"doobie-weaver",
    description := "Weaver support for doobie.",
    testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "weaver-cats" % weaverVersion,
      "com.h2database" % "h2" % h2Version % "test"
    )
  )
  .jvmPlatform(scalaVersions = allScalaVersions)

lazy val bench = projectMatrix
  .in(file("modules/bench"))
  .enablePlugins(NoPublishPlugin)
  .enablePlugins(AutomateHeaderPlugin)
  .enablePlugins(JmhPlugin)
  .dependsOn(core, postgres, hikari)
  .settings(doobieSettings)
  .jvmPlatform(scalaVersions = Seq(scala213Version, scala3Version))

lazy val docs = projectMatrix
  .in(file("modules/docs"))
  .dependsOn(core, postgres, specs2, munit, hikari, h2, scalatest, weaver, otel4s)
  .enablePlugins(NoPublishPlugin)
  .enablePlugins(ParadoxPlugin)
  .enablePlugins(ParadoxSitePlugin)
  .enablePlugins(SitePreviewPlugin)
  .enablePlugins(GhpagesPlugin)
  .enablePlugins(MdocPlugin)
  .settings(doobieSettings)
  .settings(
    scalacOptions := Nil, // Seq("-Xsource:3"),
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "io.opentelemetry.instrumentation" % "opentelemetry-jdbc" % otelInstrumentationVersion
    ),
    Test / fork := true,

    // postgis is `provided` dependency for users, and section from book of doobie needs it
    libraryDependencies += postgisDep,
    git.remoteRepo := doobieGitRemote,
    ghpagesNoJekyll := true,
    publish / skip := true,
    paradoxTheme := Some(builtinParadoxTheme("generic")),
    version := version.value.takeWhile(_ != '+'), // strip off the +3-f22dca22+20191110-1520-SNAPSHOT business
    paradoxProperties ++= Map(
      "scala-versions" -> {
        val crossVersions = allScalaVersions.flatMap(CrossVersion.partialVersion)
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
      "scalaVersion" -> scalaVersion.value,
      "canonical.base_url" -> "https://github.com/typelevel/doobie/"
    ),
    mdocIn := (ThisBuild / baseDirectory).value / "modules" / "docs" / "src" / "main" / "mdoc",
    ghpagesRepository := (ThisBuild / baseDirectory).value / "doc_worktree",
    mdocExtraArguments ++= Seq("--no-link-hygiene"),
    Compile / paradox / sourceDirectory := mdocOut.value,
    makeSite := makeSite.dependsOn(mdoc.toTask("")).value
  )
  .jvmPlatform(scalaVersions = Seq(scala213Version))

lazy val refined = projectMatrix
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
  .jvmPlatform(scalaVersions = allScalaVersions)

lazy val otel4s = projectMatrix
  .in(file("modules/otel4s"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core)
  .settings(doobieSettings)
  .settings(
    name := "doobie-otel4s",
    description := "otel4s support for doobie.",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "otel4s-core-trace" % otel4sVersion,
      "org.typelevel" %% "otel4s-semconv" % otel4sVersion,
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "org.typelevel" %% "otel4s-oteljava-trace-testkit" % otel4sVersion % "test",
      "com.h2database" % "h2" % h2Version % "test"
    ),
    tlVersionIntroduced := Map("2.13" -> "1.0.0", "3" -> "1.0.0")
  )
  .jvmPlatform(scalaVersions = Seq(scala213Version, scala3Version))

lazy val testutils = projectMatrix
  .in(file("modules/testutils"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(doobieSettings)
  .settings(publish / skip := true)
  .jvmPlatform(scalaVersions = allScalaVersions)

lazy val checkGitNoUncommittedChanges =
  taskKey[Unit]("Check git working tree is clean (no uncommitted changes) due to generated code not being committed")
