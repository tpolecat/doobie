import FreeGen2._
import ReleaseTransformations._
import microsites._

resolvers in Global += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

// Library versions all in one place, for convenience and sanity.
lazy val catsVersion          = "1.5.0"
lazy val catsEffectVersion    = "1.2.0"
lazy val circeVersion         = "0.11.1"
def fs2CoreVersion(scalaVersion: String) = CrossVersion.partialVersion(scalaVersion) match {
  case Some((2, v)) if v >= 13 => "1.0.3-SNAPSHOT"
  case _                       => "1.0.2"
}
lazy val h2Version            = "1.4.197"
lazy val hikariVersion        = "3.3.0"
lazy val kindProjectorVersion = "0.9.9"
lazy val monixVersion         = "3.0.0-RC3"
lazy val postGisVersion       = "2.3.0"
lazy val postgresVersion      = "42.2.5"
lazy val refinedVersion       = "0.9.4"
lazy val scalaCheckVersion    = "1.14.0"
def scalatestVersion(scalaVersion: String) = CrossVersion.partialVersion(scalaVersion) match {
  case Some((2, v)) if v >= 13 => "3.0.6-SNAP5"
  case _                       => "3.0.5"
}
lazy val shapelessVersion     = "2.3.3"
lazy val sourcecodeVersion    = "0.1.5"
lazy val specs2Version        = "4.3.6"
lazy val scala211Version      = "2.11.12"
lazy val scala212Version      = "2.12.8"
lazy val scala213Version      = "2.13.0-M5"
lazy val silencerVersion      = "1.3.1"
lazy val slf4jVersion         = "1.7.25"

// Check bincompat versus this version.
lazy val binaryCompatibleVersion = "0.6.0"

// Our set of warts
lazy val doobieWarts =
  Warts.allBut(
    Wart.Any,                 // false positives
    Wart.ArrayEquals,         // false positives
    Wart.Nothing,             // false positives
    Wart.Null,                // Java API under the hood; we have to deal with null
    Wart.Product,             // false positives
    Wart.Serializable,        // false positives
    Wart.ImplicitConversion,  // we know what we're doing
    Wart.Throw,               // TODO: switch to ApplicativeError.fail in most places
    Wart.PublicInference,     // fails https://github.com/wartremover/wartremover/issues/398
    Wart.ImplicitParameter    // only used for Pos, but evidently can't be suppressed
  )

// This is used in a couple places. Might be nice to separate these things out.
lazy val postgisDep = "net.postgis" % "postgis-jdbc" % postGisVersion

lazy val compilerFlags = Seq(
  scalacOptions ++= (
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n <= 11 => // for 2.11 all we care about is capabilities, not warnings
        Seq(
          "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
          "-language:higherKinds",             // Allow higher-kinded types
          "-language:implicitConversions",     // Allow definition of implicit functions called views
          "-Ypartial-unification"              // Enable partial unification in type constructor inference
        )
      case _ =>
        Seq(
          "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
          "-encoding", "utf-8",                // Specify character encoding used by source files.
          "-explaintypes",                     // Explain type errors in more detail.
          "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
          "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
          "-language:higherKinds",             // Allow higher-kinded types
          "-language:implicitConversions",     // Allow definition of implicit functions called views
          "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
          "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
          "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
          "-Xfuture",                          // Turn on future language features.
          "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
          "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
          "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
          "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
          "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
          "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
          "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
          "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
          "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
          "-Xlint:option-implicit",            // Option.apply used implicit view.
          "-Xlint:package-object-classes",     // Class or object defined in package object.
          "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
          "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
          "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
          "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
          // "-Yno-imports",                      // No predef or default imports
          "-Ywarn-dead-code",                  // Warn when dead code is identified.
          "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
          "-Ywarn-numeric-widen",              // Warn when numerics are widened.
          "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
          "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
          "-Ywarn-unused:locals",              // Warn if a local definition is unused.
          "-Ywarn-unused:params",              // Warn if a value parameter is unused.
          "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
          "-Ywarn-unused:privates",            // Warn if a private member is unused.
          "-Ywarn-value-discard",              // Warn when non-Unit expression results are unused.
          "-Yrangepos"
        )
    }
  ),
  // flags removed in 2.13
  scalacOptions ++= (
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n == 12 =>
        Seq(
          "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
          "-Ypartial-unification"              // Enable partial unification in type constructor inference
        )
      case _ =>
        Seq.empty
    }
  ),
  scalacOptions in (Test, compile) --= (
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n <= 11 =>
        Seq("-Yno-imports")
      case _ =>
        Seq(
          "-Ywarn-unused:privates",
          "-Ywarn-unused:locals",
          "-Ywarn-unused:imports",
          "-Yno-imports"
        )
    }
  ),
  scalacOptions in (Compile, console) --= Seq("-Xfatal-warnings", "-Ywarn-unused:imports", "-Yno-imports"),
  scalacOptions in (Compile, console) ++= Seq("-Ydelambdafy:inline"), // http://fs2.io/faq.html
  scalacOptions in (Compile, doc)     --= Seq("-Xfatal-warnings", "-Ywarn-unused:imports", "-Yno-imports")
)

lazy val buildSettings = Seq(
  organization := "org.tpolecat",
  licenses ++= Seq(("MIT", url("http://opensource.org/licenses/MIT"))),
  scalaVersion := scala212Version,
  crossScalaVersions := Seq(scala211Version, scala212Version, scala213Version)
)

lazy val commonSettings =
  compilerFlags ++
  Seq(

    // These sbt-header settings can't be set in ThisBuild for some reason
    headerMappings := headerMappings.value + (HeaderFileType.scala -> HeaderCommentStyle.cppStyleLineComment),
    headerLicense  := Some(HeaderLicense.Custom(
      """|Copyright (c) 2013-2018 Rob Norris and Contributors
         |This software is licensed under the MIT License (MIT).
         |For more information see LICENSE or https://opensource.org/licenses/MIT
         |""".stripMargin
    )),

    // Wartremover in compile and test (not in Console)
    wartremoverErrors in (Compile, compile) := doobieWarts,
    wartremoverErrors in (Test,    compile) := doobieWarts,

    scalacOptions in (Compile, doc) ++= Seq(
      "-groups",
      "-sourcepath", (baseDirectory in LocalRootProject).value.getAbsolutePath,
      "-doc-source-url", "https://github.com/tpolecat/doobie/blob/v" + version.value + "€{FILE_PATH}.scala"
    ),
    libraryDependencies ++= Seq(
      compilerPlugin("com.github.ghik" %% "silencer-plugin" % silencerVersion),
      "com.github.ghik" %% "silencer-lib" % silencerVersion % Provided,
      "org.scalacheck" %% "scalacheck"        % scalaCheckVersion % "test",
      "org.specs2"     %% "specs2-core"       % specs2Version     % "test",
      "org.specs2"     %% "specs2-scalacheck" % specs2Version     % "test"
    ),
    addCompilerPlugin("org.spire-math" %% "kind-projector" % kindProjectorVersion),
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },
    releaseProcess := Nil
  )

lazy val mimaSettings = {
  import sbtrelease.Version

  def semverBinCompatVersions(major: Int, minor: Int, patch: Int): Set[(Int, Int, Int)] = {
    val majorVersions: List[Int] = List(major)
    val minorVersions : List[Int] =
      if (major >= 1) Range(0, minor).inclusive.toList
      else List(minor)
    def patchVersions(currentMinVersion: Int): List[Int] =
      if (minor == 0 && patch == 0) List.empty[Int]
      else if (currentMinVersion != minor) List(0)
      else Range(0, patch - 1).inclusive.toList

    val versions = for {
      maj <- majorVersions
      min <- minorVersions
      pat <- patchVersions(min)
    } yield (maj, min, pat)
    versions.toSet
  }

  def mimaVersions(version: String): Set[String] = {
    Version(version) match {
      case Some(Version(major, Seq(minor, patch), _)) =>
        semverBinCompatVersions(major.toInt, minor.toInt, patch.toInt)
          .map{case (maj, min, pat) => s"${maj}.${min}.${pat}"}
      case _ =>
        Set.empty[String]
    }
  }
  // Safety Net For Exclusions
  lazy val excludedVersions: Set[String] = Set()

  // Safety Net for Inclusions
  lazy val extraVersions: Set[String] = Set()

  Seq(
    mimaPreviousArtifacts := (mimaVersions(version.value) ++ extraVersions)
      .filterNot(excludedVersions.contains(_))
      .map(v => organization.value %% name.value % v)
  )
}

lazy val publishSettings = Seq(
  useGpg := false,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  homepage := Some(url("https://github.com/tpolecat/doobie")),
  pomIncludeRepository := Function.const(false),
  pomExtra := (
    <developers>
      <developer>
        <id>tpolecat</id>
        <name>Rob Norris</name>
        <url>http://tpolecat.org</url>
      </developer>
    </developers>
  ),
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  mappings in (Compile, packageSrc) ++= (managedSources in Compile).value pair sbt.io.Path.relativeTo(sourceManaged.value / "main" / "scala")
) ++ mimaSettings

lazy val noPublishSettings = Seq(
  skip in publish := true
)

lazy val doobieSettings = buildSettings ++ commonSettings

lazy val doobie = project.in(file("."))
  .settings(doobieSettings)
  .settings(noPublishSettings)
  .dependsOn(free, core, h2, hikari, postgres, `postgres-circe`, specs2, example, bench, scalatest, docs, refined)
  .aggregate(free, core, h2, hikari, postgres, `postgres-circe`, specs2, example, bench, scalatest, docs, refined)
  .settings(
    releaseCrossBuild := true,
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      releaseStepCommand("mimaReportBinaryIssues"),
      inquireVersions,
      runClean,
      runTest,
      releaseStepCommand("docs/tut"), // annoying that we have to do this twice
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      publishArtifacts,
      releaseStepCommand("sonatypeReleaseAll"),
      // releaseStepCommand("docs/publishMicrosite"), // TODO: re-enable for 0.7.0 final
      setNextVersion,
      commitNextVersion,
      pushChanges
    )
  )

lazy val free = project
  .in(file("modules/free"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(doobieSettings)
  .settings(publishSettings)
  .settings(freeGen2Settings)
  .settings(
    name := "doobie-free",
    description := "Pure functional JDBC layer for Scala.",
    scalacOptions += "-Yno-predef",
    scalacOptions -= "-Xfatal-warnings", // the only reason this project exists
    libraryDependencies ++= Seq(
      "co.fs2"         %% "fs2-core"    % fs2CoreVersion(scalaVersion.value),
      "org.typelevel"  %% "cats-core"   % catsVersion,
      "org.typelevel"  %% "cats-free"   % catsVersion,
      "org.typelevel"  %% "cats-effect" % catsEffectVersion,
      "org.slf4j"      %  "slf4j-api"   % slf4jVersion,
    ),
    freeGen2Dir     := (scalaSource in Compile).value / "doobie" / "free",
    freeGen2Package := "doobie.free",
    freeGen2Classes := {
      import java.sql._
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
    }
  )


lazy val core = project
  .in(file("modules/core"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(free)
  .settings(doobieSettings)
  .settings(publishSettings)
  .settings(
    name := "doobie-core",
    description := "Pure functional JDBC layer for Scala.",
    libraryDependencies ++= Seq(
      scalaOrganization.value %  "scala-reflect" % scalaVersion.value, // required for shapeless macros
      "com.chuusai"           %% "shapeless"     % shapelessVersion,
      "com.lihaoyi"           %% "sourcecode"    % sourcecodeVersion,
      "com.h2database"        %  "h2"            % h2Version          % "test",
      "org.slf4j"             %  "slf4j-simple"  % slf4jVersion       % "test",
    ),
    scalacOptions += "-Yno-predef",
    unmanagedSourceDirectories in Compile += {
      val sourceDir = (sourceDirectory in Compile).value
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, n)) if n >= 13 => sourceDir / "scala-2.13+"
        case _                       => sourceDir / "scala-2.13-"
      }
    },
    sourceGenerators in Compile += Def.task {
      val outDir = (sourceManaged in Compile).value / "scala" / "doobie"
      val outFile = new File(outDir, "buildinfo.scala")
      outDir.mkdirs
      val v = version.value
      val t = System.currentTimeMillis
      IO.write(outFile,
        s"""|package doobie
            |
            |/** Auto-generated build information. */
            |object buildinfo {
            |  /** Current version of doobie ($v). */
            |  val version = "$v"
            |  /** Build date (${new java.util.Date(t)}). */
            |  val date    = new java.util.Date(${t}L)
            |}
            |""".stripMargin)
      Seq(outFile)
    }.taskValue
  )

lazy val example = project
  .in(file("modules/example"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(doobieSettings ++ noPublishSettings)
  .dependsOn(core, postgres, specs2, scalatest, hikari, h2)
  .settings(
    libraryDependencies ++= Seq(
      "co.fs2"    %% "fs2-io"       % fs2CoreVersion(scalaVersion.value),
      "org.slf4j" %  "slf4j-simple" % slf4jVersion,
    )
  )

lazy val postgres = project
  .in(file("modules/postgres"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core)
  .settings(doobieSettings)
  .settings(publishSettings)
  .settings(freeGen2Settings)
  .settings(
    name  := "doobie-postgres",
    description := "Postgres support for doobie.",
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-io"     % fs2CoreVersion(scalaVersion.value),
      "org.postgresql" % "postgresql" % postgresVersion,
      postgisDep % "provided"
    ),
    scalacOptions -= "-Xfatal-warnings", // we need to do deprecated things
    freeGen2Dir     := (scalaSource in Compile).value / "doobie" / "postgres" / "free",
    freeGen2Package := "doobie.postgres.free",
    freeGen2Classes := {
      import java.sql._
      List[Class[_]](
        classOf[org.postgresql.copy.CopyIn],
        classOf[org.postgresql.copy.CopyManager],
        classOf[org.postgresql.copy.CopyOut],
        classOf[org.postgresql.fastpath.Fastpath],
        classOf[org.postgresql.largeobject.LargeObject],
        classOf[org.postgresql.largeobject.LargeObjectManager],
        classOf[org.postgresql.PGConnection]
      )
    },
    freeGen2Renames ++= Map(
      classOf[org.postgresql.copy.CopyDual]     -> "PGCopyDual",
      classOf[org.postgresql.copy.CopyIn]       -> "PGCopyIn",
      classOf[org.postgresql.copy.CopyManager]  -> "PGCopyManager",
      classOf[org.postgresql.copy.CopyOut]      -> "PGCopyOut",
      classOf[org.postgresql.fastpath.Fastpath] -> "PGFastpath"
    ),
    initialCommands := """
      import cats._, cats.data._, cats.implicits._, cats.effect._
      import doobie._, doobie.implicits._
      import doobie.postgres._, doobie.postgres.implicits._
      implicit val cs = IO.contextShift(scala.concurrent.ExecutionContext.global)
      val xa = Transactor.fromDriverManager[IO]("org.postgresql.Driver", "jdbc:postgresql:world", "postgres", "")
      val yolo = xa.yolo
      import yolo._
      import org.postgis._
      import org.postgresql.util._
      import org.postgresql.geometric._
      """,
    initialCommands in consoleQuick := ""
  )

lazy val `postgres-circe` = project
  .in(file("modules/postgres-circe"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core, postgres)
  .settings(doobieSettings)
  .settings(publishSettings)
  .settings(
    name  := "doobie-postgres-circe",
    description := "Postgres circe support for doobie.",
    libraryDependencies ++= Seq(
      "io.circe"    %% "circe-core"    % circeVersion,
      "io.circe"    %% "circe-parser"  % circeVersion
    )
  )

lazy val h2 = project
  .in(file("modules/h2"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(doobieSettings)
  .settings(publishSettings)
  .dependsOn(core)
  .settings(
    name  := "doobie-h2",
    description := "H2 support for doobie.",
    libraryDependencies += "com.h2database" % "h2"  % h2Version,
    scalacOptions -= "-Xfatal-warnings" // we need to do deprecated things
  )

lazy val hikari = project
  .in(file("modules/hikari"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core)
  .settings(doobieSettings)
  .settings(publishSettings)
  .settings(
    name := "doobie-hikari",
    description := "Hikari support for doobie.",
    libraryDependencies += "com.zaxxer" % "HikariCP" % hikariVersion
  )

lazy val specs2 = project
  .in(file("modules/specs2"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core)
  .dependsOn(h2 % "test")
  .settings(doobieSettings)
  .settings(publishSettings)
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
  .settings(publishSettings)
  .settings(
    name := s"doobie-scalatest",
    description := "Scalatest support for doobie.",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % scalatestVersion(scalaVersion.value),
      "com.h2database"  %  "h2"       % h2Version % "test"
    )
  )

lazy val bench = project
  .in(file("modules/bench"))
  .enablePlugins(AutomateHeaderPlugin)
  .enablePlugins(JmhPlugin)
  .dependsOn(core, postgres)
  .settings(doobieSettings)
  .settings(noPublishSettings)

lazy val docs = project
  .in(file("modules/docs"))
  .dependsOn(core, postgres, specs2, hikari, h2, scalatest)
  .enablePlugins(MicrositesPlugin)
  .settings(doobieSettings)
  .settings(noPublishSettings)
  .settings(
    scalacOptions in Tut --= Seq("-Ywarn-unused:imports", "-Yno-imports", "-Ywarn-unused:params"),
    scalacOptions in Tut ++= Seq("-Ydelambdafy:inline"), // http://fs2.io/faq.html

    libraryDependencies ++= Seq(
      "io.circe"    %% "circe-core"    % circeVersion,
      "io.circe"    %% "circe-generic" % circeVersion,
      "io.circe"    %% "circe-parser"  % circeVersion,
      // sbt seems to still look for 2.12 when 2.13 is excluded from `crossScalaVersions`
      // so just comment out the dependency and readd it when Monix has version available for 2.13
      //"io.monix"    %% "monix-eval"    % monixVersion
    ),
    fork in Test := true,

    // postgis is `provided` dependency for users, and section from book of doobie needs it
    libraryDependencies += postgisDep,

    // Settings for sbt-microsites https://47deg.github.io/sbt-microsites/
    micrositeImgDirectory     := baseDirectory.value / "src/main/resources/microsite/img",
    micrositeName             := "doobie",
    micrositeDescription      := "A functional JDBC layer for Scala.",
    micrositeAuthor           := "Rob Norris",
    micrositeGithubOwner      := "tpolecat",
    micrositeGithubRepo       := "doobie",
    micrositeGitterChannel    := false, // no me gusta
    micrositeBaseUrl          := "/doobie",
    micrositeDocumentationUrl := "https://www.javadoc.io/doc/org.tpolecat/doobie-core_2.12",
    micrositeHighlightTheme   := "color-brewer",
    micrositePalette := Map(
      "brand-primary"     -> "#E35D31",
      "brand-secondary"   -> "#B24916",
      "brand-tertiary"    -> "#B24916",
      "gray-dark"         -> "#453E46",
      "gray"              -> "#837F84",
      "gray-light"        -> "#E3E2E3",
      "gray-lighter"      -> "#F4F3F4",
      "white-color"       -> "#FFFFFF"
    ),
    micrositeConfigYaml := ConfigYml(
      yamlCustomProperties = Map(
        "doobieVersion"    -> version.value,
        "catsVersion"      -> catsVersion,
        "fs2Version"       -> fs2CoreVersion(scalaVersion.value),
        "shapelessVersion" -> shapelessVersion,
        "h2Version"        -> h2Version,
        "postgresVersion"  -> postgresVersion,
        "scalaVersion"     -> scalaVersion.value,
        "scalaVersions"    -> crossScalaVersions.value.map(CrossVersion.partialVersion).flatten.map(_._2).mkString("2.", "/", "") // 2.11/12
      )
    ),
    micrositeExtraMdFiles := Map(
      file("CHANGELOG.md") -> ExtraMdFileConfig("changelog.md", "page", Map("title" -> "changelog", "section" -> "changelog", "position" -> "4")),
      file("LICENSE")      -> ExtraMdFileConfig("license.md",   "page", Map("title" -> "license",   "section" -> "license",   "position" -> "5"))
    )
  )

lazy val refined = project
  .in(file("modules/refined"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core)
  .settings(doobieSettings)
  .settings(publishSettings)
  .settings(
    name := "doobie-refined",
    description := "Refined support for doobie.",
    libraryDependencies ++= Seq(
      "eu.timepit"            %% "refined"        % refinedVersion,
      "com.h2database"        %  "h2"             % h2Version          % "test"
    )
  )
