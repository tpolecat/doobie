import FreeGen2._

// Library versions all in one place, for convenience and sanity.
lazy val catsVersion          = "2.1.1"
lazy val catsEffectVersion    = "2.1.2"
lazy val circeVersion         = "0.13.0"
lazy val collCompatVersion    = "2.1.4"
lazy val fs2Version           = "2.3.0"
lazy val h2Version            = "1.4.200"
lazy val hikariVersion        = "3.4.2"
lazy val kindProjectorVersion = "0.11.0"
lazy val monixVersion         = "3.1.0"
lazy val quillVersion         = "3.5.1"
lazy val postGisVersion       = "2.5.0"
lazy val postgresVersion      = "42.2.12"
lazy val refinedVersion       = "0.9.13"
lazy val scalaCheckVersion    = "1.14.2"
lazy val scalatestVersion     = "3.1.1"
lazy val shapelessVersion     = "2.3.3"
lazy val silencerVersion      = "1.6.0"
lazy val sourcecodeVersion    = "0.2.1"
lazy val specs2Version        = "4.9.2"
lazy val scala212Version      = "2.12.10"
lazy val scala213Version      = "2.13.1"
lazy val slf4jVersion         = "1.7.30"
lazy val monocleVersion       = "2.0.4"

// These are releases to ignore during MiMa checks
lazy val botchedReleases = Set("0.8.0", "0.8.1")

// This is used in a couple places. Might be nice to separate these things out.
lazy val postgisDep = "net.postgis" % "postgis-jdbc" % postGisVersion

lazy val compilerFlags = Seq(
  scalacOptions in (Compile, console) ++= Seq(
    "-Ydelambdafy:inline",    // http://fs2.io/faq.html
    "-P:silencer:checkUnused" // https://github.com/ghik/silencer#detecting-unused-annotations
  ),
  scalacOptions in (Compile, doc) --= Seq(
    "-Xfatal-warnings"
  ),
  libraryDependencies ++= Seq(
    compilerPlugin("com.github.ghik" % "silencer-plugin" % silencerVersion cross CrossVersion.full),
    "com.github.ghik" % "silencer-lib" % silencerVersion % Provided cross CrossVersion.full,
    "org.scala-lang.modules" %% "scala-collection-compat" % collCompatVersion
  )
)

lazy val buildSettings = Seq(
  organization := "org.tpolecat",
  licenses ++= Seq(("MIT", url("http://opensource.org/licenses/MIT")))
)

lazy val commonSettings =
  compilerFlags ++
  Seq(
    scalaVersion := scala213Version,
    crossScalaVersions := Seq(scala212Version, scala213Version),

    // These sbt-header settings can't be set in ThisBuild for some reason
    headerMappings := headerMappings.value + (HeaderFileType.scala -> HeaderCommentStyle.cppStyleLineComment),
    headerLicense  := Some(HeaderLicense.Custom(
      """|Copyright (c) 2013-2018 Rob Norris and Contributors
         |This software is licensed under the MIT License (MIT).
         |For more information see LICENSE or https://opensource.org/licenses/MIT
         |""".stripMargin
    )),

    scalacOptions in (Compile, doc) ++= Seq(
      "-groups",
      "-sourcepath", (baseDirectory in LocalRootProject).value.getAbsolutePath,
      "-doc-source-url", "https://github.com/tpolecat/doobie/blob/v" + version.value + "€{FILE_PATH}.scala"
    ),
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck"        % scalaCheckVersion % "test",
      "org.specs2"     %% "specs2-core"       % specs2Version     % "test",
      "org.specs2"     %% "specs2-scalacheck" % specs2Version     % "test"
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector" % kindProjectorVersion cross CrossVersion.full),
  )

lazy val publishSettings = Seq(
  homepage := Some(url("https://github.com/tpolecat/doobie")),
  developers := List(
    Developer("tpolecat", "Rob Norris", "rob_norris@mac.com", url("http://www.tpolecat.org"))
  ),
  mappings in (Compile, packageSrc) ++= (managedSources in Compile).value pair sbt.io.Path.relativeTo(sourceManaged.value / "main" / "scala"),
  mimaPreviousArtifacts ~= { as => as.filterNot(a => botchedReleases.contains(a.revision)) }
)

lazy val noPublishSettings = Seq(
  skip in publish := true,
  mimaPreviousArtifacts := Set()
)

lazy val doobieSettings = buildSettings ++ commonSettings

lazy val doobie = project.in(file("."))
  .settings(doobieSettings)
  .settings(noPublishSettings)
  .aggregate(
    bench,
    core,
    docs,
    example,
    free,
    h2,
    hikari,
    postgres,
    `postgres-circe`,
    quill,
    refined,
    scalatest,
    specs2,
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
      "co.fs2"         %% "fs2-core"    % fs2Version,
      "org.typelevel"  %% "cats-core"   % catsVersion,
      "org.typelevel"  %% "cats-free"   % catsVersion,
      "org.typelevel"  %% "cats-effect" % catsEffectVersion,
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
      "com.h2database"        %  "h2"            % h2Version % "test",
    ),

    scalacOptions += "-Yno-predef",
    unmanagedSourceDirectories in Compile += {
      val sourceDir = (sourceDirectory in Compile).value
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, n)) if n <= 12 => sourceDir / "scala-2.13-"
        case _                       => sourceDir / "scala-2.13+"
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
    scalacOptions in Compile +=  "-Ymacro-annotations",
    libraryDependencies ++= Seq(
      "co.fs2"                     %% "fs2-io"       % fs2Version,
      "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion
    )
  )

lazy val postgres = project
  .in(file("modules/postgres"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core % "compile->compile;test->test")
  .settings(doobieSettings)
  .settings(publishSettings)
  .settings(freeGen2Settings)
  .settings(
    name  := "doobie-postgres",
    description := "Postgres support for doobie.",
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-io"     % fs2Version,
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
  .dependsOn(core % "compile->compile;test->test")
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
  .dependsOn(postgres % "test")
  .settings(doobieSettings)
  .settings(publishSettings)
  .settings(
    name := "doobie-hikari",
    description := "Hikari support for doobie.",
    libraryDependencies ++= Seq(
      "com.zaxxer"     % "HikariCP"   % hikariVersion,
      "com.h2database" % "h2"         % h2Version      % "test",
      "org.slf4j"      % "slf4j-nop"  % slf4jVersion   % "test"
    )
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
      "org.scalatest" %% "scalatest" % scalatestVersion,
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
  .dependsOn(core, postgres, specs2, hikari, h2, scalatest, quill)
  .enablePlugins(ParadoxPlugin)
  .enablePlugins(ParadoxSitePlugin)
  .enablePlugins(GhpagesPlugin)
  .enablePlugins(MdocPlugin)
  .settings(doobieSettings)
  .settings(noPublishSettings)
  .settings(
    scalacOptions := Nil,

    libraryDependencies ++= Seq(
      "io.circe"    %% "circe-core"    % circeVersion,
      "io.circe"    %% "circe-generic" % circeVersion,
      "io.circe"    %% "circe-parser"  % circeVersion,
      "io.monix"    %% "monix-eval"    % monixVersion,
    ),
    fork in Test := true,

    // postgis is `provided` dependency for users, and section from book of doobie needs it
    libraryDependencies += postgisDep,


    // Settings for sbt-microsites https://47deg.github.io/sbt-microsites/
    // version                   := version.value.takeWhile(_ != '+'), // strip off the +3-f22dca22+20191110-1520-SNAPSHOT business
    // micrositeImgDirectory     := baseDirectory.value / "src/main/resources/microsite/img",
    // micrositeName             := "doobie",
    // micrositeDescription      := "A functional JDBC layer for Scala.",
    // micrositeAuthor           := "Rob Norris",
    // micrositeGithubOwner      := "tpolecat",
    // micrositeGithubRepo       := "doobie",
    // micrositeGitterChannel    := false, // no me gusta
    // micrositeBaseUrl          := "/doobie",
    // micrositeDocumentationUrl := "https://www.javadoc.io/doc/org.tpolecat/doobie-core_2.12",
    // micrositeHighlightTheme   := "color-brewer",
    // micrositePalette := Map(
    //   "brand-primary"     -> "#E35D31",
    //   "brand-secondary"   -> "#B24916",
    //   "brand-tertiary"    -> "#B24916",
    //   "gray-dark"         -> "#453E46",
    //   "gray"              -> "#837F84",
    //   "gray-light"        -> "#E3E2E3",
    //   "gray-lighter"      -> "#F4F3F4",
    //   "white-color"       -> "#FFFFFF"
    // ),
    // micrositeConfigYaml := ConfigYml(
    //   yamlCustomProperties = Map(
    //     "doobieVersion"    -> version.value,
    //     "catsVersion"      -> catsVersion,
    //     "fs2Version"       -> fs2Version,
    //     "shapelessVersion" -> shapelessVersion,
    //     "h2Version"        -> h2Version,
    //     "postgresVersion"  -> postgresVersion,
    //     "scalaVersion"     -> scalaVersion.value,
    //     "scalaVersions"    -> (crossScalaVersions in core).value.flatMap(CrossVersion.partialVersion).map(_._2).mkString("2.", "/", ""), // 2.12/13
    //     "quillVersion"     -> quillVersion
    //   )
    // ),
    // micrositeExtraMdFiles := Map(
    //   file("CHANGELOG.md") -> ExtraMdFileConfig("changelog.md", "page", Map("title" -> "changelog", "section" -> "changelog", "position" -> "4")),
    //   file("LICENSE")      -> ExtraMdFileConfig("license.md",   "page", Map("title" -> "license",   "section" -> "license",   "position" -> "5"))
    // ),
    // micrositeCompilingDocsTool := WithMdoc,
    // mdocIn                     := sourceDirectory.value / "main" / "tut"

    git.remoteRepo     := "git@github.com:tpolecat/doobie.git",
    ghpagesNoJekyll    := true,
    publish / skip     := true,
    paradoxTheme       := Some(builtinParadoxTheme("generic")),
    version            := version.value.takeWhile(_ != '+'), // strip off the +3-f22dca22+20191110-1520-SNAPSHOT business
    paradoxProperties ++= Map(
      "scala-versions"           -> (crossScalaVersions in core).value.map(CrossVersion.partialVersion).flatten.map(_._2).mkString("2.", "/", ""),
      "org"                      -> organization.value,
      "scala.binary.version"     -> s"2.${CrossVersion.partialVersion(scalaVersion.value).get._2}",
      "core-dep"                 -> s"${(core / name).value}_2.${CrossVersion.partialVersion(scalaVersion.value).get._2}",
      "version"                  -> version.value,
      "scaladoc.doobie.base_url" -> s"https://static.javadoc.io/org.tpolecat/doobie-core_2.12/${version.value}",
      "catsVersion"              -> catsVersion,
      "fs2Version"               -> fs2Version,
      "shapelessVersion"         -> shapelessVersion,
      "h2Version"                -> h2Version,
      "postgresVersion"          -> postgresVersion,
      "quillVersion"             -> quillVersion,
      "scalaVersion"             -> scalaVersion.value,
    ),

    mdocIn := (baseDirectory.value) / "src" / "main" / "mdoc",
    Compile / paradox / sourceDirectory := mdocOut.value,
    makeSite := makeSite.dependsOn(mdoc.toTask("")).value,

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
      "eu.timepit"     %% "refined" % refinedVersion,
      "com.h2database" %  "h2"      % h2Version       % "test"
    )
  )

lazy val quill = project
  .in(file("modules/quill"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core)
  .dependsOn(postgres % "test")
  .settings(doobieSettings)
  .settings(publishSettings)
  .settings(
    name := "doobie-quill",
    description := "Quill support for doobie.",
    libraryDependencies ++= Seq(
      "io.getquill" %% "quill-jdbc"   % quillVersion,
      "org.slf4j"   %  "slf4j-simple" % slf4jVersion % "test"
    ),
  )
