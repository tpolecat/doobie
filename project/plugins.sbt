addSbtPlugin("com.lightbend.paradox" % "sbt-paradox" % "0.10.7")
addSbtPlugin("com.github.sbt" % "sbt-site" % "1.8.0")
addSbtPlugin("com.github.sbt" % "sbt-site-paradox" % "1.8.0")
addSbtPlugin("com.github.sbt" % "sbt-ghpages" % "0.9.0")
addSbtPlugin("org.typelevel" % "sbt-typelevel-ci-release" % "0.8.7")
addSbtPlugin("org.typelevel" % "sbt-typelevel-mergify" % "0.8.7")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.4.4")
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.7.0")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")
addSbtPlugin("com.github.sbt" % "sbt-header" % "5.11.0")
addSbtPlugin("org.typelevel" % "sbt-tpolecat" % "0.5.7")
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.9.2")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.6.2")
addSbtPlugin("com.eed3si9n" % "sbt-projectmatrix" % "0.11.0")
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.14.9")
addDependencyTreePlugin

// Setting semanticdbVersion explicitly as the version is defaulting to 4.12.3
// which doesn't exist for 2.12.21
// Perhaps SBT / one of the plugins using need to update its version?
semanticdbVersion := "4.14.6"
