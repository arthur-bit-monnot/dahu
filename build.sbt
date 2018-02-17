name := "dahu"



lazy val commonSettings = Seq(
  organization := "com.github.arthur-bit-monnot",
  scalaVersion := "2.12.4",
  crossPaths := true,

  // To sync with Maven central
  publishMavenStyle := true,

  // POM settings for Sonatype
  homepage := Some(url("https://github.com/arthur-bit-monnot/copla")),
  scmInfo := Some(ScmInfo(url("https://github.com/arthur-bit-monnot/copla"), "git@github.com:arthur-bit-monnot/fape.git")),
  developers += Developer("abitmonn", "Arthur Bit-Monnot", "arthur.bit-monnot@laas.fr", url("https://github.com/arthur-bit-monnot")),
  licenses += ("BSD-2-Clause", url("https://opensource.org/licenses/BSD-2-Clause")),
  pomIncludeRepository := (_ => false),

  excludeDependencies += "org.typelevel" % "scala-library", // pulled by matryoshka and causing problems in IntelliJ and sbt-assembly
  scalacOptions ++= Seq(
    "-target:jvm-1.8",
    "-encoding",
    "UTF-8",
    "-unchecked",
    "-deprecation",
    //  "-Xfuture",
    "-Ypartial-unification",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    //  "-Ywarn-unused",
    "-feature",
    "-language:higherKinds",
    "-language:existentials"
  ),


  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
  addCompilerPlugin("io.tryp" % "splain" % "0.2.7" cross CrossVersion.patch),

  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest"       % "3.0.4" % "test",
    "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  )
)

lazy val root = project.in(file(".")).
  aggregate(utils, staging).
  settings(
    publish := {},
    publishLocal := {}
  )
lazy val utils = project
  .in(file("dahu-utils"))
  .settings(name := "dahu-utils")
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= Seq(
    "org.spire-math" %% "debox" % "0.8.0",
  ))


lazy val staging = project
  .in(file("dahu-staging"))
  .dependsOn(utils)
  .settings(name := "dahu-staging")
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= Seq(
    "com.slamdata"  %% "matryoshka-core" % "0.21.3",
    "org.typelevel" %% "spire"           % "0.14.1",
    "org.typelevel" %% "cats-core"       % "0.9.0",
    "com.chuusai"   %% "shapeless"       % "2.3.2",
    "com.propensive" %% "magnolia" % "0.6.1",
  ))





resolvers += Resolver.sonatypeRepo("releases")


exportJars := true
