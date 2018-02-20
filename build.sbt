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

  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6"),
//  addCompilerPlugin("io.tryp" % "splain" % "0.2.7" cross CrossVersion.patch),

  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest"       % "3.0.5" % "test",
    "org.scalacheck" %% "scalacheck" % "1.13.5" % "test",
  )
)

lazy val root = project.in(file("."))
  .aggregate(utils, recursion, model)
//  .aggregate(utils, model, solvers)
  .settings(
    publish := {},
    publishLocal := {}
  )
lazy val utils = project
  .in(file("dahu-utils"))
  .settings(name := "dahu-utils")
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= Seq(
    "org.spire-math" %% "debox" % "0.8.0",
    "org.typelevel" %% "spire"           % "0.14.1",
  ))

lazy val recursion = project
  .in(file("recursion"))
  .settings(name := "recursion")
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "1.0.1",
    "org.typelevel" %% "cats-free" % "1.0.1",
//     "org.scalaz" %% "scalaz-core" % "7.2.17",
    "com.lihaoyi" %% "utest" % "0.5.4" % "test"
  ))
  .settings(testFrameworks += new TestFramework("utest.runner.Framework"))

lazy val model = project
  .in(file("dahu-model"))
  .dependsOn(utils, recursion)
  .settings(name := "dahu-model")
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= Seq(
//    "com.slamdata"  %% "matryoshka-core" % "0.21.3",
    "org.typelevel" %% "cats-core"       % "0.9.0",
    "com.chuusai"   %% "shapeless"       % "2.3.3",
  ))

//
//lazy val solvers = project
//  .in(file("dahu-solvers"))
//  .dependsOn(utils, model)
//  .settings(name := "dahu-solvers")
//  .settings(commonSettings: _*)

//lazy val benchmarks = project
//  .in(file("dahu-benchmarks"))
//  .dependsOn(utils, solvers)
//  .settings(commonSettings: _*)



resolvers += Resolver.sonatypeRepo("releases")


exportJars := true
