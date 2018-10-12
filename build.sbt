name := "dahu"

//scalafixSettings

lazy val commonSettings = Seq(
  organization := "com.github.arthur-bit-monnot",
  scalaVersion := "2.12.7",
  crossPaths := true,
  // To sync with Maven central
  publishMavenStyle := true,
  // POM settings for Sonatype
  homepage := Some(url("https://github.com/arthur-bit-monnot/copla")),
  scmInfo := Some(
    ScmInfo(url("https://github.com/arthur-bit-monnot/copla"),
            "git@github.com:arthur-bit-monnot/fape.git")),
  developers += Developer("abitmonn",
                          "Arthur Bit-Monnot",
                          "arthur.bit-monnot@laas.fr",
                          url("https://github.com/arthur-bit-monnot")),
  licenses += ("BSD-2-Clause", url("https://opensource.org/licenses/BSD-2-Clause")),
  pomIncludeRepository := (_ => false),
  resolvers += Resolver.mavenLocal,
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
//    "-Ywarn-value-discard", // to many false positives
    //  "-Ywarn-unused",
    "-feature",
    "-language:higherKinds",
    "-language:existentials",
    // experimental option to speed up the build require 2.12.5+
     "-Ycache-plugin-class-loader:last-modified",
     "-Ycache-macro-class-loader:last-modified",
     "-Ybackend-parallelism", "3"
//    "-opt:simplify-jumps",
//    "-opt:compact-locals",
//    "-opt:copy-propagation",
//    "-opt:box-unbox",
//    "-opt:closure-invocations",
//    "-opt:unreachable-code"
  ),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6"),
  //addCompilerPlugin("io.tryp" % "splain" % "0.2.10" cross CrossVersion.patch),
)
lazy val utestSettings = Seq(
  libraryDependencies += "com.lihaoyi" %% "utest" % "0.6.4" % "test",
  testFrameworks += new TestFramework("utest.runner.Framework")
)

lazy val root = project
  .in(file("."))
  .aggregate(utils, recursion, model,
    //solvers, benchmarks,
    anmlParser)
  .settings(
    scalaVersion := "2.12.6",
    publish := {},
    publishLocal := {}
  )

lazy val algebra = project
  .in(file("core/algebra"))
  .settings(name := "dahu-core-algebra")
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= Seq(
     "org.typelevel" %% "cats-core" % "1.1.0",
    "org.typelevel" %% "spire" % "0.14.1"
  ))

lazy val graphs = project
  .in(file("graphs"))
  .settings(name := "dahu-graphs")
  .dependsOn(recursion, utils)
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= Seq(
     "org.typelevel" %% "cats-core" % "1.1.0",
    "org.typelevel" %% "spire" % "0.14.1"
  ))

lazy val planningModel = project
  .in(file("planning/model"))
  .settings(name := "dahu-planning-model")
  .dependsOn(algebra)
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= Seq(
    "com.chuusai" %% "shapeless" % "2.3.3",
    "org.typelevel" %% "spire" % "0.14.1"
  ))

lazy val anmlParser = project
  .in(file("planning/anml/parser"))
  .settings(name := "dahu-anml")
  .dependsOn(planningModel)
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= Seq(
    "com.lihaoyi" %% "fastparse" % "1.0.0",
    "com.github.scopt" %% "scopt" % "3.7.0",
    "com.chuusai" %% "shapeless" % "2.3.3",
    "org.scalatest" %% "scalatest" % "3.0.5" % "test"
  ))

lazy val pddlParser = project
  .in(file("planning/pddl/parser"))
  .settings(name := "dahu-pddl")
  .dependsOn(planningModel, utils)
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= Seq(
    "com.github.pellierd" % "pddl4j" % "3.7.2"
  ))

lazy val pddlProblems = project
  .in(file("planning/pddl/problems"))
  .settings(name := "dahu-pddl-problems")
  .settings(commonSettings: _*)
  .settings(resourceDirectory in Compile := baseDirectory.value / "resources": _*)
  .settings(libraryDependencies ++= Seq(
    "com.github.alexarchambault" %% "case-app" % "2.0.0-M3"
  ))

lazy val utils = project
  .in(file("utils"))
  .settings(name := "dahu-utils")
  .settings(commonSettings ++ utestSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.spire-math" %% "debox" % "0.8.0",
      "org.typelevel" %% "cats-core" % "1.1.0",
    ))

lazy val recursion = project
  .in(file("recursion"))
  .dependsOn(utils)
  .settings(name := "recursion")
  .settings(commonSettings ++ utestSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "1.1.0",
      "org.typelevel" %% "cats-free" % "1.1.0",
    ))

lazy val model = project
  .in(file("core/model"))
  .dependsOn(utils, recursion, algebra, graphs)
  .settings(name := "dahu-model")
  .settings(commonSettings ++ utestSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.3",
    ))

lazy val solvers = project
  .in(file("core/solvers"))
  .dependsOn(utils, model)
  .settings(name := "dahu-solvers")
  .settings(commonSettings ++ utestSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.13.5" % "test",
    ))
//
lazy val z3 = project
  .in(file("core/solvers/z3"))
  .dependsOn(utils, model, solvers)
  .settings(name := "dahu-z3")
  .settings(commonSettings ++ utestSettings: _*)
//
//lazy val benchmarks = project
//  .in(file("benchmarks"))
//  .dependsOn(utils, solvers, z3, pddlPlanner, anmlPlanner, pddlProblems)
//  .settings(commonSettings ++ utestSettings: _*)
//  .settings(libraryDependencies ++= Seq(
//      "com.lihaoyi" %% "fastparse" % "1.0.0",
//      "com.lihaoyi" %% "ammonite-ops" % "1.1.1",
//      "com.github.alexarchambault" %% "case-app" % "2.0.0-M3"
//  ))
//
lazy val planner = project
  .in(file("planning/planner"))
  .dependsOn(anmlParser, solvers, z3, pddlProblems % "compile->test")
  .settings(commonSettings ++ utestSettings: _*)
  .settings(libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-effect" % "1.0.0-RC",
    "com.github.alexarchambault" %% "case-app" % "2.0.0-M3"
  ))

lazy val anmlPlanner = project
  .in(file("planning/anml/planner"))
  .dependsOn(anmlParser, planner)
  .settings(commonSettings ++ utestSettings: _*)
  .settings(
    mainClass in assembly := Some("dahu.planning.anml.planner.Main"),
    assemblyJarName in assembly := "dahu-anml-planner.jar"
  )
  .settings(
    libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt" % "3.7.0",
      "org.typelevel" %% "cats-effect" % "1.0.0-RC"
    ))

lazy val pddlPlanner = project
  .in(file("planning/pddl/planner"))
  .dependsOn(pddlParser, planner)
  .settings(commonSettings ++ utestSettings: _*)
  .settings(
    mainClass in assembly := Some("dahu.planning.pddl.planner.Main"),
    assemblyJarName in assembly := "dahu-pddl-planner.jar"
  )
  .settings(
    libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt" % "3.7.0",
      "com.lihaoyi" %% "ammonite-ops" % "1.1.0"
    ))
//
//lazy val rcllPlanner = project
//  .in(file("planning/rcll"))
//  .dependsOn(pddlPlanner)
//  .settings(commonSettings: _*)
//  .settings(
//    name := "rcll-parser",
//    libraryDependencies ++= Seq(
//      "com.lihaoyi" %% "fastparse" % "1.0.0",
//      "com.lihaoyi" %% "ammonite-ops" % "1.1.1",
//      "com.github.alexarchambault" %% "case-app" % "2.0.0-M3"
//    )
//  )
//
//lazy val tampPlanner = project
//  .in(file("planning/tamp"))
//  .dependsOn(solvers, z3)
//  .settings(commonSettings ++ utestSettings: _*)
//  .settings(
//    libraryDependencies ++= Seq(
//      "com.github.scopt" %% "scopt" % "3.7.0",
//      "com.lihaoyi" %% "ammonite-ops" % "1.1.0"
//    ))

resolvers += Resolver.sonatypeRepo("releases")

exportJars := true
