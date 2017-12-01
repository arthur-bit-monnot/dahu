name := "dahu"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.4"

libraryDependencies += "com.slamdata" %% "matryoshka-core" % "0.21.3"
libraryDependencies += "org.typelevel" %% "spire" % "0.14.1"
libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.4" % "test"

scalacOptions += "-Ypartial-unification"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

exportJars := true
