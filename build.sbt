name := "dahu"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.4"

libraryDependencies += "com.slamdata"  %% "matryoshka-core" % "0.21.3"
libraryDependencies += "org.typelevel" %% "spire"           % "0.14.1"
libraryDependencies += "org.typelevel" %% "cats-core"       % "0.9.0"
libraryDependencies += "org.scalatest" %% "scalatest"       % "3.0.4" % "test"
libraryDependencies += "com.chuusai"   %% "shapeless"       % "2.3.2"

libraryDependencies += "com.propensive" %% "magnolia" % "0.6.1"

unmanagedJars in Compile += file("lib/ILOG.CP.jar")

// pulled by matryoshka and causing problems in IntelliJ and sbt-assembly
excludeDependencies += "org.typelevel" % "scala-library"

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
)

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

addCompilerPlugin("io.tryp" % "splain" % "0.2.7" cross CrossVersion.patch)

exportJars := true
