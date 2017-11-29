name := "dahu"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.4"

libraryDependencies += "com.slamdata" %% "matryoshka-core" % "0.21.3"

scalacOptions += "-Ypartial-unification"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

assemblyExcludedJars in assembly := {
    val cp = (fullClasspath in assembly).value

    cp filter { af =>
      val file = af.data

      (file.getName == "scala-library-" + scalaVersion.value + ".jar") &&
        (file.getPath contains "org.scala-lang")
    }
}

retrieveManaged := true

exportJars := true
