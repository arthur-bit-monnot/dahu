import mill._
import mill.scalalib._
import ammonite.ops._

trait Module extends SbtModule {
  def scalaVersion = "2.12.4"

  def compileIvyDeps = Agg(ivy"org.spire-math::kind-projector:0.9.6")
  def scalacPluginIvyDeps = Agg(ivy"org.spire-math::kind-projector:0.9.6")

  def scalacOptions = Seq(
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
    "-language:existentials",

    // experimental option to speed up the build        
//    "-Ycache-plugin-class-loader:last-modified",
//    "-Ycache-macro-class-loader:last-modified"
  )

   def unmanagedClasspath = T {
     if (!ammonite.ops.exists(millSourcePath / "lib")) Agg()  else
     Agg.from(ammonite.ops.ls(millSourcePath / "lib").map(PathRef(_)))
   }
}

object anml extends Module {
  def mainClass = Some("copla.lang.ParserApp")

  def ivyDeps = Agg(
    ivy"com.lihaoyi::fastparse:1.0.0",
    ivy"com.github.scopt::scopt:3.7.0",
    ivy"com.github.arthur-bit-monnot::landscaper:0.1.2"
  )

  object tests extends Tests {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.0.5")
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
}

object recursion extends Module {
  def moduleDeps = Seq(utils)
  def ivyDeps = Agg(
    ivy"org.typelevel::cats-core:1.1.0",
    ivy"org.typelevel::cats-free:1.1.0"
  )

  object tests extends Tests {
    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.6.4")
    def testFrameworks = Seq("utest.runner.Framework")
  }
}

object utils extends Module {
  def ivyDeps = Agg(
    ivy"org.spire-math::debox:0.8.0",
    ivy"org.typelevel::cats-core:1.1.0"
  )

  object tests extends Tests {
    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.6.4")
    def testFrameworks = Seq("utest.runner.Framework")
  }
}

object model extends Module {
  def moduleDeps = Seq(utils, recursion)
  def ivyDeps = Agg(ivy"com.chuusai::shapeless:2.3.3")

  object tests extends Tests {
    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.6.4")
    def testFrameworks = Seq("utest.runner.Framework")
  }
}

object solvers extends Module {
  def moduleDeps = Seq(utils, model)

  object tests extends Tests {
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest:0.6.4",
      ivy"org.scalacheck::scalacheck:1.13.5"
    )
    def testFrameworks = Seq("utest.runner.Framework")
  }
}

object z3 extends Module {
  def moduleDeps = Seq(utils, model, solvers)
}

object planner extends Module {
  def moduleDeps = Seq(anml, solvers, z3)

  def mainClass = Some("dahu.planner.Main")

  def ivyDeps = Agg(
    ivy"com.github.scopt::scopt:3.7.0",
    ivy"io.monix::monix:3.0.0-RC1"
  )

    object tests extends Tests {
    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.6.4")
    def testFrameworks = Seq("utest.runner.Framework")
  }


  def bundle() = T.command {
    val fatjar: PathRef = assembly()
    val dest = T.ctx.dest
    val z3Dir = Path("/home/arthur/work/ext/z3-4.6.0-x64-ubuntu-16.04/bin")
    cp(fatjar.path, dest / "lcp.jar")
    mkdir(dest / "z3")
    cp(z3Dir / "libz3.so", dest / "z3" / "libz3.so")
    cp(z3Dir / "libz3java.so", dest / "z3" / "libz3java.so")
    cp(z3Dir / "com.microsoft.z3.jar", dest / "z3" / "com.microsoft.z3.jar")
    write(dest / "lcp",
      """#!/bin/bash
        |
        |DIR=$(dirname "$0")
        |export LD_LIBRARY_PATH=$DIR/z3:$LD_LIBRARY_PATH
        |
        |java -cp $DIR/z3/com.microsoft.z3.jar:$DIR/lcp.jar dahu.planner.Main "$@"
      """.stripMargin)
    //cp(Path("/home/arthur/work/fape/planning/domains/tmp"), dest / "domains")

    println(s"Bundled in ${T.ctx.dest}")
  }
}  