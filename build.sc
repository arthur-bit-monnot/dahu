import mill._
import mill.scalalib._

trait Module extends SbtModule {
  def scalaVersion = "2.12.5"

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
    "-Ycache-plugin-class-loader:last-modified",
    "-Ycache-macro-class-loader:last-modified"
  )
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
  def ivyDeps = Agg(ivy"org.spire-math::debox:0.8.0")

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