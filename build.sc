import coursier.maven.MavenRepository
import mill._
import mill.define.Task
import mill.scalalib._

trait Module extends SbtModule {
  def scalaVersion = "2.12.6"
  
 def compileIvyDeps = Agg(
   ivy"org.spire-math::kind-projector:0.9.6",
//   ivy"io.tryp:::splain:0.3.1"
 )
 def scalacPluginIvyDeps = Agg(
   ivy"org.spire-math::kind-projector:0.9.6",
//   ivy"io.tryp:::splain:0.3.1"
 )

 def scalacOptions = Seq(
   "-target:jvm-1.8",
   "-encoding",
   "UTF-8",
   "-unchecked",
   "-deprecation",
   "-Ypartial-unification",
   // "-Yno-adapted-args",
   // "-Ywarn-dead-code",
   // "-Ywarn-numeric-widen",
   // "-Ywarn-value-discard",
   //  "-Ywarn-unused",
   "-feature",
   "-language:higherKinds",
   "-language:existentials",

   // experimental option to speed up the build
   "-Ycache-plugin-class-loader:last-modified",
   "-Ycache-macro-class-loader:last-modified",
   "-Ybackend-parallelism", "3"
 )

  def unmanagedClasspath = T {
    val own =
      if (!ammonite.ops.exists(millSourcePath / "lib")) Agg()
      else Agg.from(ammonite.ops.ls(millSourcePath / "lib").map(PathRef(_)))
    own ++ Task.traverse(moduleDeps)(_.unmanagedClasspath)().flatten
  }

   override def repositories = super.repositories ++ Seq(
     MavenRepository("file:///home/arthur/.m2/repository")
   )
}
trait ModuleTests extends Module { outer =>
  object test extends Tests {
    override def ivyDeps = Agg(deps.utest, deps.scalatest, deps.scalacheck)
    override def testFrameworks = Seq("utest.runner.Framework", "org.scalatest.tools.Framework")
    override def unmanagedClasspath = outer.unmanagedClasspath
  }
}
trait EmptyModule extends mill.Module {}


object deps {
  val cats = ivy"org.typelevel::cats-core:1.4.0"
  val catsFree = ivy"org.typelevel::cats-free:1.4.0"
  val catsEffect = ivy"org.typelevel::cats-effect:1.0.0"
  val spire = ivy"org.typelevel::spire:0.16.0"
  val shapeless = ivy"com.chuusai::shapeless:2.3.3"
  val fastparse = ivy"com.lihaoyi::fastparse:1.0.0"
  val scopt = ivy"com.github.scopt::scopt:3.7.0"
  val debox = ivy"org.spire-math::debox:0.8.0"
  val pddl4j = ivy"com.github.pellierd:pddl4j:3.7.2"
  val ammoniteOps = ivy"com.lihaoyi::ammonite-ops:1.2.1"
  val caseApp = ivy"com.github.alexarchambault::case-app:2.0.0-M3"

  val utest = ivy"com.lihaoyi::utest:0.6.5"
  val scalatest = ivy"org.scalatest::scalatest:3.0.5"
  val scalacheck = ivy"org.scalacheck::scalacheck:1.14.0"
}
import deps._

object core extends EmptyModule {
  object algebra extends Module {
    override def ivyDeps = Agg(cats, spire)
  }
  object model extends ModuleTests {
    def moduleDeps = Seq(algebra, utils, recursion)
    def ivyDeps = Agg(shapeless)
  }
  object solvers extends ModuleTests {
    def moduleDeps = Seq(model)

    object z3 extends Module {
      def moduleDeps = Seq(utils, model, solvers)
    }
  }
}

object planning extends EmptyModule {
  object model extends Module {
    def moduleDeps = Seq(core.algebra)
    def ivyDeps = Agg(spire, shapeless)
  }
  object planner extends Module {
    def moduleDeps = Seq(planning.model, core.solvers, core.solvers.z3)
    def ivyDeps = Agg(catsEffect)
  }
  object anml extends EmptyModule {
    object parser extends ModuleTests {
      def moduleDeps = Seq(planning.model)
      def ivyDeps = Agg(fastparse, scopt)
      def mainClass = Some("dahu.planning.anml.parser.ParserApp")
    }
    object planner extends Module {
      def moduleDeps = Seq(planning.anml.parser, planning.planner)
      def ivyDeps = Agg(scopt)
      def mainClass = Some("dahu.planning.anml.planner.Main")
    }
  }
  object pddl extends EmptyModule {
    object parser extends Module {
      def moduleDeps = Seq(planning.model, utils)
      def ivyDeps = Agg(pddl4j)
    }
    object planner extends Module {
      def moduleDeps = Seq(planning.pddl.parser, planning.planner)
      def ivyDeps = Agg(scopt, ammoniteOps)
      def mainClass = Some("dahu.planning.pddl.planner.Main")
      object test extends Tests {
        override def unmanagedClasspath = planner.unmanagedClasspath
        override def moduleDeps = super.moduleDeps ++ Seq(planning.pddl.problems)
        override def ivyDeps = Agg(deps.utest)

        override def testFrameworks = Seq("utest.runner.Framework")
      }
    }
    object problems extends Module {
      override def resources = T.sources{ millSourcePath / 'resources }
    }
  }
}

object recursion extends ModuleTests {
 def moduleDeps = Seq(utils)
 def ivyDeps = Agg(cats, catsFree)
}

object utils extends ModuleTests {
 def ivyDeps = Agg(debox, cats)
}

object graphs extends ModuleTests {
 def moduleDeps = Seq(utils, recursion)
 def ivyDeps = Agg(cats, catsFree, debox)
}

object benchmarks extends ModuleTests {
  override def moduleDeps = Seq(core.solvers.z3, planning.pddl.planner, planning.pddl.problems, planning.anml.planner)
  override def ivyDeps = Agg(caseApp)
}