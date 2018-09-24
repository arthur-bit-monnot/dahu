package dahu.planning.tamp

import cats.Id
import dahu.model.functions.Fun2
import dahu.model.input._
import dahu.model.input.dsl._
import dahu.model.types.ProductTag
import dahu.model.problem.API
import dahu.solvers.MetaSolver
import dahu.z3.Z3PartialSolver

object Core extends App {

  case class Pose[F[_]](x: F[Double], y: F[Double])
  object Pose {
    implicit val tag: ProductTag[Pose] = ProductTag.ofProd[Pose]
  }

  val close = new Fun2[Pose[cats.Id], Pose[cats.Id], Boolean] {
    override def of(in1: Pose[Id], in2: Pose[Id]): Boolean = math.abs(in1.x - in2.x) < 0.1
    override def name: String = "close"
  }
  val far = new Fun2[Pose[cats.Id], Pose[cats.Id], Boolean] {
    override def of(in1: Pose[Id], in2: Pose[Id]): Boolean = math.abs(in1.x - in2.x) > 1.0
    override def name: String = "far"
  }

  def pose(): Expr[Pose[cats.Id]] = Product[Pose](Pose[Expr](Input(), Input()))(Pose.tag)
  val p1 = pose()
  val p2 = pose()

  val pb = Cst(true).subjectTo(_ => close(p1, p2) || far(p1, p2))
  println(pb)
  val res = API.parseAndProcess(pb)
  println(res.fullTree)

  solve(pb)

  def solve[T](e: Expr[T]): Unit = {
    val solver = MetaSolver.of(e, Z3PartialSolver.builder)
    val sol = solver.nextSolution(None)

    println(sol)

  }

}
