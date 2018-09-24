package dahu.z3

import com.microsoft.z3._

object SolverInterfaceTest extends App {

  val ctx = new Context()

  val a = ctx.mkIntConst("a")
  val b = ctx.mkIntConst("b")

  val solver = ctx.mkSolver()
  solver.add(ctx.mkLe(a, b))
  val res = solver.check()
//  val model = solver.ge
  println(solver.getModel)
  solver.add(ctx.mkLt(a, b))
  println(solver.check())
  println(solver.getModel)

}
