package dahu.model.lambdas
import dahu.graphs.{IDTop, SomeID}
import dahu.graphs.transformations._
import dahu.model.functions.->:
import dahu.model.input.{Cst, Expr, Lambda}
import dahu.model.input.dsl._
import dahu.model.ir.{ApplyF, StaticF, Total}
import dahu.model.problem.API

object Tests extends App {

  val sum: Expr[Int ->: Int ->: Int] = Lambda(a => Lambda[Int, Int](b => a + b))
  val min: Expr[Int ->: Int ->: Int] = Lambda(a => Lambda[Int, Int](b => ITE(a < b, a, b)))
  val inc: Expr[Int ->: Int] = Lambda[Int, Int](a => a + Cst(1))

  val zero: Expr[Int] = Cst(0)
  val one: Expr[Int] = Cst(1)
  val two: Expr[Int] = Cst(2)

  val e: Expr[Int] = sum(sum(inc(zero), one), inc(two))
  val e2: Expr[Int ->: Int] = sum.partialApply(e)
  val tree = API.eliminateDynamics(API.parse(e2), Nil)

  API.echo(tree)

  val expanded = API.expandLambdas(tree)
  API.echo(expanded)

  expanded.fixID

  val trans = new TransformationWithSubstitution[StaticF, Total] {
    override def transformation[I <: Int](
        retrieve: I => Total[I]): StaticF[I] => (Total[I], Option[I => I]) = {
      case ApplyF(lbd, param, _) =>
        // lambda application, return the tree of the lambda with a substitution that replaces the
        // param of the lambda by the target of the application
        retrieve(lbd) match {
          case dahu.model.ir.LambdaF(in, tree, _, _) =>
            val rt = retrieve(tree)
            (rt, Some(id => if(id == in) param else id))

          case _ => dahu.utils.errors.unexpected
        }
      case x: Total[I] => (x, None)
    }
  }

  val nolbd = tree.tree.transformWithSubstitution(trans).rootedAt(tree.root)

  API.echo(nolbd.postpro(dahu.model.transformations.optimizer))
}
