package dahu.ast

import dahu.arrows.{==>, Arrow, TypeInstances}
import dahu.expr.labels.Labels.Value
import dahu.recursion._

object Ops {

  val toTable: dahu.expr.Expr[Any] ==> ASTable = Arrow.lift((x: dahu.expr.Expr[Any]) => {
    val (headInt, tableInt) = Algebras.encodeAsPair(x)
    new ASTable {
      private val vec: Array[ResultF[EId]] = tableInt.asInstanceOf[Vector[Expr]].toArray

      override val root: EId = headInt.asInstanceOf[EId]

      override val arrow: EId ==> Expr = Arrow.lift(x => vec(EId.unwrap(x)))

      override def ids: TypeInstances[EId] = new TypeInstances[EId] {
        override val enumerate: Array[EId] = vec.indices.toArray.asInstanceOf[Array[EId]]
      }
    }
  })

  def extractType[X]: ResultF[X] ==> String = Arrow.lift(_.typ.toString)

  def types(ast: ASTable): ast.EId ==> String = ast.arrow.andThen(extractType)

  def evaluator(ast: ASTable)(inputs: ast.Variable ==> Value): ast.EId ==> Value = {
    def go(i: ast.EId): Value = {
      ast.arrow(i) match {
        case CstF(v, _)               => Value(v)
        case x @ InputF(_, _)         => inputs(x)
        case ComputationF(f, args, _) => Value(f.compute(args.map(go)))
      }
    }
    Arrow.lift(go)
  }

}
