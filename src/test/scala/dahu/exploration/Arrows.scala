package dahu.exploration

import dahu.arrows.memoization.Memo.ArrayMemo
import dahu.arrows.{==>, Arrow}
import dahu.ast.ASTable
import dahu.expr.BagPacking
import dahu.expr.labels.Labels._
import dahu.ast.Ops._
import dahu.recursion.ExprF
import org.scalatest.FreeSpec

class Arrows extends FreeSpec {

  val bag = new BagPacking

  val ast: ASTable = toTable(bag.valid)

  def inputsByName(ast: ASTable, map: Map[String, Any]): ast.Variable ==> Value =
    Arrow.lift[ast.Variable, Value](x => Value(map.apply(x.name)))

  "arrows" - {
    "evaluation equivalence" in {
      val inputs = inputsByName(ast, Map("x1" -> false, "x2" -> true))

      val ev = evaluator(ast)(inputs)

      val evaluationAlgebra: Algebra[ExprF, Value] = evalAlgebra(ast)(inputs)

      val ev2 = cata(ast)(evaluationAlgebra)
      val ev3 = memoizedCata(ast)(evaluationAlgebra)

      for(i <- ast.ids.enumerate) {
        assert(ev(i) == ev2(i))
        assert(ev(i) == ev3(i))
      }

    }
  }

}
