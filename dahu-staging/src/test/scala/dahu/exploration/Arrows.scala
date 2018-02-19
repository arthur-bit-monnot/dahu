package dahu.exploration

import dahu.arrows.recursion.Algebra
import dahu.arrows.{==>, Arrow, MutableInputs}
import dahu.ast.ASTable
import dahu.expr.BagPacking
import dahu.ast.Ops._
import dahu.recursion.{ComputationF, CstF, ExprF, InputF}
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

      val ev2 = ast.hylo(evaluationAlgebra)
      val ev3 = ast.memoizedHylo(evaluationAlgebra)

      for(i <- ast.ids.enumerate) {
        assert(ev(i) == ev2(i))
        assert(ev(i) == ev3(i))
      }

    }

    "mutation" in {
      import cats._
      import cats.implicits._
      def evalAlgebra(ast: ASTable)(
          inputs: MutableInputs[ast.Variable, Value]): Algebra[ExprF, Option[Value]] =
        Arrow.lift {
          case CstF(v, _)               => Some(v)
          case x @ InputF(_, _)         => inputs(x)
          case ComputationF(f, args, _) => args.toList.sequence.map(as => Value(f.compute(as)))
        }

      val inputs = new MutableInputs[ast.Variable, Value]()
      val alg    = evalAlgebra(ast)(inputs)
      val ev     = ast.hylo(alg)

      assert(ev(ast.root).isEmpty)

      for(i <- ast.variableIds.enumerate.map(i => ast.variableCoalgebra(i))) {
        inputs.update(i, Value(false))
      }

      assert(ev(ast.root).contains(true))
      for(i <- ast.variableIds.enumerate.map(i => ast.variableCoalgebra(i))) {
        inputs.update(i, Value(true))
      }
      assert(ev(ast.root).contains(false))

      inputs.unset(ast.variableIds.enumerate.map(i => ast.variableCoalgebra(i)).head)
      assert(ev(ast.root).isEmpty)
    }
  }

}
