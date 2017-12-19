package dahu.exploration

import dahu.arrows.memoization.Memo.ArrayMemo
import dahu.arrows.{==>, Arrow}
import dahu.ast.ASTable
import dahu.expr.BagPacking
import dahu.expr.labels.Labels._
import dahu.ast.Ops._
import org.scalatest.FreeSpec

import scala.reflect.ClassTag

class Arrows extends FreeSpec {

  val bag = new BagPacking

  val ast: ASTable = toTable(bag.valid)

  def inputsByName(ast: ASTable, map: Map[String, Any]): ast.Variable ==> Value =
    Arrow.lift[ast.Variable, Value](x => Value(map.apply(x.name)))

  "arrows" - {
    "evaluation equivalence" in {
      val inputs = inputsByName(ast, Map("x1" -> false, "x2" -> true))

      val ev  = evaluator(ast)(inputs)
      val ev2 = asRecursiveFold(ast)(evalAlgebra(ast)(inputs))

      for(i <- ast.ids.enumerate)
        assert(ev(i) == ev2(i))

      implicit val instance = ast.ids
//      implicit val ct = ClassTag.Any.asInstanceOf[ClassTag[Value]]
      val memo = new ArrayMemo[ast.EId, Value](ev)

      println(memo.memory.mkString("\n"))
    }
  }

}
