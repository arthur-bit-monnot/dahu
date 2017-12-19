package dahu.exploration

import dahu.arrows.{==>, Arrow}
import dahu.ast.ASTable
import dahu.expr.BagPacking
import dahu.expr.labels.Labels._
import dahu.ast.Ops._

object Arrows extends App {

  val bag = new BagPacking

  val ast: ASTable = toTable(bag.valid)

  def inputsByName(ast: ASTable, map: Map[String, Any]): ast.Variable ==> Value =
    Arrow.lift[ast.Variable, Value](x => Value(map.apply(x.name)))

  val inputs = inputsByName(ast, Map("x1" -> false, "x2" -> true))

  val ev = evaluator(ast)(inputs)

  println(ev(ast.root))

  for(i <- ast.ids.enumerate) {
    println(s"$i : ${ev(i)}")
  }

  println("Done")

}
