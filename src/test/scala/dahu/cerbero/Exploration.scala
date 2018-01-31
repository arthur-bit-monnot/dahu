package dahu.cerbero

import cats.Id
import dahu.cerberdo.Planning.Structs.IntervalF
import dahu.constraints.CSP
import dahu.recursion.{ComputationF, ExprF, ProductF}
import matryoshka.data.Fix
import matryoshka.instances.fixedpoint.Cofree
import matryoshka.patterns.EnvT

object Exploration extends App {

  import scala.reflect.runtime.universe._
  val x = weakTypeTag[IntervalF[Id]]
  println(x.toString())
  println(x.tpe.baseClasses)

  case class Pt(x: Expr[Int], y: Expr[Int])
  case class Opt[VVV](v: VVV, present: Expr[Boolean]) {
    def x: Opt[Expr[Int]] = Opt(v.asInstanceOf[Pt].x, present)
  }

}

object CPOptimizer extends App {

  import ilog.cp._

  val cp  = new IloCP()
  val itv = cp.intervalVar()
  val end = cp.intVar(0, 1000)
  cp.eq(cp.endOf(itv), end)

}

object Planner extends App {

  import Planning._

  val size      = 2
  val items     = 0 until size
  val locations = 0 until size + 1

  val targetLoc = CstInt(size)

  // timelines(i) = location(i)

  val start = AbsoluteTime(0)

  val initTokens = for(i <- 0 until size) yield {
    // item i is at the ith location
    val itv = Interval(start, intVar(), timepoint())
    Token(itv, i, CstInt(i), isEffect = true)
  }

  val goals = Seq(Token(interval(), 0, targetLoc, isEffect = false))

  val actions: Seq[Opt[Action]] = for(i <- 0 until size) yield {
    val itv     = interval()
    val initLoc = intVar()
    val cond    = Token(interval(10), i, initLoc, isEffect = false)
    val effect  = Token(interval(10), i, targetLoc, isEffect = true)
    val constraints = and(
      equal(itv.start, cond.itv.start),
      equal(itv.end, effect.itv.end),
      equal(cond.itv.end, effect.itv.start)
    )
    val act = Action(itv, Seq(cond, effect), constraints)
    Opt.optional(act)
  }

  val pb = Problem(initTokens, goals, actions)

  val x = dahu.recursion.Algebras.pprint(Algebras.coalgebra, pb.satProblem)
  println(x)
  import matryoshka._
  import matryoshka.implicits._
  import dahu.recursion.Algebras._
  val rec = Recursive.fromCoalgebra(attributeCoalgebra(Algebras.coalgebra))
  val pprint = dahu.recursion.Algebras.printAlgebra
  def printCoffre : Algebra[EnvT[Ast, ExprF, ?], String] = {
    case EnvT((ast, estring)) => (ast.toString, pprint(estring)).toString()
  }
  def collectCofree: Algebra[EnvT[Ast, ExprF, ?], List[(Ast, String)]] = {
    case EnvT((ast, x@ProductF(members, _))) => (ast, "                      "+x.toString) :: members.toList.flatten
    case EnvT((ast, x@ComputationF(f, members, _))) => (ast, f.toString) :: members.toList.flatten
    case EnvT((ast, x)) => List((ast, x.toString))
  }

  val withAtt = attributeCoalgebra(Algebras.coalgebra)
  val y: Cofree[ExprF, Ast] = pb.satProblem.asInstanceOf[Ast].ana[Cofree[ExprF, Ast]].apply(withAtt)
  println(y)

  println(y.cata(printCoffre))
  println(y.cata(collectCofree).mkString("\n"))


//  println(y.cata(alg))
//  pb.satProblem.asInstanceOf[Ast].hylo(alg, withAtt)
//  println(store.mkString("\n"))

//  println(astStore.map(t => t._1.toString+"\n  "+t._2.mkString("\n  ")).mkString("\n"))
  val tmp = transpile(pb.satProblem, Algebras.coalgebra)
  val tmp2 = transpile(tmp.root, tmp.coalgebra.asScalaFunction)
  val f: Ast => Option[tmp2.EId] = x => tmp.compiledForm(x).flatMap(y => tmp2.compiledForm(y))
  println(tmp)

  val csp = new CSP(tmp)
  csp.solve

  println(tmp2)

  {
    val p: Ast = pb.satProblem
    val tree = p.ana[Fix[ExprF]].apply(Algebras.coalgebra)
    val flattened = tree.transCata[Fix[ExprF]].apply(dahu.recursion.Algebras.passes).cata(pprint)
//    val flattened2 = tree.prepro(dahu.recursion.Algebras.passes, pprint)

    def withSize(s: String) = s"${s.length}: $s"
    println(withSize(tree.cata(pprint)))
    println(withSize(flattened))
  }
}
