package dahu.model.problem

import dahu.maps.ArrayMap
import dahu.model.ir._
import dahu.model.math.bool
import dahu.model.types._
import dahu.recursion._

import scala.collection.mutable

object SatisfactionProblem {

  def satisfactionSubAST(ast: AST[_]): TotalSubAST[ast.ID] = {
    val Partial(_, condition, _) = encode(ast.root, ast.tree.asFunction)

    val memory = mutable.LinkedHashMap[Total[Int], Int]()

    val alg: Total[Int] => Int = env => {
      memory.getOrElseUpdate(env, memory.size)
    }
    val treeRoot = Recursion.cata[Total, Int](alg)(condition)
    val reversedMemory = memory.map(_.swap).toMap
    val genTree = ArrayMap.build(reversedMemory.keys, k => reversedMemory(k))

    new TotalSubAST[ast.ID] {
      override def tree: ArrayMap.Aux[ID, Total[ID]] =
        genTree.asInstanceOf[ArrayMap.Aux[ID, Total[ID]]]

      override def root: ID = treeRoot.asInstanceOf[ID]

      override val subset: TotalSubAST.SubSet[ast.ID, ID] = new TotalSubAST.SubSet[ast.ID, ID] {
        override def from: ID => Option[ast.ID] = x => {
          val e = tree(x)
          ast.reverseTree.get(e.asInstanceOf[ast.Expr])
        }
        override def to: ast.ID => Option[ID] = x => {
          val e: ExprF[ast.ID] = ast.tree(x)
          e match {
            case t: Total[_] => reverseTree.get(t.asInstanceOf[Total[ID]])
            case _           => None
          }
        }

      }
    }
  }

  type PB = Partial[Fix[Total]]

  private object Utils {
    import scala.language.implicitConversions
    implicit def autoFix[F[_]](x: F[Fix[F]]): Fix[F] = Fix(x)

    def and(conjuncts: Fix[Total]*): Fix[Total] = {
      assert(conjuncts.forall(c => c.unfix.typ == Tag.ofBoolean))
      ComputationF(bool.And, conjuncts.toSeq, Tag.ofBoolean)
    }
    def implies(cond: Fix[Total], eff: Fix[Total]): Fix[Total] = {
      assert(cond.unfix.typ == Tag.ofBoolean && eff.unfix.typ == Tag.ofBoolean)
      val notCond = Fix(ComputationF(bool.Not, Seq(cond), Tag.ofBoolean))
      ComputationF(bool.Or, Seq(notCond, eff), Tag.ofBoolean)
    }
  }
  import Utils._

  val ALG: FAlgebra[ExprF, PB] = {
    case Partial(value, condition, tpe) =>
      Partial(value.value, and(value.condition, condition.condition, condition.value), tpe)
    case x: InputF[PB] => Partial(Fix(x), and(), x.typ)
    case x: CstF[PB]   => Partial(Fix(x), and(), x.typ)
    case ComputationF(f, args, t) =>
      Partial(ComputationF(f, args.map(a => a.value), t), and(args.map(_.condition): _*), t)
    case ProductF(members, t) =>
      Partial(ProductF(members.map(a => a.value), t), and(members.map(_.condition): _*), t)
    case OptionalF(value, present, t) =>
      Partial(
        OptionalF(value.value, present.value, t),
        and(present.condition, implies(present.value, value.condition)),
        t
      )
  }

  def encode[X](root: X, coalgebra: FCoalgebra[ExprF, X], optimize: Boolean = true): PB = {
    val pb = Recursion.hylo(coalgebra, ALG)(root)
    val condition =
      if(optimize)
        dahu.recursion.Recursion.cata[Total, Fix[Total]](
          dahu.model.compiler.Optimizations.simplificationAlgebra)(pb.condition)
      else
        pb.condition

    pb.copy(condition = condition)
  }

}
