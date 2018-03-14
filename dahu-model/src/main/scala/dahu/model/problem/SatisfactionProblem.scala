package dahu.model.problem

import dahu.maps.ArrayMap
import dahu.model.compiler.Algebras
import dahu.model.ir._
import dahu.model.math.bool
import dahu.model.types._
import dahu.recursion._

import scala.collection.mutable

object SatisfactionProblem {

  def satisfactionSubAST(ast: AST[_]): TotalSubAST[ast.ID] = {
    val condition = encode(ast.root, ast.tree.asFunction)

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
      val nonEmptyConjuncts = conjuncts.filter {
        case ComputationF(bool.And, Seq(), _) => false
        case CstF(true, _)                    => false
        case _                                => true
      }
      ComputationF(bool.And, nonEmptyConjuncts, Tag.ofBoolean)
    }
    def not(e: Fix[Total]): Fix[Total] = {
      assert(e.unfix.typ == Tag.ofBoolean)
      ComputationF(bool.Not, Seq(e), Tag.ofBoolean)
    }
    def implies(cond: Fix[Total], eff: Fix[Total]): Fix[Total] = {
      assert(cond.unfix.typ == Tag.ofBoolean && eff.unfix.typ == Tag.ofBoolean)
      val notCond = Fix(ComputationF(bool.Not, Seq(cond), Tag.ofBoolean))
      ComputationF(bool.Or, Seq(notCond, eff), Tag.ofBoolean)
    }
  }
  import Utils._

  case class IR(value: Fix[Total], present: Fix[Total], valid: Fix[Total])
  def compiler(optimize: Boolean): FAlgebra[ExprF, IR] = x => {
    val IR(value1, present1, valid1) = x match {
      case x: InputF[_] => IR(Fix(x), and(), and())
      case x: CstF[_]   => IR(Fix(x), and(), and())
      case ComputationF(f, args, t) =>
        IR(
          value = ComputationF(f, args.map(a => a.value), t),
          present = and(args.map(_.present): _*),
          valid = and(args.map(_.valid): _*)
        )
      case ProductF(members, t) =>
        IR(
          value = ProductF(members.map(a => a.value), t),
          present = and(members.map(_.present): _*),
          valid = and(members.map(_.valid): _*)
        )
      case ITEF(cond, onTrue, onFalse, t) =>
        IR(
          value = ITEF(cond.value, onTrue.value, onFalse.value, t),
          present = and(cond.present,
                        implies(cond.value, onTrue.present),
                        implies(not(cond.value), onFalse.present)),
          valid = and(cond.valid,
                      implies(cond.value, onTrue.valid),
                      implies(not(cond.value), onFalse.valid))
        )
      case OptionalF(value, present, _) =>
        IR(
          value = value.value,
          present = and(value.present, present.present, present.value),
          valid = and(present.valid, value.valid)
        )
      case PresentF(opt) =>
        IR(
          value = opt.present,
          present = and(),
          valid = opt.valid
        )
      case ValidF(part) =>
        IR(
          value = part.valid,
          present = part.present,
          valid = and()
        )
      case Partial(value, condition, tpe) =>
        IR(
          value = value.value,
          present = value.present,
          valid =
            and(value.valid, implies(condition.present, and(condition.value, condition.valid)))
        )
    }
    if(optimize)
      IR(optimizer(value1), optimizer(present1), optimizer(valid1))
    else
      IR(value1, present1, valid1)
  }

  def optimizer(tot: Fix[Total]): Fix[Total] =
    dahu.recursion.Recursion
      .cata[Total, Fix[Total]](dahu.model.compiler.Optimizations.simplificationAlgebra)(tot)

  def encode[X](root: X, coalgebra: FCoalgebra[ExprF, X], optimize: Boolean = true): Fix[Total] = {

    val ir = Recursion.hylo(coalgebra, compiler(optimize = optimize))(root)

    ir.valid
  }

}
