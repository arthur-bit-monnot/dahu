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
    def andOpt(conjuncts: Fix[TotalOrOptionalF]*): Fix[TotalOrOptionalF] = {
      assert(conjuncts.forall(c => c.unfix.typ == Tag.ofBoolean))
      val nonEmptyConjuncts = conjuncts.filter {
        case ComputationF(bool.And, Seq(), _) => false
        case CstF(true, _)                    => false
        case _                                => true
      }
      ComputationF(bool.And, nonEmptyConjuncts, Tag.ofBoolean)
    }
    def notOpt(e: Fix[TotalOrOptionalF]): Fix[TotalOrOptionalF] = {
      assert(e.unfix.typ == Tag.ofBoolean)
      ComputationF(bool.Not, Seq(e), Tag.ofBoolean)
    }
    def impliesOpt(cond: Fix[TotalOrOptionalF],
                   eff: Fix[TotalOrOptionalF]): Fix[TotalOrOptionalF] = {
      assert(cond.unfix.typ == Tag.ofBoolean && eff.unfix.typ == Tag.ofBoolean)
      val notCond = Fix(ComputationF(bool.Not, Seq(cond), Tag.ofBoolean))
      ComputationF(bool.Or, Seq(notCond, eff), Tag.ofBoolean)
    }
//    def andPart(conjuncts: XXX*): XXX = {
//      assert(conjuncts.forall(c => c.typ == Tag.ofBoolean))
//      ComputationF(bool.And, conjuncts, Tag.ofBoolean)
//    }
//    def notPart(e: Fix[TotalOrPartialF]): Fix[TotalOrPartialF] = {
//      assert(e.unfix.typ == Tag.ofBoolean)
//      ComputationF(bool.Not, Seq(e), Tag.ofBoolean)
//    }
//    def impliesPart(cond: Fix[TotalOrPartialF],
//                   eff: Fix[TotalOrPartialF]): Fix[TotalOrPartialF] = {
//      assert(cond.unfix.typ == Tag.ofBoolean && eff.unfix.typ == Tag.ofBoolean)
//      val notCond = Fix(ComputationF(bool.Not, Seq(cond), Tag.ofBoolean))
//      ComputationF(bool.Or, Seq(notCond, eff), Tag.ofBoolean)
//    }
  }
  import Utils._
//  type XXX = OptionalF[Fix[Total]]
  type PB1 = Partial[Fix[TotalOrOptionalF]]

  val ALGOpt: FAlgebra[ExprF, PB1] = {
    case Partial(value, condition, tpe) =>
      Partial(value.value, andOpt(value.condition, condition.condition, condition.value), tpe)
    case x: InputF[PB1] => Partial(Fix(x), andOpt(), x.typ)
    case x: CstF[PB1]   => Partial(Fix(x), andOpt(), x.typ)
    case ComputationF(f, args, t) =>
      Partial(ComputationF(f, args.map(a => a.value), t), andOpt(args.map(_.condition): _*), t)
    case ProductF(members, t) =>
      Partial(ProductF(members.map(a => a.value), t), andOpt(members.map(_.condition): _*), t)
    case OptionalF(value, present, t) =>
      Partial(
        OptionalF(value.value, present.value, t),
        andOpt(present.condition, impliesOpt(present.value, value.condition)),
        t
      )
    case ITEF(cond, onTrue, onFalse, t) =>
      Partial(
        ITEF(cond.value, onTrue.value, onFalse.value, t),
        andOpt(cond.condition,
               impliesOpt(cond.value, onTrue.condition),
               impliesOpt(notOpt(cond.condition), onFalse.condition)),
        t
      )
    case PresentF(part) =>
      Partial(
        PresentF(part.value),
        andOpt(), // always present
        part.typ
      )

    case ValidF(part) =>
      Partial(
        ValidF(andOpt(part.condition, ValidF(part.condition), ValidF(part.value))),
        andOpt(), // always present
        part.typ
      )
  }
  val ALG2: FAlgebra[TotalOrOptionalF, OptionalF[Fix[Total]]] = {
    case x: InputF[_] => OptionalF(Fix(x), and(), x.typ)
    case x: CstF[_]   => OptionalF(Fix(x), and(), x.typ)
    case ComputationF(f, args, t) =>
      OptionalF(ComputationF(f, args.map(a => a.value), t), and(args.map(_.present): _*), t)
    case ProductF(members, t) =>
      OptionalF(ProductF(members.map(a => a.value), t), and(members.map(_.present): _*), t)
    case ITEF(cond, onTrue, onFalse, t) =>
      OptionalF(
        ITEF(cond.value, onTrue.value, onFalse.value, t),
        and(cond.present,
            implies(cond.value, onTrue.present),
            implies(cond.value, onFalse.present)),
        t
      )
    case OptionalF(value, present, t) =>
      OptionalF(
        value.value,
        and(implies(present.present, present.value), value.present),
        t
      )
    case PresentF(opt) =>
      OptionalF(
        and(), //PresentF(and(opt.present, PresentF(opt.value))),
        and(), // always true
        opt.typ
      )

    case ValidF(opt) =>
      OptionalF(
        ValidF(and(ValidF(opt.present), ValidF(opt.value))), // this should always be true given that opt.{present, valid} are total
        and(),
        opt.typ
      )
  }

  case class IR(value: Fix[Total], present: Fix[Total] = and(), valid: Fix[Total] = and())
  val ALGPART: FAlgebra[ExprF, IR] = x => {
    val IR(value1, present1, valid1) = x match {
      case x: InputF[_] => IR(Fix(x))
      case x: CstF[_]   => IR(Fix(x))
      case ComputationF(f, args, t) =>
        val ret = IR(
          value = ComputationF(f, args.map(a => a.value), t),
          present = and(args.map(_.present): _*),
          valid = and(args.map(_.valid): _*)
        )
        println(ret)
        ret
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
        val x = IR(
          value = value.value,
          present = value.present,
          valid =
            and(value.valid, implies(condition.present, and(condition.value, condition.valid)))
        )
        x
    }
    val opt = IR(optimize(value1), optimize(present1), optimize(valid1))
    val noopt = IR(value1, present1, valid1)
//    println(noopt)
    println(opt)
    opt
  }

  def optimize(tot: Fix[Total]): Fix[Total] =
    dahu.recursion.Recursion
      .cata[Total, Fix[Total]](dahu.model.compiler.Optimizations.simplificationAlgebra)(tot)

  case class Pb(value: Fix[TotalOrOptionalF], condition: Fix[Total])
  def encode[X](root: X, coalgebra: FCoalgebra[ExprF, X], optimize: Boolean = true): Fix[Total] = {

    val IR(value, present, valid) = Recursion.hylo(coalgebra, ALGPART)(root)

    println("present: " + optim(present))
    println("valid  : " + optim(valid))

    def optim(tot: Fix[Total]): Fix[Total] =
      dahu.recursion.Recursion
        .cata[Total, Fix[Total]](dahu.model.compiler.Optimizations.simplificationAlgebra)(tot)

    val condition: Fix[Total] =
      if(optimize)
        optim(valid)
      else
        valid
    println(condition)
    println(dahu.recursion.Recursion.cata(Algebras.printAlgebraMultiLine)(condition))
//    println(dahu.recursion.Recursion.cata(Algebras.printAlgebraMultiLine)(cond))
//    pb.copy(condition = condition)
    condition
  }

}
