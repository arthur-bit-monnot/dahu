package dahu.planning.pddl.parser.ast

import dahu.planning.model.common.{operators, Cst, IntLiteral, Predef}
import dahu.planning.model.full.{BinaryExprTree, StaticExpr, UnaryExprTree}
import dahu.planning.pddl.parser.{PddlPredef, Resolver}
import fr.uga.pddl4j.parser._
import dahu.utils.errors._

import scala.collection.JavaConverters._

object AssertionOnFunction {
  def unapply(e: Exp): Option[String] = e match {
    case Fluent(name, _)                => Some(name)
    case Eq(Fluent(name, _), _)         => Some(name)
    case Not(AssertionOnFunction(name)) => Some(name)
    case _                              => None
  }
}

object BooleanExpr {
  def unapply(e: Exp)(implicit ctx: Resolver, predef: Predef): Option[StaticExpr] = e match {
    case Fluent(name, _)                    => None
    case EqAtom(Variable(v1), Variable(v2)) => Some(BinaryExprTree(operators.Eq, v1, v2))
    case Not(BooleanExpr(v))                => Some(UnaryExprTree(operators.Not, v))
    case _                                  => None
  }
}

object Variable {
  def unapply(e: Symbol)(implicit ctx: Resolver): Option[StaticExpr] = e match {
    case _ => Some(ctx.variable(e.getImage))
  }
}

object Fluent {
  def unapply(exp: Exp): Option[(String, List[String])] = {
    if(exp.getConnective == Connective.ATOM || exp.getConnective == Connective.FN_HEAD) {
      exp.getAtom.asScala.toList.map(_.getImage) match {
        case head :: tail => Some((head, tail))
        case _            => None
      }
    } else if(exp.getConnective == Connective.F_EXP) {
      exp.getChildren.asScala match {
        case Seq(e) => Fluent.unapply(e)
        case _      => unexpected
      }
    } else {
      None
    }
  }
}

object Cst {
  def unapply(e: Exp)(implicit predef: PddlPredef): Option[Cst] = {
    if(e.getConnective == Connective.NUMBER)
      Some(IntLiteral(predef.discretize(e.getValue)))
    else
      None
  }
}

object Eq {
  def unapply(e: Exp): Option[(Exp, Exp)] = {
    if(e.getConnective == Connective.FN_ATOM || e.getConnective == Connective.EQUAL) {
      e.getChildren.asScala.toList match {
        case lhs :: rhs :: Nil => Some((lhs, rhs))
        case _                 => unexpected
      }
    } else {
      None
    }
  }
}

object EqAtom {
  def unapply(e: Exp): Option[(Symbol, Symbol)] = {
    if(e.getConnective == Connective.EQUAL_ATOM) {
      e.getAtom.asScala.toList match {
        case lhs :: rhs :: Nil => Some((lhs, rhs))
        case _                 => unexpected
      }
    } else {
      None
    }
  }
}

object Not {
  def unapply(e: Exp): Option[Exp] = {
    if(e.getConnective == Connective.NOT) {
      e.getChildren.asScala match {
        case Seq(neg) => Some(neg)
        case _        => unexpected
      }
    } else {
      None
    }
  }
}

case class Tpe(name: String, parent: Option[String])

object ReadTpe {
  def unapply(e: TypedSymbol): Option[Tpe] =
    if(e.getKind.name() == "TYPE") {
      e.getTypes.asScala.map(_.getImage).toList match {
        case Nil           => Some(Tpe(e.getImage, None))
        case parent :: Nil => Some(Tpe(e.getImage, Some(parent)))
        case _             => None
      }
    } else
      None
}

object And {
  def unapply(e: Exp): Option[List[Exp]] = {
    if(e.getConnective == Connective.AND) {
      Some(e.getChildren.asScala.toList)
    } else {
      None
    }
  }
}

object TypedSymbol {
  def unapply(e: TypedSymbol): Option[(String, String)] = {
    if(e.getKind == Symbol.Kind.VARIABLE || e.getKind == Symbol.Kind.CONSTANT) {
      val name = e.getImage
//        if(e.getImage.startsWith("?"))
//          e.getImage.drop(1)
//        else
//          e.getImage
      e.getTypes.asScala.toList match {
        case tpe :: Nil => Some((name, tpe.getImage))
        case _          => None
      }
    } else {
      None
    }
  }
}

object Duration {
  def unapply(e: Exp): Option[Unit] = {
    if(e.getConnective == Connective.TIME_VAR && e.getVariable.getImage == "?duration")
      Some(())
    else
      None
  }
}

object AtStart {
  def unapply(e: Exp): Option[Exp] = {
    if(e.getConnective == Connective.AT_START) {
      e.getChildren.asScala match {
        case Seq(sub) => Some(sub)
        case _        => unexpected
      }
    } else {
      None
    }
  }
}

object AtEnd {
  def unapply(e: Exp): Option[Exp] = {
    if(e.getConnective == Connective.AT_END) {
      e.getChildren.asScala match {
        case Seq(sub) => Some(sub)
        case _        => unexpected
      }
    } else {
      None
    }
  }
}

object OverAll {
  def unapply(e: Exp): Option[Exp] = {
    if(e.getConnective == Connective.OVER_ALL) {
      e.getChildren.asScala match {
        case Seq(sub) => Some(sub)
        case _        => unexpected
      }
    } else {
      None
    }
  }
}
