package dahu.planning.model

import dahu.planning.model.common.{Id, _}

package object core {

  type CoreModel = Seq[InModuleBlock]

  sealed trait Block
  sealed trait InModuleBlock
  sealed trait InActionBlock
  sealed trait Statement extends InModuleBlock with InActionBlock

  sealed trait Declaration[T] {
    def id: Id
  }

  final case class TypeDeclaration(typ: Type) extends Declaration[Type] with InModuleBlock {
    override def id: Id = typ.id
    override def toString: String = s"type $id" + {
      if(typ.parent.isDefined) " < " + typ.parent.get else ""
    }
  }
  sealed trait VarDeclaration[V <: Term] extends Declaration[Term] {
    def variable: Term
    def id: Id = variable.id
  }
  final case class LocalVarDeclaration(variable: LocalVar)
      extends VarDeclaration[LocalVar]
      with Statement {
    override def toString: String = s"constant ${variable.typ} ${variable.id}"
  }
  final case class InstanceDeclaration(instance: Instance)
      extends VarDeclaration[Instance]
      with InModuleBlock {
    override def variable: Instance = instance
    override def toString: String = s"instance ${instance.typ} ${instance.id}"
  }
  final case class ArgDeclaration(arg: Arg) extends VarDeclaration[Arg] with InActionBlock {
    override def variable: Arg = arg
    override def toString: String = s"${arg.typ.id} ${arg.id}"
  }
  final case class FunctionDeclaration(func: FunctionTemplate)
      extends Declaration[FunctionTemplate]
      with InModuleBlock {
    override def id: Id = func.id
    override def toString: String = {
      val paramsString = "(" + func.params.map(p => s"${p.typ} ${p.id.name}").mkString(", ") + ")"
      func match {
        case _: FluentTemplate   => s"fluent ${func.typ} ${func.id}$paramsString"
        case _: ConstantTemplate => s"constant ${func.typ} ${func.id}$paramsString"
      }
    }
  }

  /** A block wrapping other blocks pertaining to the same scope. */
  sealed trait Wrapper extends Block {
    def wrapped: Seq[Block]
  }

  sealed trait Function {
    def template: FunctionTemplate
    def params: Seq[Expr]
    def typ: Type = template.typ
  }

  case class Constant(override val template: ConstantTemplate, override val params: Seq[Expr])
      extends Function {
    require(template.params.size == params.size)
    override def toString: String = s"$template(${params.mkString(", ")})"
  }
  class BoundConstant(override val template: ConstantTemplate, override val params: Seq[Cst])
      extends Constant(template, params)

  final case class Fluent(override val template: FluentTemplate, override val params: Seq[Expr])
      extends Function {
    require(template.params.size == params.size)
    template.params.zip(params).foreach {
      case (tpl, v) =>
        require(v.typ.isSubtypeOf(tpl.typ), s"$v is not of type ${tpl.typ}")
    }
    override def toString = s"$template(${params.mkString(", ")})"
  }

  sealed trait StaticAssertion extends Statement
  final case class StaticBooleanAssertion(e: Expr) extends StaticAssertion {
    require(e.typ.isBoolean)
    override def toString: String = s"assert($e)"
  }
  final case class StaticAssignmentAssertion(left: BoundConstant, right: Cst)
      extends StaticAssertion {
    override def toString: String = s"$left := $right"
  }
  final case class BindAssertion(constant: Constant, variable: LocalVar) extends StaticAssertion {
    override def toString: String = s"$constant == $variable"
  }

  sealed trait TimedAssertion extends Statement {
    def itv: Interval[Expr]
    def fluent: Fluent
  }

  /** Denotes an assertion that requires causal support */
  sealed trait RequiresSupport { self: TimedAssertion =>
  }

  /** Denotes an assertion that changes a fluent */
  sealed trait ProvidesChange { self: TimedAssertion =>
    def valueAfterChange: Expr
  }

  final case class TimedEqualAssertion(itv: Interval[Expr], fluent: Fluent, value: Expr)
      extends TimedAssertion
      with RequiresSupport {
    override def toString: String = s"$itv $fluent == $value"
  }
  final case class TimedAssignmentAssertion(itv: Interval[Expr], fluent: Fluent, value: Expr)
      extends TimedAssertion
      with ProvidesChange {
    override def valueAfterChange: Expr = value
    override def toString: String = s"$itv $fluent := $value"
  }
  final case class TimedTransitionAssertion(itv: Interval[Expr],
                                            fluent: Fluent,
                                            from: Expr,
                                            to: Expr)
      extends TimedAssertion
      with RequiresSupport
      with ProvidesChange {
    override def valueAfterChange: Expr = to
    override def toString: String = s"$itv $fluent == $from :-> $to"
  }

  final case class ActionTemplate(scope: InnerScope, content: Seq[InActionBlock])(
      implicit predef: Predef)
      extends InModuleBlock {
    def name: String = scope.name
    lazy val args: Seq[Arg] = content.collect { case ArgDeclaration(a) => a }

    override def toString: String =
      s"action $name(${args.map(a => s"${a.typ} ${a.id.name}").mkString(", ")})"

    lazy val start: LocalVar = content
      .collectFirst {
        case LocalVarDeclaration(tp @ LocalVar(Id(`scope`, "start"), predef.Time)) => tp
      }
      .getOrElse(sys.error("No start timepoint in this action"))

    lazy val end: LocalVar = content
      .collectFirst {
        case LocalVarDeclaration(tp @ LocalVar(Id(`scope`, "end"), predef.Time)) => tp
      }
      .getOrElse(sys.error("No end timepoint in this action"))
  }

  /** Instance of an action template */
  final case class Action(scope: InnerScope, content: Seq[InActionBlock], template: ActionTemplate)(
      implicit predef: Predef) {
    def name: String = scope.name
    val args: Seq[Arg] = content.collect { case ArgDeclaration(a) => a }

    lazy val start: LocalVar = content
      .collectFirst {
        case LocalVarDeclaration(tp @ LocalVar(Id(`scope`, predef.StartSym), predef.Time)) => tp
      }
      .getOrElse(sys.error("No start timepoint in this action"))

    lazy val end: LocalVar = content
      .collectFirst {
        case LocalVarDeclaration(tp @ LocalVar(Id(`scope`, predef.EndSym), predef.Time)) => tp
      }
      .getOrElse(sys.error("No end timepoint in this action"))
  }
}
