package copla.lang.model

import copla.lang.model.common._

package object core {

  type CoreModel = Seq[InModuleBlock]

  sealed trait Block
  sealed trait InModuleBlock
  sealed trait InActionBlock
  sealed trait Statement extends InModuleBlock with InActionBlock

  sealed trait Declaration[T] {
    def id: Id
  }
  final case class TimepointDeclaration(tp: Timepoint)
      extends Declaration[Timepoint]
      with Statement {
    override def id: Id = tp.id
    override def toString: String = s"timepoint $id"
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
    def params: Seq[Term]
    def typ: Type = template.typ
  }

  case class Constant(override val template: ConstantTemplate, override val params: Seq[Term])
      extends Function {
    require(template.params.size == params.size)
    override def toString: String = super.toString
  }
  class BoundConstant(override val template: ConstantTemplate, override val params: Seq[Cst])
      extends Constant(template, params)

  final case class Fluent(override val template: FluentTemplate, override val params: Seq[Term])
      extends Function {
    require(template.params.size == params.size)
    template.params.zip(params).foreach {
      case (tpl, v) =>
        require(v.typ.isSubtypeOf(tpl.typ), s"$v is not of type ${tpl.typ}")
    }
    override def toString = s"$template(${params.mkString(", ")})"
  }

  sealed trait StaticAssertion extends Statement
  final case class StaticEqualAssertion(left: Term, right: Term) extends StaticAssertion {
    override def toString: String = s"$left == $right"
  }
  final case class StaticDifferentAssertion(left: Term, right: Term) extends StaticAssertion {
    override def toString: String = s"$left != $right"
  }
  final case class StaticAssignmentAssertion(left: BoundConstant, right: Cst)
      extends StaticAssertion {
    override def toString: String = s"$left := $right"
  }
  final case class BindAssertion(constant: Constant, variable: LocalVar) extends StaticAssertion {
    override def toString: String = s"$constant == $variable"
  }

  sealed trait TimedAssertion extends Statement {
    def start: TPRef
    def end: TPRef
    def fluent: Fluent
  }

  /** Denotes an assertion that requires causal support */
  sealed trait RequiresSupport { self: TimedAssertion =>
  }

  /** Denotes an assertion that changes a fluent */
  sealed trait ProvidesChange { self: TimedAssertion =>
    def valueAfterChange: Term
  }

  final case class TimedEqualAssertion(start: TPRef, end: TPRef, fluent: Fluent, value: Term)
      extends TimedAssertion
      with RequiresSupport {
    override def toString: String = s"[$start, $end] $fluent == $value"
  }
  final case class TimedAssignmentAssertion(start: TPRef, end: TPRef, fluent: Fluent, value: Term)
      extends TimedAssertion
      with ProvidesChange {
    override def valueAfterChange: Term = value
    override def toString: String = s"[$start,$end] $fluent := $value"
  }
  final case class TimedTransitionAssertion(start: TPRef,
                                            end: TPRef,
                                            fluent: Fluent,
                                            from: Term,
                                            to: Term)
      extends TimedAssertion
      with RequiresSupport
      with ProvidesChange {
    override def valueAfterChange: Term = to
    override def toString: String = s"[$start, $end] $fluent == $from :-> $to"
  }

  sealed trait IntExpr
  object IntExpr {
    def apply(lit: Int): IntExpr = VarIntExpr(IntLiteral(lit))
  }
  final case class VarIntExpr(e: Term) extends IntExpr {
    require(e.typ == Type.Integers)

    override def toString: String = e.toString
  }
  final case class Minus(e: IntExpr) extends IntExpr
  final case class Add(lhs: IntExpr, rhs: IntExpr) extends IntExpr

  /** A timepoint, declared when appearing in the root of a context.*/
  final case class TPRef(id: Timepoint, delay: IntExpr = IntExpr(0)) {

    override def toString: String =
      id.toString + (delay match {
        case VarIntExpr(IntLiteral(d)) =>
          if(d == 0) ""
          else if(d > 0) s"+$d"
          else s"-${-d}"
        case x => s"+$x"
      })

    def +(d: IntExpr): TPRef = TPRef(id, Add(delay, d))
    def +(d: Int): TPRef = this + IntExpr(d)
    def -(d: IntExpr): TPRef = TPRef(id, Add(delay, Minus(d)))
    def -(d: Int): TPRef = this - IntExpr(d)

    def <=(other: TPRef): TBefore = TBefore(this, other)
    def <(other: TPRef): TBefore = TBefore(this, other - 1)
    def >=(other: TPRef): TBefore = other <= this
    def >(other: TPRef): TBefore = other < this
    def ===(other: TPRef): Seq[TBefore] = Seq(this <= other, this >= other)
  }

  case class TBefore(from: TPRef, to: TPRef) extends Statement {
    override def toString: String = s"$from <= $to"
  }

  final case class ActionTemplate(scope: InnerScope, content: Seq[InActionBlock])
      extends InModuleBlock {
    def name: String = scope.name
    lazy val args: Seq[Arg] = content.collect { case ArgDeclaration(a) => a }

    override def toString: String =
      s"action $name(${args.map(a => s"${a.typ} ${a.id.name}").mkString(", ")})"

    lazy val start: TPRef = content
      .collectFirst {
        case TimepointDeclaration(tp) if tp.id == Id(scope, "start") => TPRef(tp)
      }
      .getOrElse(sys.error("No start timepoint in this action"))

    lazy val end: TPRef = content
      .collectFirst {
        case TimepointDeclaration(tp) if tp.id == Id(scope, "end") => TPRef(tp)
      }
      .getOrElse(sys.error("No end timepoint in this action"))

  }

  /** Instance of an action template */
  final case class Action(scope: InnerScope,
                          content: Seq[InActionBlock],
                          template: ActionTemplate) {
    def name: String = scope.name
    val args: Seq[Arg] = content.collect { case ArgDeclaration(a) => a }

    lazy val start: TPRef = content
      .collectFirst {
        case TimepointDeclaration(tp) if tp.id == Id(scope, "start") => TPRef(tp)
      }
      .getOrElse(sys.error("No start timepoint in this action"))

    lazy val end: TPRef = content
      .collectFirst {
        case TimepointDeclaration(tp) if tp.id == Id(scope, "end") => TPRef(tp)
      }
      .getOrElse(sys.error("No end timepoint in this action"))
  }
}
