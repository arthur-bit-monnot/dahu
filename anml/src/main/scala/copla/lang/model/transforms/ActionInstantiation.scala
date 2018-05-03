package copla.lang.model.transforms

import copla.lang.model.common._
import copla.lang.model.core._

object ActionInstantiation {

  private[this] object implicits {

    trait IdRewrite[T] {

      /** Transforms a type T, recursively replacing any id: Id in it with f(id) */
      def map(a: T, f: Id => Id): T
    }
    // provides syntax for boilerplate
    implicit class Rewrite[T](private val lhs: T) extends AnyVal {
      def rw(f: Id => Id)(implicit rewrite: IdRewrite[T]): T = rewrite.map(lhs, f)
    }

    implicit object ofCst extends IdRewrite[Cst] {
      override def map(a: Cst, f: Id => Id): Cst = a match {
        case i: IntLiteral     => i
        case Instance(id, tpe) => Instance(f(id), tpe)
      }
    }

    implicit object ofExpr extends IdRewrite[Expr] {
      override def map(a: Expr, f: Id => Id): Expr = a match {
        case x: Cst            => x.rw(f)
        case Arg(id, tpe)      => Arg(f(id), tpe)
        case LocalVar(id, tpe) => LocalVar(f(id), tpe)
        case Op1(op, e)        => Op1(op, map(e, f))
        case Op2(op, lhs, rhs) => Op2(op, map(lhs, f), map(rhs, f))
      }
    }

    implicit object ofConstantTemplate extends IdRewrite[ConstantTemplate] {
      override def map(a: ConstantTemplate, f: Id => Id): ConstantTemplate = a match {
        case ConstantTemplate(id, tpe, params) =>
          ConstantTemplate(f(id), tpe, params.map { case Arg(id, tpe) => Arg(f(id), tpe) })
      }
    }
    implicit object ofFluentTemplate extends IdRewrite[FluentTemplate] {
      override def map(a: FluentTemplate, f: Id => Id): FluentTemplate = a match {
        case FluentTemplate(id, tpe, params) =>
          FluentTemplate(f(id), tpe, params.map { case Arg(id, tpe) => Arg(f(id), tpe) })
      }
    }
    implicit object ofFluent extends IdRewrite[Fluent] {
      override def map(a: Fluent, f: Id => Id): Fluent = a match {
        case Fluent(template, params) => Fluent(template.rw(f), params.map(_.rw(f)))
      }
    }

    implicit object ofBlock extends IdRewrite[InActionBlock] {
      override def map(a: InActionBlock, f: Id => Id): InActionBlock = a match {
        case StaticBooleanAssertion(e)              => StaticBooleanAssertion(e.rw(f))
        case ArgDeclaration(Arg(id, tpe))           => ArgDeclaration(Arg(f(id), tpe))
        case LocalVarDeclaration(LocalVar(id, tpe)) => LocalVarDeclaration(LocalVar(f(id), tpe))
        case StaticAssignmentAssertion(x: BoundConstant, cst) =>
          StaticAssignmentAssertion(new BoundConstant(x.template, x.params.map(_.rw(f))), cst.rw(f))
        case TimedAssignmentAssertion(st, ed, fl, value) =>
          TimedAssignmentAssertion(st.rw(f), ed.rw(f), fl.rw(f), value.rw(f))
        case TimedEqualAssertion(st, ed, fl, value) =>
          TimedEqualAssertion(st.rw(f), ed.rw(f), fl.rw(f), value.rw(f))
        case TimedTransitionAssertion(st, ed, fl, from, to) =>
          TimedTransitionAssertion(st.rw(f), ed.rw(f), fl.rw(f), from.rw(f), to.rw(f))
        case BindAssertion(Constant(template, params), LocalVar(id, tpe)) =>
          BindAssertion(Constant(template.rw(f), params.map(_.rw(f))), LocalVar(f(id), tpe))
      }
    }
  }

  /** Builds a new action instance with the given name*/
  def instance(template: ActionTemplate, instanceName: String): Action = {
    val instanceScope: InnerScope = template.scope.parent + instanceName

    val trans: Id => Id = x => {
      var transScope: Scope => Scope = null
      transScope = {
        case s if s == template.scope => template.scope.parent + instanceName
        case RootScope                => RootScope
        case InnerScope(s, name)      => transScope(s) + name
      }
      x match { case Id(s, name) => Id(transScope(s), name) }
    }

    val instanceContent = template.content.map(s => implicits.ofBlock.map(s, trans))
    Action(instanceScope, instanceContent, template)
  }
}
