package dahu.planning.model.transforms

import dahu.planning.model.common._
import dahu.planning.model.core._

object ActionInstantiation {

  private[this] object implicits {

    trait IdRewrite[T] {

      /** Transforms a type T, recursively replacing any id: Id in it with f(id) */
      def map(a: T, f: Id => Id)(implicit predef: Predef): T
    }
    // provides syntax for boilerplate
    implicit class Rewrite[T](private val lhs: T) extends AnyVal {
      def rw(f: Id => Id)(implicit rewrite: IdRewrite[T], predef: Predef): T = rewrite.map(lhs, f)
    }

    implicit object ofCst extends IdRewrite[Cst] {
      override def map(a: Cst, f: Id => Id)(implicit predef: Predef): Cst = a match {
        case i: IntLiteral     => i
        case Instance(id, tpe) => Instance(f(id), tpe)
      }
    }

    implicit object ofExpr extends IdRewrite[Expr] {
      override def map(a: Expr, f: Id => Id)(implicit predef: Predef): Expr = a match {
        case x: Cst            => x.rw(f)
        case Arg(id, tpe)      => Arg(f(id), tpe)
        case LocalVar(id, tpe) => LocalVar(f(id), tpe)
        case Op1(op, e)        => Op1(op, map(e, f))
        case Op2(op, lhs, rhs) => Op2(op, map(lhs, f), map(rhs, f))
        case Constant(template, params) =>
          Constant(ofConstantTemplate.map(template, f), params.map(x => map(x, f)))
      }
    }
    implicit object ofInterval extends IdRewrite[Interval[Expr]] {
      override def map(a: Interval[Expr], f: Id => Id)(implicit predef: Predef): Interval[Expr] =
        a match {
          case ClosedInterval(l, r)    => ClosedInterval(l.rw(f), r.rw(f))
          case LeftOpenInterval(l, r)  => LeftOpenInterval(l.rw(f), r.rw(f))
          case RightOpenInterval(l, r) => RightOpenInterval(l.rw(f), r.rw(f))
          case OpenInterval(l, r)      => OpenInterval(l.rw(f), r.rw(f))
        }
    }

    implicit object ofConstantTemplate extends IdRewrite[ConstantTemplate] {
      override def map(a: ConstantTemplate, f: Id => Id)(
          implicit predef: Predef): ConstantTemplate = a match {
        case ConstantTemplate(id, tpe, params) =>
          ConstantTemplate(f(id), tpe, params.map { case Arg(id, tpe) => Arg(f(id), tpe) })
      }
    }
    implicit object ofFluentTemplate extends IdRewrite[FluentTemplate] {
      override def map(a: FluentTemplate, f: Id => Id)(implicit predef: Predef): FluentTemplate =
        a match {
          case FluentTemplate(id, tpe, params, isContinuous) =>
            FluentTemplate(f(id),
                           tpe,
                           params.map { case Arg(id, tpe) => Arg(f(id), tpe) },
                           isContinuous)
        }
    }
    implicit object ofFluent extends IdRewrite[Fluent] {
      override def map(a: Fluent, f: Id => Id)(implicit predef: Predef): Fluent = a match {
        case Fluent(template, params) => Fluent(template.rw(f), params.map(_.rw(f)))
      }
    }

    implicit object ofBlock extends IdRewrite[InActionBlock] {
      override def map(a: InActionBlock, f: Id => Id)(implicit predef: Predef): InActionBlock =
        a match {
          case StaticBooleanAssertion(e)              => StaticBooleanAssertion(e.rw(f))
          case ArgDeclaration(Arg(id, tpe))           => ArgDeclaration(Arg(f(id), tpe))
          case LocalVarDeclaration(LocalVar(id, tpe)) => LocalVarDeclaration(LocalVar(f(id), tpe))
          case StaticAssignmentAssertion(x: BoundConstant, cst) =>
            StaticAssignmentAssertion(new BoundConstant(x.template, x.params.map(_.rw(f))),
                                      cst.rw(f))
          case TimedAssignmentAssertion(itv, fl, value) =>
            TimedAssignmentAssertion(itv.rw(f), fl.rw(f), value.rw(f))
          case TimedBooleanAssertion(itv, fl, op, value) =>
            TimedBooleanAssertion(itv.rw(f), fl.rw(f), op, value.rw(f))
          case TimedTransitionAssertion(itv, fl, from, to) =>
            TimedTransitionAssertion(itv.rw(f), fl.rw(f), from.rw(f), to.rw(f))
        }
    }
  }

  /** Builds a new action instance with the given name*/
  def instance(template: ActionTemplate, instanceName: String)(implicit predef: Predef): Action = {
    val instanceScope: InnerScope = template.scope.parent + instanceName

    val trans: Id => Id = x => {
      var transScope: Scope => Scope = null
      transScope = {
        case s if s == template.scope => template.scope.parent + instanceName
        case RootScope                => RootScope
        case InnerScope(s, name)      => transScope(s) + name
      }
      x match {
        case Id(s, name)  => Id(transScope(s), name)
        case x: Anonymous => new Anonymous(transScope(x.scope))
      }
    }

    val instanceContent = template.content.map(s => implicits.ofBlock.map(s, trans))
    Action(instanceScope, instanceContent, template)
  }
}
