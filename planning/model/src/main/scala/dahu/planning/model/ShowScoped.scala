package dahu.planning.model

import dahu.planning.model.common._
import dahu.planning.model.core._

trait ShowScoped[-A] {

  def show(elem: A)(implicit scope: Scope): String

}

object ShowScoped {

  def apply[A](implicit instance: ShowScoped[A]): ShowScoped[A] = instance

  implicit object ofString extends ShowScoped[String] {
    override def show(elem: String)(implicit scope: Scope): String = elem
  }

  implicit object ofId extends ShowScoped[Id] {
    override def show(elem: Id)(implicit scope: Scope): String = elem match {
      case _: Anonymous                 => "_"
      case Named(s, name) if s == scope => name
      case x                            => x.toString // TODO: remove scope head
    }
  }
  implicit object ofType extends ShowScoped[Type] {
    override def show(elem: Type)(implicit scope: Scope): String = show"${elem.id}"
  }
  implicit object ofExpr extends ShowScoped[Expr] {
    override def show(elem: Expr)(implicit scope: Scope): String = elem match {
      case LocalVar(id, tpe) => show"$id"
      case Arg(id, _)        => show"$id"
      case IntLiteral(v)     => v.toString
      case Instance(id, _)   => show"$id"
      case Op1(op, e)        => show"${op.op}$e"
      case Op2(op, l, r)     => show"(${op.op} $l $r)"
    }
  }
  implicit object ofInterval extends ShowScoped[Interval[Expr]] {
    override def show(elem: Interval[Expr])(implicit scope: Scope): String = elem match {
      case ClosedInterval(s, e)    => show"[$s, $e]"
      case RightOpenInterval(s, e) => show"[$s, $e["
      case LeftOpenInterval(s, e)  => show"]$s, $e]"
      case OpenInterval(s, e)      => show"]$s, $e["
    }
  }
  implicit object ofFluent extends ShowScoped[Fluent] {
    override def show(elem: Fluent)(implicit scope: Scope): String = elem match {
      case Fluent(template, params) =>
        show"${template.id}(${params.map(p => show"$p").mkString(", ")})"
    }
  }
  implicit object ofStatement extends ShowScoped[Block] {
    override def show(elem: Block)(implicit scope: Scope): String = elem match {
      case a @ ActionTemplate(id, content) =>
        val sb = new StringBuilder()
        sb.append(a.toString + "\n")
        implicit val actScope = a.scope
        val nonImplicit = content.filter {
          case _: ArgDeclaration                                    => false
          case LocalVarDeclaration(v) if v == a.start || v == a.end => false
          case _                                                    => true
        }
        for(c <- nonImplicit) {
          sb.append("  " + show(c)(actScope) + "\n")
        }
        sb.toString()
      case TimedEqualAssertion(itv, f, v)         => show"$itv $f == $v"
      case TimedTransitionAssertion(itv, f, s, e) => show"$itv $f == $s :-> $e"
      case TimedAssignmentAssertion(itv, f, v)    => show"$itv $f := $v"
      case StaticBooleanAssertion(e)              => show"$e"
      case x                                      => x.toString //TODO
    }
  }

  implicit class Shown[+A](elem: A)(implicit printer: ShowScoped[A], scope: Scope) {
    override def toString = printer.show(elem)
  }

  implicit class ShownInterpolator(val sc: StringContext) extends AnyVal {
    def show(args: Shown[Any]*): String = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      var buf = new StringBuffer(strings.next)
      while(strings.hasNext) {
        buf append expressions.next
        buf append strings.next
      }
      buf.toString
    }
  }
}
