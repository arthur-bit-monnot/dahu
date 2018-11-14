package dahu.planning.model

import cats.Functor
import dahu.planning.model.common.Type.BooleanType
import dahu.planning.model.common._
import dahu.planning.model.common.operators.{BinaryOperator, UnaryOperator}

package object full {

  sealed trait Block
  sealed trait InModuleBlock extends Block
  sealed trait InActionBlock extends Block
  sealed trait Statement extends InModuleBlock with InActionBlock

  sealed trait Declaration[T] {
    def id: Id
  }

  case class TypeDeclaration(typ: Type) extends Declaration[Type] with InModuleBlock {
    override def id: Id = typ.id
    override def toString: String = s"type $id" + {
      if(typ.parent.isDefined) " < " + typ.parent.get else ""
    }
  }
  sealed trait VarDeclaration[V <: Term] extends Declaration[Term] {
    def variable: Term
    def id: Id = variable.id
  }
  case class LocalVarDeclaration(variable: LocalVar)
      extends VarDeclaration[LocalVar]
      with Statement {
    override def toString: String = s"constant ${variable.typ} ${variable.id}"
  }
  case class InstanceDeclaration(instance: Instance)
      extends VarDeclaration[Instance]
      with InModuleBlock {
    override def variable: Instance = instance
    override def toString: String = s"instance ${instance.typ} ${instance.id}"
  }
  case class ArgDeclaration(arg: Arg) extends VarDeclaration[Arg] with InActionBlock {
    override def variable: Arg = arg
    override def toString: String = s"${arg.typ.id} ${arg.id}"
  }
  case class FunctionDeclaration(func: FunctionTemplate)
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

  sealed trait Expr {
    def typ: Type
  }
  sealed trait StaticExpr extends Expr
  sealed trait TimedExpr extends Expr

  sealed trait ExprTree extends StaticExpr
  case class BinaryExprTree(op: BinaryOperator, lhs: StaticExpr, rhs: StaticExpr)(
      implicit predef: Predef)
      extends ExprTree {
    override val typ: Type = op.tpe(lhs.typ, rhs.typ) match {
      case Right(tpe) => tpe
      case Left(err) =>
        sys.error(err)
    }

    override def toString: String = s"(${op.op} $lhs $rhs)"
  }
  case class UnaryExprTree(op: UnaryOperator, lhs: StaticExpr) extends ExprTree {
    override val typ: Type = op.tpe(lhs.typ) match {
      case Right(tpe) => tpe
      case Left(err)  => sys.error(err)
    }

    override def toString: String = s"(${op.op} $lhs)"
  }

  sealed trait CommonTerm extends StaticExpr
  object CommonTerm {
    def apply(v: Term): CommonTerm = v match {
      case t: Cst => ConstantExpr(t)
      case v: Var => Variable(v)
    }
  }

  case class Variable(v: Var) extends CommonTerm {
    override def typ: Type = v.typ
    override def toString: String = v.toString
  }
  case class ConstantExpr(term: Cst) extends CommonTerm {
    override def typ: Type = term.typ
    override def toString: String = term.toString
  }

  /** A block wrapping other blocks pertaining to the same scope. */
  sealed trait Wrapper extends Block {
    def wrapped: Seq[Block]
  }

  sealed abstract class TimedAssertion(parent: Option[Ctx], id: Id)(implicit predef: Predef)
      extends Ctx
      with Block {
    def fluent: TimedExpr

    override val scope: InnerScope = parent.map(_.scope).getOrElse(RootScope) + id.name

    val start: LocalVar = Timepoint(this.id("start"))
    val end: LocalVar = Timepoint(this.id("end"))

    override val store: BlockStore[Statement] = new BlockStore[Statement]() +
      LocalVarDeclaration(start) +
      LocalVarDeclaration(end)
  }
  case class TimedBooleanAssertion(fluent: TimedExpr,
                                   op: BinaryOperator,
                                   right: StaticExpr,
                                   parent: Option[Ctx],
                                   id: Id)(implicit predef: Predef)
      extends TimedAssertion(parent, id) {
    override def toString: String = id match {
      case Id(_, name) => s"$name: $fluent $op $right"
      case _           => s"$fluent $op $right"
    }
  }
  object TimedEqualAssertion {
    def apply(fluent: TimedExpr, right: StaticExpr, parent: Option[Ctx], id: Id)(
        implicit predef: Predef): TimedBooleanAssertion =
      TimedBooleanAssertion(fluent, operators.Eq, right, parent, id)
  }

  case class TimedTransitionAssertion(fluent: TimedExpr,
                                      from: StaticExpr,
                                      to: StaticExpr,
                                      parent: Option[Ctx],
                                      id: Id)(implicit predef: Predef)
      extends TimedAssertion(parent, id) {
    override def toString: String = id match {
      case Id(_, name) => s"$name: $fluent == $from :-> $to"
      case _           => s"$fluent == $from :-> $to"
    }
  }

  case class TimedAssignmentAssertion(fluent: TimedExpr,
                                      to: StaticExpr,
                                      parent: Option[Ctx],
                                      id: Id)(implicit predef: Predef)
      extends TimedAssertion(parent, id) {
    override def toString: String = id match {
      case Id(_, name) => s"$name: $fluent := $to"
      case _           => s"$fluent := $to"
    }

  }

  trait TemporalQualifier
  case class Equals(interval: Interval[StaticExpr]) extends TemporalQualifier {
    require(interval.start.typ.isSubtypeOf(Type.Integers))
    require(interval.end.typ.isSubtypeOf(Type.Integers))
    override def toString: String = interval.toString
  }
  case class Contains(interval: Interval[StaticExpr]) extends TemporalQualifier {
    require(interval.start.typ.isSubtypeOf(Type.Integers))
    require(interval.end.typ.isSubtypeOf(Type.Integers))
    override def toString: String = s"$interval contains"
  }

  case class TemporallyQualifiedAssertion(qualifier: TemporalQualifier, assertion: TimedAssertion)
      extends Statement
      with Wrapper {

    override def wrapped = Seq(assertion)
    override def toString = s"$qualifier $assertion"
  }

  trait StaticAssertion extends Statement
  case class BooleanAssertion(expr: StaticExpr) extends StaticAssertion {
    require(expr.typ.isInstanceOf[BooleanType])
    override def toString: String = expr.toString
  }

  case class StaticAssignmentAssertion(left: Constant, right: StaticExpr) extends StaticAssertion {
    override def toString: String = s"$left := $right"
  }

  case class Fluent(template: FluentTemplate, params: Seq[StaticExpr]) extends TimedExpr {
    require(template.params.size == params.size)
    template.params.zip(params).foreach {
      case (tpl, v) =>
        require(v.typ.isSubtypeOf(tpl.typ), s"$v is not of type ${tpl.typ}")
    }

    override def typ: Type = template.typ

    override def toString = s"$template(${params.mkString(", ")})"
  }

  case class Constant(template: ConstantTemplate, params: Seq[StaticExpr]) extends StaticExpr {
    require(template.params.size == params.size)
    template.params.zip(params).foreach {
      case (tpl, v) =>
        require(v.typ.isSubtypeOf(tpl.typ), s"$v is not of type ${tpl.typ}")
    }

    override def typ: Type = template.typ

    override def toString: String = s"$template(${params.mkString(", ")})"
  }

  final case class ActionTemplate(id: Id,
                                  containingModel: Model,
                                  override val store: BlockStore[InActionBlock] = new BlockStore())
      extends Ctx
      with InModuleBlock {
    override val parent: Some[Ctx] = Some(containingModel)
    override val scope: InnerScope = parent.get.scope + name

    def +(block: InActionBlock): ActionTemplate =
      ActionTemplate(id, containingModel, store + block)
    def ++(newBlocks: Seq[InActionBlock]): ActionTemplate = newBlocks.headOption match {
      case Some(first) => (this + first) ++ newBlocks.tail
      case None        => this
    }

    def map(f: InActionBlock => InActionBlock): ActionTemplate =
      ActionTemplate(id, containingModel, store.map(f))

    override def toString: String =
      s"action $name(${store.blocks
        .collect { case x: ArgDeclaration => s"${x.arg.typ} ${x.arg.id.name}" }
        .mkString(", ")}) {\n" +
        store.blocks.map("    " + _.toString).mkString("\n") +
        "  };"
  }

  final case class Model(store: BlockStore[InModuleBlock] = new BlockStore()) extends Ctx {
    override def parent = None
    override val scope: Scope = RootScope
    override val id: Id = scope.makeNewId()

    def +(block: InModuleBlock): Option[Model] = {
      Some(Model(store + block))
    }

    def ++(blocks: Seq[InModuleBlock]): Option[Model] = {
      blocks.foldLeft(Option(this))((m, block) => m.flatMap(_ + block))
    }

    def map(f: InModuleBlock => InModuleBlock): Model = {
      Model(store.map(f))
    }

    override def toString: String =
      "module:\n" +
        store.blocks
          .map("  " + _)
          .mkString("\n")
  }

  final class BlockStore[+T <: Block] private (val blocks: Vector[T],
                                               val declarations: Map[Id, Declaration[_]]) {

    def this() = this(Vector(), Map())

    def +[B >: T <: Block](b: B): BlockStore[B] = {
      val newBlocks = blocks :+ b
      val toProcess = (b match {
        case wrapper: Wrapper =>
          wrapper +: wrapper.wrapped
        case x => Seq(x)
      }).flatMap {
        case ctx: Ctx => ctx.store.blocks :+ ctx
        case x        => Seq(x)
      }
      val newDeclarations = declarations ++ toProcess.collect {
        case x: Declaration[_] => (x.id, x)
      }

      new BlockStore[B](newBlocks, newDeclarations)
    }

    def map[B <: Block](f: T => B): BlockStore[B] = {
      blocks.foldLeft(new BlockStore[B]())((store, b) => store + f(b))
    }
  }

  trait Ctx {
    def scope: Scope
    def id: Id

    def id(name: String): Id = Id(scope, name)

    def parent: Option[Ctx]
    final def name: String = id.name
    def root: Ctx = parent match {
      case Some(p) => p.root
      case None    => this
    }
    def store: BlockStore[Block]

    def findDeclaration(localID: String): Option[Declaration[_]] = {
      (localID.split("\\.").toList match {
        case single :: Nil =>
          store.declarations.get(id(single))
        case subScopeName :: name :: Nil =>
          store.declarations
            .get(Id(scope + subScopeName, name))
        case Nil =>
          sys.error("Invalid name: " + localID)
        case _ =>
          sys.error(s"No support for multiple nested declarations: $localID")
      }).orElse(parent.flatMap(_.findDeclaration(localID)))
    }

    def findVariable(name: String): Option[Term] =
      findDeclaration(name: String).flatMap {
        case decl: VarDeclaration[_] => Some(decl.variable)
        case _                       => None
      }

    def findTimepoint(name: String)(implicit predef: Predef): Option[LocalVar] =
      findDeclaration(name).flatMap {
        case LocalVarDeclaration(v @ LocalVar(_, tpe)) if tpe.isSubtypeOf(predef.Time) => Some(v)
        case _                                                                         => None
      }

    def findType(name: String): Option[Type] =
      findDeclaration(name).flatMap {
        case decl: TypeDeclaration => Some(decl.typ)
        case _                     => None
      }

    def findFunction(name: String): Option[FunctionTemplate] =
      findDeclaration(name).flatMap {
        case decl: FunctionDeclaration => Some(decl.func)
        case _                         => None
      }

    def findFluent(name: String): Option[FluentTemplate] =
      findFunction(name).flatMap {
        case t: FluentTemplate => Some(t)
        case _                 => None
      }

    def findConstant(name: String): Option[ConstantTemplate] =
      findFunction(name).flatMap {
        case t: ConstantTemplate => Some(t)
        case _                   => None
      }
  }
}
