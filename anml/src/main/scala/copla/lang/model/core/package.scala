package copla.lang.model

import copla.lang.model

package object core {

  type CoreModel = Seq[InModuleBlock]

  sealed trait Block extends full.Block with InCore
  sealed trait InModuleBlock extends full.InModuleBlock with InCore
  sealed trait Statement extends InModuleBlock with full.Statement

  sealed trait InCore

  /** A block wrapping other blocks pertaining to the same scope. */
  sealed trait Wrapper extends Block {
    def wrapped: Seq[Block]
  }

  sealed trait Expr
  sealed trait StaticExpr extends Expr {
    def typ: Type
  }

  sealed trait SymExpr extends Expr {
    def typ: Type
  }
  sealed trait TimedSymExpr extends SymExpr
  sealed trait StaticSymExpr extends SymExpr with StaticExpr

  sealed trait Term extends Var

  case class Id(scope: Scope, name: String) {
    override def toString: String = scope.toScopedString(name)

    def toTPId: TPId = new TPId(this)
  }

  sealed trait Scope {

    def +(nestedScope: String): InnerScope = InnerScope(this, nestedScope)

    def makeNewId(): Id = Id(this, model.defaultId())
    def toScopedString(name: String): String
  }
  object RootScope extends Scope {
    override def toString: String = "root"
    def toScopedString(name: String): String = name
  }
  case class InnerScope(parent: Scope, name: String) extends Scope {

    override def toString: String = parent match {
      case RootScope => name
      case nonScope  => s"$nonScope.$name"
    }
    def toScopedString(name: String): String = s"$this.$name"
  }

  sealed trait Declaration[T] {
    def id: Id
  }

  case class Type(id: Id, parent: Option[Type]) {
    def isSubtypeOf(typ: Type): Boolean =
      this == typ || parent.exists(t => t == typ || t.isSubtypeOf(typ))

    def overlaps(typ: Type): Boolean = this.isSubtypeOf(typ) || typ.isSubtypeOf(this)

    def asScope: Scope = id.scope + id.name

    override def toString: String = id.toString
  }
  object Type {
    val Integers = Type(Id(RootScope, "integer"), None)
  }
  case class TypeDeclaration(typ: Type) extends Declaration[Type] with InModuleBlock with InCore {
    override def id: Id = typ.id
    override def toString: String = s"type $id" + {
      if(typ.parent.isDefined) " < " + typ.parent.get else ""
    }
  }

  sealed trait Var extends StaticExpr {
    def id: Id
    def typ: Type
  }
  object Var {
    def unapply(v: Var) = Option((v.id, v.typ))
  }

  sealed trait VarDeclaration[V <: Var] extends Declaration[Var] {
    def variable: Var
    def id: Id = variable.id
  }

  /** Variable declared locally */
  case class LocalVar(id: Id, typ: Type) extends Var {
    override def toString: String = id.toString
  }
  case class LocalVarDeclaration(variable: LocalVar)
      extends VarDeclaration[LocalVar]
      with Statement {
    override def toString: String = s"constant ${variable.typ} ${variable.id}"
  }

  /** Instance of a given type, result of the ANML statement "instance Type id;" */
  case class Instance(id: Id, typ: Type) extends Var with Term {
    override def toString: String = id.toString
  }
  case class InstanceDeclaration(instance: Instance)
      extends VarDeclaration[Instance]
      with InModuleBlock {
    override def variable: Instance = instance
    override def toString: String = s"instance ${instance.typ} ${instance.id}"
  }

  /** Denote the argument of the template of state variables and actions. */
  case class Arg(id: Id, typ: Type) extends Var {
    override def toString: String = id.toString
  }
  case class ArgDeclaration(arg: Arg) extends VarDeclaration[Arg] with Statement {
    override def variable: Arg = arg
    override def toString: String = s"${arg.typ.id} ${arg.id}"
  }

  sealed trait FunctionTemplate {
    def id: Id
    def typ: Type
    def params: Seq[Arg]
  }

  case class FluentTemplate(id: Id, typ: Type, params: Seq[Arg]) extends FunctionTemplate {
    override def toString: String = id.toString
  }

  case class ConstantTemplate(id: Id, typ: Type, params: Seq[Arg]) extends FunctionTemplate {
    override def toString: String = id.toString
  }

  case class FunctionDeclaration(func: FunctionTemplate)
      extends Declaration[FunctionTemplate]
      with InModuleBlock
      with InCore {
    override def id: Id = func.id
    override def toString: String = {
      val paramsString = "(" + func.params.map(p => s"${p.typ} ${p.id.name}").mkString(", ") + ")"
      func match {
        case _: FluentTemplate   => s"fluent ${func.typ} ${func.id}$paramsString"
        case _: ConstantTemplate => s"constant ${func.typ} ${func.id}$paramsString"
      }
    }
  }

  private type Param = Var

  sealed trait Function {
    def template: FunctionTemplate
    def params: Seq[Var]
  }

  case class Constant(override val template: ConstantTemplate, override val params: Seq[Var])
      extends full.Constant(template, params)
      with Function {
    override def toString: String = super.toString
  }
  class BoundConstant(override val template: ConstantTemplate, override val params: Seq[Term])
      extends Constant(template, params)

  case class Fluent(override val template: FluentTemplate, override val params: Seq[Param])
      extends full.Fluent(template, params)
      with Function {

    override def toString = s"$template(${params.mkString(", ")})"
  }

  sealed trait StaticAssertion extends full.StaticAssertion with Statement
  case class StaticEqualAssertion(override val left: Var, override val right: Var)
      extends full.StaticEqualAssertion(left, right)
      with StaticAssertion {
    override def toString: String = super.toString
  }
  case class StaticDifferentAssertion(override val left: Var, override val right: Var)
      extends full.StaticDifferentAssertion(left, right)
      with StaticAssertion {
    override def toString: String = super.toString
  }
  case class StaticAssignmentAssertion(override val left: BoundConstant, override val right: Term)
      extends full.StaticAssignmentAssertion(left, right)
      with StaticAssertion {
    override def toString: String = super.toString
  }
  case class BindAssertion(constant: Constant, variable: LocalVar) extends StaticAssertion {
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
    def valueAfterChange: Var
  }

  case class TimedEqualAssertion(start: TPRef, end: TPRef, fluent: Fluent, value: Var)
      extends TimedAssertion
      with RequiresSupport {
    override def toString: String = s"[$start, $end] $fluent == $value"
  }
  case class TimedAssignmentAssertion(start: TPRef, end: TPRef, fluent: Fluent, value: Var)
      extends TimedAssertion
      with ProvidesChange {
    override def valueAfterChange: Var = value
    override def toString: String = s"[$start,$end] $fluent := $value"
  }
  case class TimedTransitionAssertion(start: TPRef, end: TPRef, fluent: Fluent, from: Var, to: Var)
      extends TimedAssertion
      with RequiresSupport
      with ProvidesChange {
    override def valueAfterChange: Var = to
    override def toString: String = s"[$start, $end] $fluent == $from :-> $to"
  }

  case class TPId(id: Id) {
    override def toString: String = id.toString
  }
  object TPId {
    import scala.language.implicitConversions
    implicit def asId(tpId: TPId): Id = tpId.id
  }

  case class IntLiteral(value: Int) extends Term with Var {
    override def typ: Type = Type.Integers
    override def id: Id = Id(RootScope + "_integers_", value.toString)
  }

  sealed trait IntExpr extends full.IntExpr
  object IntExpr {
    def apply(lit: Int): IntExpr = VarIntExpr(IntLiteral(lit))
  }
  case class VarIntExpr(e: Var) extends IntExpr {
    require(e.typ.id.name == "integer")
  }
  case class Minus(e: IntExpr) extends IntExpr
  case class Add(lhs: IntExpr, rhs: IntExpr) extends IntExpr

  /** A timepoint, declared when appearing in the root of a context.*/
  case class TPRef(override val id: TPId, override val delay: IntExpr = IntExpr(0))
      extends full.TPRef(id, delay) {

    override def toString: String =
      id.toString + (delay match {
        case VarIntExpr(IntLiteral(d)) =>
          if(d == 0) ""
          else if(d > 0) s"+$d"
          else s"-${-d}"
        case x => s"+$x"
      })

    def +(d: IntExpr): TPRef = TPRef(id, Add(delay, d))
    override def +(d: Int): TPRef = this + IntExpr(d)
    def -(d: IntExpr): TPRef = TPRef(id, Add(delay, Minus(d)))
    override def -(d: Int): TPRef = this - IntExpr(d)

    def <=(other: TPRef): TBefore = TBefore(this, other)
    def <(other: TPRef): TBefore = TBefore(this, other - 1)
    def >=(other: TPRef): TBefore = other <= this
    def >(other: TPRef): TBefore = other < this
    def ===(other: TPRef): Seq[TBefore] = Seq(this <= other, this >= other)
  }
  sealed trait SimpleTPRefWitness {
    self: TPRef =>
    require(delay == IntExpr(0))
  }
  type SimpleTPRef = TPRef with SimpleTPRefWitness
  object SimpleTPRef {
    def apply(id: TPId): SimpleTPRef = new TPRef(id) with SimpleTPRefWitness
  }

  case class TimepointDeclaration(tp: TPRef) extends Declaration[TPRef] with Statement {
    require(tp.delay == IntExpr(0), "Cannot declare a relative timepoint.")
    override def id: Id = tp.id
    override def toString: String = s"timepoint $id"
  }

  case class TBefore(from: TPRef, to: TPRef) extends Statement {
    override def toString: String = s"$from <= $to"
  }

  case class ActionTemplate(scope: InnerScope, content: Seq[Statement]) extends InModuleBlock {
    def name: String = scope.name
    lazy val args: Seq[Arg] = content.collect { case ArgDeclaration(a) => a }

    override def toString: String =
      s"action $name(${args.map(a => s"${a.typ} ${a.id.name}").mkString(", ")})"

    lazy val start: TPRef = content
      .collectFirst {
        case TimepointDeclaration(tp) if tp.id.id == Id(scope, "start") => tp
      }
      .getOrElse(sys.error("No start timepoint in this action"))

    lazy val end: TPRef = content
      .collectFirst {
        case TimepointDeclaration(tp) if tp.id.id == Id(scope, "end") => tp
      }
      .getOrElse(sys.error("No end timepoint in this action"))

    /** Builds a new action instance with the given name*/
    def instance(instanceName: String): Action = {
      val instanceScope: InnerScope = scope.parent + instanceName

      val trans: Id => Id = x => {
        var transScope: Scope => Scope = null
        transScope = {
          case s if s == scope     => scope.parent + instanceName
          case RootScope           => RootScope
          case InnerScope(s, name) => transScope(s) + name
        }
        x match { case Id(s, name) => Id(transScope(s), name) }
      }
      import landscaper.transformations._

      // landscaper has problem with the new ADT, provide some help
      implicit val x1 = Trans[Id, Id, StaticExpr]
      implicit val x2 = Trans[Id, Id, VarIntExpr]
      implicit val x3 = Trans[Id, Id, IntExpr]
      implicit val x4 = Trans[Id, Id, TPRef]
      implicit val x5 = Trans[Id, Id, TBefore] // ok
      implicit val x6 = Trans[Id, Id, StaticAssertion] //ok
      implicit val x7 = Trans[Id, Id, ArgDeclaration] //ok
      implicit val x8 = Trans[Id, Id, TimedAssignmentAssertion] //ok
      implicit val x9 = Trans[Id, Id, TimedEqualAssertion] //ok
      implicit val x10 = Trans[Id, Id, TimedTransitionAssertion] //ok
      implicit val x11 = Trans[Id, Id, TimedAssertion] //ok
      implicit val x12 = Trans[Id, Id, LocalVarDeclaration] //ok
      implicit val x13 = Trans[Id, Id, TimepointDeclaration] //ok

      // derivation of Trans[Id,Id,Statement] never ends for an obscure reason, dispatch manually
      implicit val x14: Trans.Aux[Id, Id, Statement, Statement] = new Trans[Id, Id, Statement] {
        override type Result = Statement
        override def rewrite(f: Id => Id, in: Statement): Result = in match {
          case x: TimedAssertion       => x11.rewrite(f, x)
          case x: StaticAssertion      => x6.rewrite(f, x)
          case x: TBefore              => x5.rewrite(f, x)
          case x: ArgDeclaration       => x7.rewrite(f, x)
          case x: LocalVarDeclaration  => x12.rewrite(f, x)
          case x: TimepointDeclaration => x13.rewrite(f, x)
        }
      }
      val instanceContent = content.map(s => x14.rewrite(trans, s))
      Action(instanceScope, instanceContent, this)
    }
  }

  /** Instance of an action template */
  case class Action(scope: InnerScope, content: Seq[Statement], template: ActionTemplate) {
    def name: String = scope.name
    val args: Seq[Arg] = content.collect { case ArgDeclaration(a) => a }

    lazy val start: TPRef = content
      .collectFirst {
        case TimepointDeclaration(tp) if tp.id.id == Id(scope, "start") => tp
      }
      .getOrElse(sys.error("No start timepoint in this action"))

    lazy val end: TPRef = content
      .collectFirst {
        case TimepointDeclaration(tp) if tp.id.id == Id(scope, "end") => tp
      }
      .getOrElse(sys.error("No end timepoint in this action"))
  }

}

import copla.lang.model.core.{InnerScope, RootScope, SimpleTPRef}
import copla.lang.model.transforms.FullToCore
import copla.lang.parsing.anml.Parser

package object full {

  trait Block
  trait InModuleBlock extends Block
  trait InActionBlock extends Block
  trait Statement extends InModuleBlock with InActionBlock

  type Declaration[T] = core.Declaration[T]
  type Var = core.Var
  type VarDeclaration[T <: Var] = core.VarDeclaration[T]
  type Instance = core.Instance
  type InstanceDeclaration = core.InstanceDeclaration
  type LocalVar = core.LocalVar
  type LocalVarDeclaration = core.LocalVarDeclaration
  type Arg = core.Arg
  type ArgDeclaration = core.ArgDeclaration
  type SimpleTPRef = core.SimpleTPRef
  type TimepointDeclaration = core.TimepointDeclaration
  type FunctionTemplate = core.FunctionTemplate
  type FunctionDeclaration = core.FunctionDeclaration
  type ConstantTemplate = core.ConstantTemplate
  type FluentTemplate = core.FluentTemplate
  type Type = core.Type
  type TypeDeclaration = core.TypeDeclaration
  type Id = core.Id
  type Scope = core.Scope
  type Expr = core.Expr
  type StaticExpr = core.StaticExpr
  type SymExpr = core.SymExpr
  type TimedSymExpr = core.TimedSymExpr
  type StaticSymExpr = core.StaticSymExpr

  sealed trait IntExpr

  /** A timepoint, declared when appearing in the root of a context.*/
  class TPRef(val id: core.TPId, val delay: IntExpr = core.IntExpr(0)) {

    override def toString: String =
      id.toString + (delay match {
        case GenIntExpr(core.IntLiteral(d)) =>
          if(d == 0) ""
          else if(d > 0) s"+$d"
          else s"-${-d}"
        case x => s"+$x"
      })

    def +(d: IntExpr): TPRef = new TPRef(id, Add(delay, d))
    def +(d: Int): TPRef = this + core.IntExpr(d)
    def -(d: IntExpr): TPRef = new TPRef(id, Add(delay, Minus(d)))
    def -(d: Int): TPRef = this - core.IntExpr(d)

    def <=(other: TPRef): TBefore = TBefore(this, other)
    def <(other: TPRef): TBefore = TBefore(this, other - 1)
    def >=(other: TPRef): TBefore = other <= this
    def >(other: TPRef): TBefore = other < this
    def ===(other: TPRef): Seq[TBefore] = Seq(this <= other, this >= other)

    def -(other: TPRef) = Delay(other, this)
  }

  case class GenIntExpr(e: StaticExpr) extends IntExpr {
    require(e.typ.id.name == "integer")
  }
  case class Minus(e: IntExpr) extends IntExpr
  case class Add(lhs: IntExpr, rhs: IntExpr) extends IntExpr

  case class Interval(start: TPRef, end: TPRef) {
    override def toString: String = s"[$start, $end]"
  }

  case class TBefore(from: TPRef, to: TPRef) extends Statement {
    override def toString: String = s"$from <= $to"
  }

  /*** Time ***/
  case class Delay(from: TPRef, to: TPRef) {
    def <=(dur: IntExpr): TBefore = to <= from + dur
    def <(dur: IntExpr): TBefore = to < from + dur
    def >=(dur: IntExpr): TBefore = to >= from + dur
    def >(dur: IntExpr): TBefore = to > from + dur
    def +(time: IntExpr): Delay = Delay(from, to + time)
    def -(time: IntExpr): Delay = Delay(from, to - time)
    def ===(dur: IntExpr): Seq[TBefore] = Seq(this <= dur, this >= dur)
  }

  /** A block wrapping other blocks pertaining to the same scope. */
  sealed trait Wrapper extends Block {
    def wrapped: Seq[Block]
  }

  abstract class TimedAssertion(parent: Option[Ctx], name: String) extends Ctx with Block {
    override val scope: InnerScope = parent.map(_.scope).getOrElse(RootScope) + name

    val start: SimpleTPRef = SimpleTPRef(this.id("start").toTPId)
    val end: SimpleTPRef = SimpleTPRef(this.id("end").toTPId)

    override val store: BlockStore[Statement] = new BlockStore[Statement]() +
      new TimepointDeclaration(start) +
      new TimepointDeclaration(end)
  }
  case class TimedEqualAssertion(
      left: TimedSymExpr, // TODO: should not restrict this to be symbolic
      right: StaticExpr,
      parent: Option[Ctx],
      name: String)
      extends TimedAssertion(parent, name) {
    if(name == "__296")
      println(name)
    override def toString: String =
      if(name.startsWith(reservedPrefix)) s"$left == $right"
      else s"$name: $left == $right"
  }

  case class TimedTransitionAssertion(fluent: TimedSymExpr,
                                      from: StaticExpr,
                                      to: StaticExpr,
                                      parent: Option[Ctx],
                                      name: String)
      extends TimedAssertion(parent, name) {
    override def toString: String =
      if(name.startsWith(reservedPrefix)) s"$fluent == $from :-> $to"
      else s"$name: $fluent == $from :-> $to"
  }

  case class TimedAssignmentAssertion(fluent: TimedSymExpr,
                                      to: StaticExpr,
                                      parent: Option[Ctx],
                                      name: String)
      extends TimedAssertion(parent, name) {
    override def toString: String =
      if(name.startsWith(reservedPrefix)) s"$fluent := $to"
      else s"$name: $fluent := $to"
  }

  trait TemporalQualifier
  case class Equals(interval: Interval) extends TemporalQualifier {
    override def toString: String = interval.toString
  }
  case class Contains(interval: Interval) extends TemporalQualifier {
    override def toString: String = s"$interval contains"
  }

  case class TemporallyQualifiedAssertion(qualifier: TemporalQualifier, assertion: TimedAssertion)
      extends Statement
      with Wrapper {

    override def wrapped = Seq(assertion)
    override def toString = s"$qualifier $assertion"
  }

  trait StaticAssertion extends Statement
  class StaticEqualAssertion(val left: StaticExpr, val right: StaticExpr) extends StaticAssertion {
    override def toString: String = s"$left == $right"
  }
  class StaticDifferentAssertion(val left: StaticExpr, val right: StaticExpr)
      extends StaticAssertion {
    override def toString: String = s"$left != $right"
  }
  class StaticAssignmentAssertion(val left: StaticExpr, val right: StaticExpr)
      extends StaticAssertion {
    override def toString: String = s"$left := $right"
  }

  class Fluent(val template: FluentTemplate, val params: Seq[StaticExpr]) extends TimedSymExpr {
    require(template.params.size == params.size)
    template.params.zip(params).foreach {
      case (tpl, v) =>
        require(v.typ.isSubtypeOf(tpl.typ), s"$v is not of type ${tpl.typ}")
    }

    override def typ: Type = template.typ

    override def toString = s"$template(${params.mkString(", ")})"
  }

  class Constant(val template: ConstantTemplate, val params: Seq[StaticExpr]) extends StaticExpr {
    require(template.params.size == params.size)
    template.params.zip(params).foreach {
      case (tpl, v) =>
        require(v.typ.isSubtypeOf(tpl.typ), s"$v is not of type ${tpl.typ}")
    }

    override def typ: Type = template.typ

    override def toString: String = s"$template(${params.mkString(", ")})"
  }

  class ActionTemplate(override val name: String,
                       val containingModel: Model,
                       override val store: BlockStore[InActionBlock] = new BlockStore())
      extends Ctx
      with InModuleBlock {
    override val parent: Some[Ctx] = Some(containingModel)
    override val scope: InnerScope = parent.get.scope + name

    def +(block: InActionBlock): ActionTemplate =
      new ActionTemplate(name, containingModel, store + block)
    def ++(newBlocks: Seq[InActionBlock]): ActionTemplate = newBlocks.headOption match {
      case Some(first) => (this + first) ++ newBlocks.tail
      case None        => this
    }

    override def toString: String =
      s"action $name(${store.blocks
        .collect { case x: ArgDeclaration => s"${x.arg.typ} ${x.arg.id.name}" }
        .mkString(", ")}) {\n" +
        store.blocks.map("    " + _.toString).mkString("\n") +
        "  };"
  }

  case class Model(store: BlockStore[InModuleBlock] = new BlockStore()) extends Ctx {
    override def parent = None
    override def name = "_module_"
    override val scope: Scope = RootScope

    def +(block: InModuleBlock): Option[Model] = {
      Some(Model(store + block))
    }

    def ++(blocks: Seq[InModuleBlock]): Option[Model] = {
      blocks.foldLeft(Option(this))((m, block) => m.flatMap(_ + block))
    }

    def asCore(opt: transforms.Config = transforms.Config()): core.CoreModel =
      FullToCore.trans(this)

    override def toString: String =
      "module:\n" +
        store.blocks
          .filter(!Parser.baseAnmlModel.store.blocks.contains(_))
          .map("  " + _)
          .mkString("\n")
  }

  class BlockStore[+T <: Block] private (val blocks: Vector[T],
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
  }

  trait Ctx {
    def scope: Scope

    def id(name: String): Id = new Id(scope, name)

    def parent: Option[Ctx]
    def name: String
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
            .get(new Id(scope + subScopeName, name))
        case Nil =>
          sys.error("Invalid name: " + localID)
        case _ =>
          sys.error(s"No support for multiple nested declarations: $localID")
      }).orElse(parent.flatMap(_.findDeclaration(localID)))
    }

    def findVariable(name: String): Option[Var] =
      findDeclaration(name: String).flatMap {
        case decl: VarDeclaration[_] => Some(decl.variable)
        case _                       => None
      }

    def findTimepoint(name: String): Option[TPRef] =
      findDeclaration(name).flatMap {
        case decl: TimepointDeclaration => Some(decl.tp)
        case _                          => None
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