package dahu.planning.model

import dahu.planning.model
import dahu.planning.model.common.operators.{BinaryOperator, UnaryOperator}
import spire.math.Real
import spire.implicits._

package object common {

  final case class Id(scope: Scope, name: String) {
    override def toString: String = scope.toScopedString(name)

    def toTPId = Timepoint(this)
  }

  object Timepoint {
    def apply(id: Id): LocalVar = LocalVar(id, Type.Time)
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

  sealed trait Type {
    def id: Id
    def parent: Option[Type]

    def isSubtypeOf(typ: Type): Boolean =
      this == typ || parent.exists(t => t == typ || t.isSubtypeOf(typ))

    def overlaps(typ: Type): Boolean =
      this.isSubtypeOf(typ) || typ.isSubtypeOf(this)

    def lowestCommonAncestor(typ: Type): Option[Type] =
      if(this.isSubtypeOf(typ))
        Some(typ)
      else if(typ.isSubtypeOf(this))
        Some(this)
      else
        parent match {
          case Some(father) => father.lowestCommonAncestor(typ)
          case None         => None
        }

    def asScope: Scope = id.scope + id.name

    override def toString: String = id.toString
  }
  object Type {

    sealed trait ObjType extends Type {
      def id: Id
      def parent: Option[ObjType]
    }

    object ObjectTop extends ObjType {
      override def id: Id = Id(RootScope, "__object__")
      override def parent: Option[Nothing] = None
    }

    final case class ObjSubType(id: Id, father: ObjType) extends ObjType {
      def parent: Some[ObjType] = Some(father)
    }

    sealed trait IRealType extends Type {
      def min: Option[Real]
      def max: Option[Real]
    }

    object Reals extends IRealType {
      def id: Id = Id(RootScope, "float")
      override def parent: Option[Nothing] = None
      override def min: Option[Real] = None
      override def max: Option[Real] = None
    }

    final case class RealSubType(id: Id,
                                 father: IRealType,
                                 min: Option[Real] = None,
                                 max: Option[Real] = None)
        extends IRealType {
      require(min.forall(m => father.min.forall(_ <= m)))
      require(max.forall(m => father.max.forall(m >= _)))

      override def parent: Some[IRealType] = Some(father)
    }

    sealed trait IIntType extends IRealType {
      def intMin: Option[BigInt]
      def intMax: Option[BigInt]
      override def min: Option[Real] = intMin.map(Real(_))
      override def max: Option[Real] = intMax.map(Real(_))
    }

    object Integers extends IIntType {
      override def id: Id = Id(RootScope, "integer")
      override def parent: Some[Reals.type] = Some(Reals)
      override def intMin: Option[BigInt] = None
      override def intMax: Option[BigInt] = None
    }

    final case class IntSubType(id: Id,
                                father: IIntType,
                                intMin: Option[BigInt] = None,
                                intMax: Option[BigInt] = None)
        extends IIntType {
      require(intMin.forall(min => father.intMin.forall(_ <= min)))
      require(intMax.forall(max => father.intMax.forall(max >= _)))

      override def parent: Some[IIntType] = Some(father)
    }

    val Time = IntSubType(Id(RootScope, "time"), Integers)

    val Boolean = ObjSubType(Id(RootScope, "boolean"), ObjectTop)

//    val Numeric = Type(Id(RootScope, "__numeric__"), None)
//    val Integer = Type(Id(RootScope, "integer"), Some(Numeric))
//    val Time = Integer //Type(Id(RootScope, "time"), Some(Integer))
//    val Float = Type(Id(RootScope, "float"), Some(Numeric))
//    val Boolean = Type(Id(RootScope, "boolean"), None)

    val True = Instance(Id(RootScope, "true"), Boolean)
    val False = Instance(Id(RootScope, "false"), Boolean)

    val Start = Id(RootScope, "start")
    val End = Id(RootScope, "end")
  }
  sealed trait Expr {
    def typ: Type
  }
  sealed trait Term extends Expr {
    def id: Id
  }
  object Term {
    def unapply(v: Term) = Option((v.id, v.typ))
  }

  sealed trait Cst extends Term
  sealed trait Var extends Term

  case class IntLiteral(value: Int) extends Cst {
    override def typ: Type = Type.Integers
    override def id: Id = Id(RootScope + "_integers_", value.toString)
    override def toString: String = value.toString
  }

  /** Variable declared locally */
  case class LocalVar(id: Id, typ: Type) extends Var {
    override def toString: String = id.toString
  }

  /** Instance of a given type, result of the ANML statement "instance Type id;" */
  case class Instance(id: Id, typ: Type) extends Cst {
    override def toString: String = id.toString
  }

  /** Denote the argument of the template of state variables and actions. */
  case class Arg(id: Id, typ: Type) extends Var {
    override def toString: String = id.toString
  }

  case class Op2(op: BinaryOperator, lhs: Expr, rhs: Expr) extends Expr {
    override def typ: Type = op.tpe(lhs.typ, rhs.typ) match {
      case Right(t)  => t
      case Left(err) => sys.error(err)
    }

    override def toString: String = s"(${op.op} $lhs $rhs)"
  }
  case class Op1(op: UnaryOperator, lhs: Expr) extends Expr {
    override def typ: Type = op.tpe(lhs.typ) match {
      case Right(t)  => t
      case Left(err) => sys.error(err)
    }
    override def toString: String = s"(${op.op} $lhs)"
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

}
