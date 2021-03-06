package dahu.model.input

import java.util.Objects

import cats.Id
import dahu.utils._
import dahu.graphs.DAG
import dahu.model.functions._
import dahu.model.products.ProductTag
import dahu.model.structs._
import dahu.model.types._

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.runtime.ScalaRunTime

/** Evaluation yields an Either[ConstraintViolated, T] */
sealed trait Expr[+T] {
  def typ: Tag[T] @uncheckedVariance
  val hash: Int
  override final def hashCode(): Int = hash
}
object Expr {

  implicit def dagInstance: DAG[Id, Expr[Any]] = new DAG[Id, Expr[Any]] {
    override def algebra(a: Expr[Any]): Id[Expr[Any]] = a
    override def foreachChild(graph: Id[Expr[Any]])(f: Expr[Any] => Unit): Unit = graph match {
      case x: Computation[_]          => x.args.foreach(f)
      case x: Product[_]              => x.members.foreach(f)
      case x: Input[_]                =>
      case x: Cst[_]                  =>
      case ITE(cond, onTrue, onFalse) => f(cond); f(onTrue); f(onFalse)
      case x: DynInput[_]             =>
      case x: DynCollector[_]         =>
      case Apply(l, i)                => f(l); f(i)
      case x: Lambda[_, _]            => f(x.parameterizedTree); f(x.inputVar)
      case Lambda.Param(_)            =>
      case Sequence(ms)               => ms.foreach(f)
    }
  }
}

sealed abstract class Term[T] extends Expr[T]

/** Evaluation yields a Right[T] */
final case class Input[T: Tag] private (id: TypedIdent) extends Term[T] {
  require(!id.typ.isInstanceOf[TagIsoInt[_]])
  require(typ == id.typ)
  override def typ: Tag[T] = Tag[T]
  override val hash: Int = ScalaRunTime._hashCode(this)
  override def toString: String = "?" + id
}
object Input {

  /** Wrapper to generate a new unique ID from the given one */
  private case class Raw(id: Any) {
    override def toString: String = id.toString + "!"
  }

  /** This should be the only entry point for building an Input.
    * It checks whether the Input is isomorphic to an int and in which case, the returned value is a box
    * of a raw int variable.
    */
  private def build[T](id: Ident)(implicit tag: Tag[T]): Expr[T] = {
    tag match {
      case _: RawInt => new Input[T](TypedIdent(id, tag))
      case t: TagIsoInt[T] =>
        val rawInput =
          new Input[Int](TypedIdent(Ident(id.scope, Raw(id.lid)), t.rawType))(t.rawType)
        Computation1(t.box, rawInput)
      case _ => new Input[T](TypedIdent(id, tag))
    }
  }

  def apply[T: Tag](name: Any, scope: Scope): Expr[T] =
    build[T](Ident(scope, name))
  def apply[T: Tag](scope: Scope): Expr[T] =
    build[T](Ident.anonymous(scope))
}

final case class Cst[T] private (value: T, typ: Tag[T]) extends Term[T] {
  require(typ != null)
  require(typ match {
    case _: TagIsoInt[_]    => false
    case _ if typ.isBoolean => value == 0 || value == 1
    case ri: RawInt         => ri.min <= value.asInstanceOf[Int] && value.asInstanceOf[Int] <= ri.max
    case _                  => true
  })
  override val hash: Int = ScalaRunTime._hashCode(this)

  override def toString: String = value.toString
}

object Cst {

  /** This should be the only entry point for building an Input.
    * It checks whether the Input is isomorphic to an int and in which case, the returned value is a box
    * of a raw int variable.
    */
  private def build[T](value: T, typ: Tag[T]): Expr[T] = {
    typ match {
      case _: RawInt => new Cst(value, typ)
      case t: TagIsoInt[T] =>
        val rawCst =
          new Cst[Int](t.toInt(value), t.rawType)
        Computation1(t.box, rawCst)
      case _ => new Cst[T](value, typ)
    }
  }

  def apply[T: Tag](v: T): Expr[T] = build(v, Tag[T])

  def unapply[T](arg: Expr[T]): Option[T] = arg match {
    case x: Cst[T] => Some(x.value)
    case _         => None
  }
}

final case class ITE[T](cond: Expr[Bool], onTrue: Expr[T], onFalse: Expr[T]) extends Expr[T] {
  override def typ: Tag[T] = onTrue.typ
  override val hash: Int = ScalaRunTime._hashCode(this)
}

sealed abstract class Computation[O] extends Expr[O] {
  override def typ: Tag[O] = f.outType
  def f: Fun[O]
  def args: Seq[Expr[Any]] // TODO: make args a Vec
  override final lazy val hash: Int = Objects.hash(f, args)

  override final def equals(o: scala.Any): Boolean = o match {
    case o: Computation[_] =>
      if(this eq o) true
      else if(this.hash != o.hash) false
      else f == o.f && args == o.args
    case _ => false
  }
  override def toString: String = s"$f(${args.mkString(", ")})"
}

final case class Sequence[T: Tag](members: Vec[Expr[T]])(implicit ct: ClassTag[Vec[T]])
    extends Expr[Vec[T]] {
  override def typ: SequenceTag[T] = SequenceTag[T]
  override val hash: Int = ScalaRunTime._hashCode(this)
  override def toString: String = members.toString
}
object Sequence {
  def apply[T: Tag](members: Seq[Expr[T]])(implicit classTag: ClassTag[Vec[T]]): Sequence[T] =
    Sequence(Vec.fromSeq(members))
}

final case class Product[T[_[_]]](value: T[Expr])(implicit tt: ProductTag[T]) extends Expr[T[Id]] {
  require(tt != null)
  override def typ: ProductTag[T] = tt
  def members: Vec[Expr[Any]] = tt.getFields(value)
  override val hash: Int = ScalaRunTime._hashCode(this)
  override def toString: String = "§" + value
}

object Computation {
  def apply[I, O](f: Fun1[I, O], in: Expr[I]): Computation1[I, O] =
    Computation1(f, in)

  def apply[I1, I2, O](f: Fun2[I1, I2, O], in1: Expr[I1], in2: Expr[I2]): Computation2[I1, I2, O] =
    Computation2(f, in1, in2)

  def apply[I1, I2, I3, O](f: Fun3[I1, I2, I3, O],
                           in1: Expr[I1],
                           in2: Expr[I2],
                           in3: Expr[I3]): Computation3[I1, I2, I3, O] =
    Computation3(f, in1, in2, in3)

  def apply[I, O](fun: FunN[I, O], arguments: Seq[Expr[I]]): Computation[O] =
    new Computation[O] {
      override def f: Fun[O] = fun
      override def args: Seq[Expr[Any]] = arguments
    }
}
final case class Computation1[I, O](f: Fun1[I, O], in: Expr[I]) extends Computation[O] {
  override val args: Seq[Expr[Any]] = Seq(in)
}

final case class Computation2[I1, I2, O](f: Fun2[I1, I2, O], in: Expr[I1], in2: Expr[I2])
    extends Computation[O] {
  override val args: Seq[Expr[Any]] = Seq(in, in2)
}

final case class Computation3[I1, I2, I3, O](f: Fun3[I1, I2, I3, O],
                                             in: Expr[I1],
                                             in2: Expr[I2],
                                             in3: Expr[I3])
    extends Computation[O] {
  override val args: Seq[Expr[Any]] = Seq(in, in2, in3)
}

final case class Computation4[I1, I2, I3, I4, O](f: Fun4[I1, I2, I3, I4, O],
                                                 in: Expr[I1],
                                                 in2: Expr[I2],
                                                 in3: Expr[I3],
                                                 in4: Expr[I4])
    extends Computation[O] {
  override val args: Seq[Expr[Any]] = Seq(in, in2, in3, in4)
}

sealed trait DynExpr[T] extends Expr[T]

final case class DynInput[T: Tag](id: TypedIdent) extends DynExpr[T] {
  require(id.typ == Tag[T])
  override def typ: Tag[T] = Tag[T]
  override val hash: Int = ScalaRunTime._hashCode(this)
}

final case class DynCollector[T: Tag](collectedTpe: Tag[T], filter: Option[TagAny => Boolean])
    extends DynExpr[Vec[OptionalF[cats.Id, T]]] {
  override val typ: Tag[Vec[Optional[T]]] = Tag.ofSequence[Optional[T]](OptionalF.tagOf[T])
  override val hash: Int = ScalaRunTime._hashCode(this)
}

final class Lambda[I: Tag, O: Tag](val inputVar: Lambda.Param[I], val parameterizedTree: Expr[O])
    extends Expr[I ->: O] {
  def outTag: Tag[O] = Tag[O]

  def id: Lambda.LambdaIdent = inputVar.ident

  override def typ: LambdaTag[I, O] = LambdaTag[I, O]

  override val hash: Int = Objects.hash(inputVar, parameterizedTree)

  override def equals(o: scala.Any): Boolean = o match {
    case l: Lambda[_, _] => inputVar == l.inputVar && parameterizedTree == l.parameterizedTree
    case _               => false
  }

  override def toString: String = s"Lambda($id)"
}

object Lambda {
  private val dummyLambdaParam =
    Input[Nothing](TypedIdent(Ident.anonymous(Scope.root), Tag.default[Nothing]))(
      Tag.default[Nothing])
  def apply[I: Tag, O: Tag](f: Expr[I] => Expr[O]): Lambda[I, O] = {
    val treeShape = f(dummyLambdaParam)
    val id = new LambdaIdent(treeShape, None, None)
    val param = Lambda.Param[I](id)
    new Lambda[I, O](param, f(param))
  }

  final case class Param[I: Tag](ident: LambdaIdent) extends Expr[I] {
    override def typ: Tag[I] = Tag[I]
    override val hash: Int = ScalaRunTime._hashCode(this)
  }
  final class LambdaIdent(private val treeShape: Expr[Any], // can be null
                          val name: Option[String],
                          val qualifier: Option[String]) {
    override def hashCode(): Int = Objects.hash(treeShape, name, qualifier)

    override def equals(o: scala.Any): Boolean = o match {
      case li: LambdaIdent =>
        if(this eq li) true
        else if(treeShape == null || li.treeShape == null)
          false
        else
          Objects.equals(treeShape, li.treeShape) &&
          name == li.name && qualifier == li.qualifier
      case _ => false
    }

    override def toString: String =
      name match {
        case Some(x) => x
        case _       => "Λ" + LambdaIdent.getShortRep(hashCode())
      }

//      name.getOrElse("Λ" + math.abs(hashCode()).toString) + qualifier.map("-" + _).getOrElse("")

    def qualified(str: String): LambdaIdent = {
      assert(qualifier.isEmpty)
      new LambdaIdent(treeShape, name, Some(str))
    }
  }
  object LambdaIdent {

    def apply(name: String): LambdaIdent = new LambdaIdent(null, Some(name), None)

    private var cnt = 0
    private val shortIDMap: mutable.Map[Int, Int] = mutable.Map()
    private def getShortRep(hashCode: Int): Int = {
      if(!shortIDMap.contains(hashCode)) {
        cnt += 1
        shortIDMap.update(hashCode, cnt)
      }
      shortIDMap(hashCode)

    }
  }
}

final case class Apply[I, O: Tag](l: Expr[I ->: O], in: Expr[I]) extends Expr[O] {
  override def typ: Tag[O] = Tag[O]
  override val hash: Int = ScalaRunTime._hashCode(this)
}
