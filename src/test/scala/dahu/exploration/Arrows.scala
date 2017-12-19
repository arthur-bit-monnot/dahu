package dahu.exploration


import dahu.expr.BagPacking
import dahu.recursion._

import dahu.expr.labels.Labels._

object Arrows extends App {

  sealed trait Arrow[-Domain, +CoDomain] {

    def apply(v: Domain): CoDomain

    def andThen[Res](next: Arrow[CoDomain, Res]): Arrow[Domain, Res] =
      ComposedArrow(this, next)
  }
  type ==>[A, B] = Arrow[A, B]

  object Arrow {
    def lift[A, B](f: A => B): Arrow[A, B] = FunctionArrow(f)
  }


  final case class FunctionArrow[In, Res](f: In => Res) extends Arrow[In,Res] {
    override def apply(v1: In): Res = f(v1)
  }

  final case class ComposedArrow[A, B, C](ab: Arrow[A,B], bc: Arrow[B,C]) extends Arrow[A, C] {
    override def apply(v1: A): C = bc(ab(v1))
  }

  trait TypeInstances[T] {

    def enumerate: Array[T]
  }


  sealed abstract class IndexLabelImpl {
    type T
    def apply(s: Int): T
    def unwrap(lbl: T): Int
  }

  trait ASTable  {
    /** Opaque type representing the IDs of the expression in this table. */
    val EId: IndexLabelImpl = new IndexLabelImpl {
      type T = Int
      override def apply(s: Int): T = s
      override def unwrap(lbl: T): Int = lbl
    }
    type EId = this.EId.T

    type Expr = ResultF[EId]
    type Variable = InputF[EId]

    def root: EId
    def arrow: EId ==> Expr

    def ids: TypeInstances[EId]
  }

  val toTable: dahu.expr.Expr[Any] ==> ASTable = Arrow.lift((x: dahu.expr.Expr[Any]) => {
    val (headInt, tableInt) = Algebras.encodeAsPair(x)
    new ASTable {
      private val vec: Array[ResultF[EId]] = tableInt.asInstanceOf[Vector[Expr]].toArray

      override val root: EId = headInt.asInstanceOf[EId]

      override val arrow: EId ==> Expr = Arrow.lift(x => vec(EId.unwrap(x)))

      override def ids: TypeInstances[EId] = new TypeInstances[EId] {
        override val enumerate: Array[EId] = vec.indices.toArray.asInstanceOf[Array[EId]]
      }
    }
  })

  def extractType[X]: ResultF[X] ==> String = Arrow.lift(_.typ.toString)

  def types(ast: ASTable): ast.EId ==> String = ast.arrow.andThen(extractType)

  def evaluator(ast: ASTable)(inputs: ast.Variable ==> Value): ast.EId ==> Value = {
    def go(i: ast.EId): Value = {
      ast.arrow(i) match {
        case CstF(v, _) => Value( v )
        case x @ InputF(_, _) => inputs(x)
        case ComputationF(f, args, _) => Value( f.compute(args.map(go)) )
      }
    }
    Arrow.lift(go)
  }


  val bag = new BagPacking

  val ast: ASTable = toTable(bag.valid)

  println(ast)

  def inputsByName(ast: ASTable, map: Map[String, Any]): ast.Variable ==> Value =
    Arrow.lift[ast.Variable, Value](x => Value(  map.apply(x.name))  )

  val inputs = inputsByName(ast, Map("x1" -> false, "x2" -> true))

  val ev = evaluator(ast)(inputs)

  println(ev(ast.root))

  for(i <- ast.ids.enumerate) {
    println(s"$i : ${ev(i)}")
  }

  println("Done")

}
