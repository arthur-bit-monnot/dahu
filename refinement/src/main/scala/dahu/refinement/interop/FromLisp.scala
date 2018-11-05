package dahu.refinement.interop
import dahu.graphs.transformations.Transformation
import dahu.lisp.compile.Env._
import dahu.lisp.compile._
import dahu.model.functions.{Fun2, FunAny}
import dahu.model.input.{Ident, Scope, TypedIdent}
import dahu.model.ir.{CstF, ExprF, InputF, ProductF}
import dahu.model.math._
import dahu.model.products.{Field, ProductTagAny}
import dahu.model.types._
import dahu.refinement.{MemImpl, RMemory}
import dahu.utils._

object FromLisp extends App {

  val mem = new MemImpl()
  implicit val memType = Tag.default[RMemory]

  val scalarize = new Transformation[ExprF, ExprF] {
    override def transformation[I <: Int](retrieve: I => ExprF[I],
                                          record: ExprF[I] => I): ExprF[I] => ExprF[I] = {
      case InputF(id, tpe: ProductTagAny) =>
        val fields = tpe.fields.map {
          case Field(name, tpe, pos) =>
            val fieldId = TypedIdent(id.id.subIdent(name), tpe)
            record(InputF(fieldId, tpe))
        }
        ProductF(fields, tpe)

      case x => x
    }
  }

  object ReadDouble extends Fun2[RMemory, String, Double] {
    override def of(in1: RMemory, in2: String): Double = in1.read(in1.addressOf(name))
    override def name: String = "read-double"
  }

  def default(): RootEnv = {
    val e = new RootEnv()

    def recVal(name: String, value: V): Unit = {
      val i = e.getId(value)
      e.setConstantValue(name, i)
    }
    def rec(name: String, f: FunAny): Unit = {
      val i = e.getId(CstF(Value(f), Tag.unsafe.ofAny))
      e.setConstantValue(name, i)
    }
    def recType(name: String, f: TagAny): Unit = {
      require(name.startsWith("^"))
      val i = e.getId(CstF(Value(f), Tag.unsafe.ofAny))
      e.setConstantValue(name, i)
    }
    rec("and", bool.And)
    rec("or", bool.Or)
    rec("not", bool.Not)
    rec("i+", int.Add)
    rec("+", double.Add)
    rec("i*", int.Times)
    rec("*", double.Times)
    rec("/", double.Div)
    rec("<", double.LT)
    rec("<=", double.LEQ)
    rec("neg", double.Negate)
    rec("abs", double.Abs)
    rec("imin", int.Min)
    rec("min", double.Min)
    rec("=", any.EQ[Any])

    rec("read", ReadDouble)

    recVal("true", bool.TrueF)
    recVal("false", bool.FalseF)

    recVal("mem!", InputF(TypedIdent(Ident(Scope.root, "_mem_"), memType), memType))

    recType("^int", Tag.ofInt)
    recType("^real", Tag.ofDouble)
    recType("^bool", Tag.ofBoolean)
    recType("^str", Tag.ofString)
    e
  }

  val env = default()
  implicit val ctx = new Context(env, Some(scalarize))

//  evalFile("refinement/domains/prelude.clj")

  dahu.lisp.compile
    .evalMany("""
    (defn diff [a b] (abs (+ a (neg b))))
    (defvar ^real x )
    (defvar ^real y )

    (defstruct point ^real x ^real y)

    (defvar ^point pt1 )
    (defvar ^point pt2 )

    (defn dist [pt1 pt2] (+ (diff (:x pt1) (:x pt2)) (diff (:y pt1) (:y pt2))))
    (define p (point x y))

    (dist p)

    (< (dist pt1 pt2) 10.0)
  """)

  ctx.graph

}
