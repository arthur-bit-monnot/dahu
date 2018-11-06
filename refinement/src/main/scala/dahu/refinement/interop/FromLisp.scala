package dahu.refinement.interop

import cats._
import cats.implicits._
import com.vividsolutions.jts.geom.Geometry
import dahu.graphs.{IDTop, OpenASG}
import dahu.graphs.transformations.Transformation
import dahu.lisp.compile.Env._
import dahu.lisp.compile._
import dahu.model.compiler.Algebras
import dahu.model.functions.{Fun2, FunAny}
import dahu.model.input.Lambda.LambdaIdent
import dahu.model.input.{Ident, Scope, TypedIdent}
import dahu.model.ir._
import dahu.model.math._
import dahu.model.products.{Field, ProductTagAny}
import dahu.model.types._
import dahu.recursion.FAlgebra
import dahu.refinement.common.{Params, R, Values}
import dahu.refinement.la.LeastSquares
import dahu.refinement.{MemImpl, RMemory, RefExpr}
import dahu.utils._

import scala.util.Try

object FromLisp extends App {

  val epsilon = 1e-8

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

  val asErrors = new Transformation[ExprF, ExprF] {
    override def transformation[I <: Int](retrieve: I => ExprF[I],
                                          record: ExprF[I] => I): ExprF[I] => ExprF[I] = {
      case ComputationF(bool.And, args, _) =>
        ComputationF(double.Add, args)
      case ComputationF(double.LEQ, Vec(a, b), _) =>
        val bMinusA = record(
          ComputationF(double.Add, Vec(b, record(ComputationF(double.Negate, Vec(a))))))
        ComputationF(double.Min, record(CstF(Value(0.0), Tag.ofDouble)), bMinusA)
      case ComputationF(double.LT, Vec(a, b), _) =>
        val bMinusA = record(
          ComputationF(double.Add,
                       b,
                       record(ComputationF(double.Negate, Vec(a))),
                       record(CstF(Value(-epsilon), Tag.ofDouble)))
        )
        ComputationF(double.Min, record(CstF(Value(0.0), Tag.ofDouble)), bMinusA)
      case ComputationF(bool.Or, _, _)  => ???
      case ComputationF(bool.Not, _, _) => ???
      case x                            => x
    }
  }

//  object ReadDouble extends Fun2[RMemory, String, Double] {
//    override def of(in1: RMemory, in2: String): Double = in1.read(in1.addressOf(name))
//    override def name: String = "read-double"
//  }

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
    rec("sqrt", double.SQRT)
    rec("pow", double.POW)
    rec("imin", int.Min)
    rec("min", double.Min)

    rec("circle", geom.Circle)
    rec("rectangle", geom.Rectangle)
//    rec("point", geom.Point)
    rec("polygon", geom.Polygon)
    rec("signed-dist", geom.SignedDist)

    rec("list", new sequence.ListBuilder[Any]()(Tag.unsafe.ofAny) {})

    rec("=", any.EQ[Any])

//    rec("read", ReadDouble)

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
    .evalMany(
      """
    (defn pow2 [base] (pow base 2.0))
    (defn - [a b] (+ a (neg b)))
(defn max [a b] (neg (min (neg a) (neg b))))
(defn > [a b] (< b a))
    (defn diff [a b] (abs (+ a (neg b))))
   ; (defstruct circle ^real x ^real y ^real rad)

    (define c1 (circle -0.0 -0.0 1.0))
    (define c2 (circle 10.0 10.0 1.0))
    (define c3 (circle 0.0 0.0 0.5))
    (define r1 (rectangle -0.5 -0.5 1.0 1.0))

    (defn euclidian-dist [x1 y1 x2 y2] (sqrt (+ (pow2 (- x2 x1))
                                                (pow2 (- y2 y1)))))



    ;(defn in-circle [pt c] (<= (signed-dist (:x pt) (:y pt) (:x c) (:y c)) (:rad c)))
    ;(defn not-in-circle [pt c] (> (euclidian-dist (:x pt) (:y pt) (:x c) (:y c)) (:rad c)))

    (defn is-in [shape pt] (<= (signed-dist shape (:x pt) (:y pt)) 0.0))
    (defn is-not-in [shape pt] (> (signed-dist shape (:x pt) (:y pt)) 0.0))

    (defvar ^real x )
    (defvar ^real y )

    (defstruct point ^real x ^real y)

    (defvar ^point pt1 )
    (defvar ^point pt2 )
    (defvar ^point pt3 )

    ;(defn dist [pt1 pt2] (+ (diff (:x pt1) (:x pt2)) (diff (:y pt1) (:y pt2))))
    ;(defn dist [pt1 pt2] (diff (:x pt1) (:x pt2)))
    ;(define p (point x y))

    ;(dist p)

    ;(< (dist pt1 pt2) 10.0)
  """)

  val mem = new MemImpl()

  type Env = EvalEnv

  class EvalEnv(binds: Map[LambdaIdent, Value] = Map(), stack: List[Value] = Nil) {
    def get(id: LambdaIdent): Value = binds(id)
    def push(value: Value): EvalEnv = new EvalEnv(binds, value :: stack)
    def bind(lambdaIdent: LambdaIdent): EvalEnv = {
      stack match {
        case head :: tail => new EvalEnv(binds.updated(lambdaIdent, head), tail)
        case Nil          => ???
      }
    }
  }

  def eval[I](coalg: I => ExprF[I], valueOf: TypedIdent => Value)(e: Env)(i: I): Value = {
    def ev(i: I, e: Env): Value = eval(coalg, valueOf)(e)(i)

    coalg(i) match {
      case InputF(id, _) =>
//        println(s"$id: ${valueOf(id)}")
        valueOf(id)
      case CstF(v, _) => v
      case ComputationF(f, args, _) =>
        Value(f.compute(args.smap(ev(_, e))))
      case SequenceF(members, _) => Value(members.smap(ev(_, e)))
      case NoopF(x, _)           => ev(x, e)
      case ITEF(cond, onTrue, onFalse, _) =>
        ev(cond, e) match {
          case true  => ev(onTrue, e)
          case false => ev(onFalse, e)
          case _     => ???
        }
      case ApplyF(lbd, param, _) =>
        val arg = ev(param, e)
        val subEnv = e.push(arg)
        ev(lbd, subEnv)
      case LambdaF(_, tree, id, _) =>
        val subEnv = e.bind(id)
        ev(tree, subEnv)
      case LambdaParamF(id, _) =>
        e.get(id)

      case ProductF(ms, tpe) =>
        val members = ms.map(ev(_, e))
        Value(tpe.buildFromValues(members))

    }

  }

  val _withParser = ctx.optGraph.fixID.flatPrepro[String](str => Try(dahu.lisp.parse(str)))

  val asg = _withParser.transform(asErrors).fixID

  def asRefExpr[X, I <: IDTop, Opt[_]: Functor](x: X,
                                                asg: OpenASG[X, ExprF, Opt, I]): Opt[RefExpr] = {
    asg
      .getTreeRoot(x)
      .map({ i =>
        val desc = asg.descendants(i).collect {
          case (_, InputF(x, _)) =>
//            println(mem.addressOf(x))
            x
        }
//        println(desc)
        val variableToArrayIndex = desc.zipWithIndex.toMap

        def valueOf(id: TypedIdent)(mem: Values): Value = {
//          println(s"addr: ${variableToArrayIndex(id)}")
          val i = variableToArrayIndex(id)
          Value(mem(i))
        }

        def evaluator(mem: Values): Value = {
          eval(asg.internalCoalgebra, valueOf(_)(mem))(new EvalEnv())(i)
        }

        val f = new dahu.refinement.Fun {
          override def numParams: Int = variableToArrayIndex.size
          override def eval(params: Values): R =
            evaluator(params).asInstanceOf[R]
        }
        f.bind(desc.map(mem.addressOf): _*)
      })

  }

  def refexpr(str: String): Try[RefExpr] = {
    val tmp = asRefExpr(str, asg)
//    println(tmp)
    tmp
  }

  val x = mem
  x.print()

  implicit class RefExprHelper(val sc: StringContext) extends AnyVal {
    def str: String = sc.parts.mkString("")
    def e(args: Any*): Try[RefExpr] = refexpr(sc.parts.mkString(""))

    def eval2(args: Any*): Try[Value] =
      asg.getTreeRoot(str).map { i =>
        eval(asg.internalCoalgebra, (id: TypedIdent) => Value(x.read(x.addressOf(id))))(
          new EvalEnv())(i)
      }

  }
  val es: Try[List[RefExpr]] = List(
//    e"(diff (dist pt1 pt2) 1.0)",
//    e"(diff (dist pt1 pt3) 2.0)",
//    e"(dist pt1 (point 0.0 0.0))",
//    e"(diff (dist pt2 pt3) 1.0)",
    e"(is-in c1 pt1)",
    e"(is-in c1 pt2)",
    e"(is-in c2 pt3)",
    e"(is-not-in r1 pt1)",
  ).sequence

  implicit val memory = x

  val ls = new LeastSquares(es.get)
  ls.lmIteration(x, 10)

  def print(str: String)(implicit mem: RMemory): Unit =
    asg.getTreeRoot(str).map { i =>
      eval(asg.internalCoalgebra, (id: TypedIdent) => Value(mem.read(mem.addressOf(id))))(
        new EvalEnv())(i)
    } match {
      case scala.util.Success(value) => println(s"$str -> $value")
      case scala.util.Failure(e)     => e.printStackTrace()
    }

  print("pt1")
  print("pt2")
  print("pt3")

  print("(list pt1 pt2 pt3)")

  println("-----")
  x.print()

  type G = Geometry
}
