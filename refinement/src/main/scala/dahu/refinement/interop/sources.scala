package dahu.refinement.interop

import dahu.lisp.compile._
import dahu.lisp.compile.Env.V
import dahu.model.functions.FunAny
import dahu.model.input.{Ident, Scope, TypedIdent}
import dahu.model.ir._
import dahu.model.math._
import dahu.model.problem.ExtractInputToLambda
import dahu.model.types._
import dahu.refinement.interop.transformations.{ForAll, ForFirst, ForLast}

object sources {

  val prelude =
    """
(defn get-field [field-name prod] ((extractor field-name (type-of prod)) prod))
(defn pow2 [base] (pow base 2.0))
(defn - [a b] (+ a (neg b)))
(defn i- [a b] (i+ a (ineg b)))
(defn max [a b] (neg (min (neg a) (neg b))))
(defn > [a b] (< b a))
(defn >= [a b] (<= b a))
(defn diff [a b] (abs (+ a (neg b))))

(defn forall [l f] (fold-and (map f l)))

(defn implies [a b] (or (not a) b))

(defn geom.euclidian-dist [x1 y1 x2 y2] (sqrt (+ (pow2 (- x2 x1))
                                              (pow2 (- y2 y1)))))

(defn geom.is-in [shape pt] (<= (geom.signed-dist shape (:x pt) (:y pt)) 0.0))
(defn geom.is-not-in [shape pt] (> (geom.signed-dist shape (:x pt) (:y pt)) 0.0))



(defstruct read-disc ^int dstate ^string field)
(defstruct read-cont ^int state ^string field)

(define dt (read-cont current-continuous-state "dt"))

(defn previous [x] (read-cont (i- (read-cont.state x) 1i) (read-cont.field x)))
(defn next [x] (read-cont (i+ (read-cont.state x) 1i) (read-cont.field x)))

(defn fst-deriv [v] (/ (- (v) (previous v)) dt))
(defn snd-deriv [v]
  (* 2 (/ (- (fst-deriv (next v)) (fst-deriv v)) (+ (next dt) dt))))

"""

  val testDomain = """
    (define c1 (geom.circle -0.0 -0.0 1.0))
    (define c2 (geom.circle 10.0 10.0 1.0))
    (define c3 (geom.circle 0.0 0.0 0.5))
    (define r1 (geom.rectangle -0.5 -0.5 1.0 1.0))
    (define r2 (geom.rectangle 10.9 10.0 1.0 1.0))

    (defvar ^real x )
    (defvar ^real y )

    (defstruct point ^real x ^real y)

    (defvar ^point pt1 )
    (defvar ^point pt2 )
    (defvar ^point pt3 )

    (defvar ^points path)
  """

  def default(): RootEnv = {
    implicit val anyTag: Tag[Any] = Tag.unsafe.ofAny
    val e = new RootEnv()

    def recVal(name: String, value: V): Unit = {
      val i = e.getId(value)
      e.setConstantValue(name, i)
    }
    def rec(name: String, f: FunAny): Unit = {
      val i = e.getId(CstF(Value(f), f.funType))
      e.setConstantValue(name, i)
    }
    def recType(name: String, f: TagAny): Unit = {
      require(name.startsWith("^"))
      val i = e.getId(CstF(Value(f), Tag.ofType))
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
    rec("ineg", int.Negate)
    rec("abs", double.Abs)
    rec("sqrt", double.SQRT)
    rec("pow", double.POW)
    rec("imin", int.Min)
    rec("min", double.Min)

    rec("fold-and", sequence.Fold(bool.And)) // todo: we need to reify fold's param

    rec("type-of", any.TypeOf)
    rec("extractor", products.Extractor)

    rec("geom.circle", geom.Circle)
    rec("geom.rectangle", geom.Rectangle)
    //    rec("point", geom.Point)
    rec("geom.polygon", geom.Polygon)
    rec("geom.signed-dist", geom.SignedDist)

    rec("list", new sequence.ListBuilder[Any]()(Tag.unsafe.ofAny) {})
    rec("first", sequence.First[Any])
    rec("last", sequence.Last[Any])
    rec("map", sequence.Map[Any, Any])
    rec("seq.indices", sequence.Indices)
    rec("seq.get", sequence.Get[Any])
    rec("forall-consecutive", sequence.AllConsecutive[Any])
    rec("map-consecutive2", sequence.MapConsecutive2[Any, Any])
    rec("map-consecutive3", sequence.MapConsecutive3[Any, Any])

    rec("meta.as-lambda", ExtractInputToLambda)

    rec("ref.forall", ForAll)
    rec("ref.forfirst", ForFirst)
    rec("ref.forlast", ForLast)

    rec("=", any.EQ[Any])

    recVal("true", bool.TrueF)
    recVal("false", bool.FalseF)
    recVal(CURRENT_CONTINUOUS_STATE, CstF(Value(0), Tag.ofInt))

    recVal("mem!",
           InputF(TypedIdent(Ident(Scope.root, "_mem_"), evaluation.memType), evaluation.memType))

    recType("^int", Tag.ofInt)
    recType("^real", Tag.ofDouble)
    recType("^bool", Tag.ofBoolean)
    recType("^string", Tag.ofString)
    e
  }

}
