package dahu.lisp.compile

import dahu.model.ir.{CstF, SequenceF}
import dahu.model.types.SequenceTag.SequenceTagImplAny
import dahu.utils._
import utest._

import scala.util.{Failure, Success, Try}

object CompileTests extends TestSuite {

  def eval(str: String): Any = {
    val ctx = new Context(Env.default())
    Try(parseEvalToString(str, ctx)) match {
      case scala.util.Success(id) => id
      case scala.util.Failure(e)  => e.printStackTrace()
    }
  }

  def check(str: String, result: Any = null) = {
    val ctx = new Context(Env.default())
    println(str)
    val evaluatedPrintable = ctx.show(str)
    println("  " + evaluatedPrintable)
    val evaluated = ctx.treeOf(str)
    evaluated match {
      case Success(t) => assert(result == null || t == result)
      case Failure(exception) =>
        exception.printStackTrace()
        assert(false)
    }
  }

  def gives(result: Any = null)(implicit x: utest.framework.TestPath): Unit = {
    val str = x.value.last
    check(str, result)
  }
  def fails()(implicit x: utest.framework.TestPath): Unit = {
    val str = x.value.last
    val ctx = new Context(Env.default())
    parseEvalToString(str, ctx) match {
      case scala.util.Success(_) => assert(false)
      case _                     => assert(true)
    }
  }

  val ? = null

  import dahu.model.types._
  def int(i: Int) = CstF(Value(i), Tag.ofInt)
  def double(d: Double) = CstF(Value(d), Tag.ofDouble)
  def list(as: Any*) = SequenceF[Any](as, SequenceTagImplAny(Tag.unsafe.ofAny))

  override def tests = Tests {

    "'(1)" - gives(?)
    "1" - gives(int(1))
    "(+ 1.0 2.)" - gives(double(3))
    "(* 2.0 4.0)" - gives(double(8))
    """'("a" "b")""" - gives(?) //list("a", "b")
    """'(1 2)""" - gives(?) //list(1, 2)

    "(defn inc [a] (+ a 1))" - gives(?)
    "(fn [a b] (* a b))" - gives(?)
    "((fn [a b] (* a b)) 1)" - gives(?)
    "((fn [a b] (* a b)) 1.0 2.0)" - gives(double(2))

//    """'("a", "b")""" - fails() // trailing comma

    "(1 2 3)" - fails()

    """
        (do (defn inc [a] (+ a 1))
            (fn [a b] (* (inc a) (inc b))))
      """ - gives(?)

    """
    (do
      (defstruct point
          ^int x
          ^int y
      )
      (point-y (point 1 2)))
    """ - gives(int(2))

//    "large" - eval("""
//        (do
//          (defn empty [l] (if (= l '()) true false))
//          (defn min [a b] (if (< a b) a b))
//          (defn reduce [f l] (foldl f (first l) (rest l)))
//          (defn sum [l] (foldl + 0 l))
//          (defn forall [f l] (foldl and true (map f l)))
//          (defn exists [f l] (foldl or false (map f l)))
//          (defn check-eq [a b] (if (= a b) true (crash!! "Not equal" a b)))
//          ;(defn map [f l]  (cons (f (first l))
//          ;    (if (empty (rest l)) '() (map f (rest l)))))
//          (defn inc [a] (+ a 1))
//          (check-eq (map inc '(1 2)) '(2 3))
//          (check-eq (sum '(1 2 3 4)) 10)
//          (check-eq (reduce + '(1 2 3)) 6)
//          (define l '(1 2 3))
//          (check-eq (forall (fn [x] (> x 1)) l) false)
//          (check-eq (forall (fn [x] (> x 0)) l) true)
//          (check-eq (exists (fn [x] (> x 0)) l) true)
//          (check-eq (exists (fn [x] (> x 4)) l) false)
//          (check-eq (exists (fn [x] (> x 2)) l) true)
//          (defn is-big [n] (> n 10))
//          (check-eq (exists is-big l) false)
//        )
//      """)
  }
}
