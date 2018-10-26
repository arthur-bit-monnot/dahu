package dahu.lisp.compile

import utest._

import scala.util.Try

object CompileTests extends TestSuite {

  def eval(str: String): Any = {
    val env = Env.default()
    Try(parseEval(str, env)) match {
      case scala.util.Success(id) => env.extractValue(id)
      case scala.util.Failure(e)  => e.printStackTrace()
    }
  }

  def check(str: String, result: Any) = {
    val evaluated = eval(str)
    if(evaluated != result)
      print("") // to place a breakpoint
    assert(evaluated == result)
  }

  def gives(result: Any)(implicit x: utest.framework.TestPath): Unit = {
    val str = x.value.last
    check(str, result)
  }
  def fails()(implicit x: utest.framework.TestPath): Unit = {
    val str = x.value.last
    Try(parseEval(str, Env.default())) match {
      case scala.util.Success(_) => assert(false)
      case _                     => assert(true)
    }
  }

  override def tests = Tests {

    "'(1)" - gives(List(1))
    "1" - gives(1)
    "(+ 1 2)" - gives(3)
    "(* 2 4)" - gives(8)
    """'("a" "b")""" - gives(List("a", "b"))

//    """'("a", "b")""" - fails() // trailing comma

    "(1 2 3)" - fails()

    "large" - eval("""
        (do
          (defn empty [l] (if (= l '()) true false))
          (defn min [a b] (if (< a b) a b))
          (defn reduce [f l] (foldl f (first l) (rest l)))
          (defn sum [l] (foldl + 0 l))
          (defn forall [f l] (foldl and true (map f l)))
          (defn exists [f l] (foldl or false (map f l)))
          (defn check-eq [a b] (if (= a b) true (crash!! "Not equal" a b)))
          ;(defn map [f l]  (cons (f (first l))
          ;    (if (empty (rest l)) '() (map f (rest l)))))
          (defn inc [a] (+ a 1))
          (check-eq (map inc '(1 2)) '(2 3))
          (check-eq (sum '(1 2 3 4)) 10)
          (check-eq (reduce + '(1 2 3)) 6)
          (define l '(1 2 3))
          (check-eq (forall (fn [x] (> x 1)) l) false)
          (check-eq (forall (fn [x] (> x 0)) l) true)
          (check-eq (exists (fn [x] (> x 0)) l) true)
          (check-eq (exists (fn [x] (> x 4)) l) false)
          (check-eq (exists (fn [x] (> x 2)) l) true)
          (defn is-big [n] (> n 10))
          (check-eq (exists is-big l) false)
        )
      """)
  }
}
