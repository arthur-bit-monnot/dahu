package dahu.model

import cats.Id
import dahu.model.compiler.Algebras
import dahu.model.functions.->:
import dahu.model.input._
import dahu.model.interpreter.{Interpreter, LambdaInterpreter}
import dahu.model.ir._
import dahu.model.math.{bool, int}
import dahu.model.problem.{API, LazyTree}
import dahu.model.problem.SatisfactionProblem.IR
import dahu.model.types.Tag._
import dahu.model.types._
import dahu.model.validation.Validation
import dahu.recursion.FAlgebra
import utest._

object LambdaTests extends TestSuite {
  import dsl._

  def tests = Tests {
    "equal-lambdas" - {

      def equivalentExprs(a: Int, b: Int): List[Expr[Boolean]] = {
        List(
          Cst(a) === Cst(b),
          Lambda[Int, Boolean](j => Cst(a) === j).apply(Cst(b)),
          Lambda[Int, Int ->: Boolean](i => Lambda[Int, Boolean](j => i === j))
            .apply(Cst(a), Cst(b))
        )
      }

      val t1: Expr[Any] => LazyTree[Expr[Any], StaticF, Id, _] = e => {
        val parsed = API.parse(e)
        API.eliminitateDynamics(parsed)
      }

      for(a <- 0 to 2; b <- 0 to 2) {
        val es = equivalentExprs(a, b)
        Validation.assertAllEvaluateTo(es, _ => None)(
          if(a == b) LambdaInterpreter.Res(true)
          else LambdaInterpreter.Res(false))
      }
    }
  }

}
