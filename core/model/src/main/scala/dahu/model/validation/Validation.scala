package dahu.model.validation

import dahu.model.input.{Expr, Ident}
import dahu.model.interpreter.{Interpreter, LambdaInterpreter}
import dahu.model.interpreter.LambdaInterpreter._
import dahu.model.math.{bool, int}
import dahu.model.problem.{API, LazyTree}
import dahu.model.problem.SatisfactionProblem.IR
import dahu.model.types.Tag._
import dahu.model.types._
import dahu.recursion.FAlgebra

object Validation {

  /** Checks that all possible methods to evaluate `e` give the same result and return it. */
  def checkedEval[A](e: Expr[A], in: Ident => Option[Value]): LambdaInterpreter.Result[Value] = {
    Evaluator.multiEvaluator.eval(e, in)
  }

  /** Checks that all expression yield the same result. */
  def checkedMultiEval[A](es: Iterable[Expr[A]], in: Ident => Option[Value]): Result[Value] = {
    val evals = es.toList.map(e => checkedEval(e, in))
    assert(allEquals(evals))
    evals.head
  }

  /** Checks that all methods for evaluating `e`, return `res` */
  def assertEvaluatesTo[A](e: Expr[A], in: Ident => Option[Any])(res: Result[A]): Unit = {
    val v = checkedEval(e, in.asInstanceOf[Ident => Option[Value]])
    assert(v == res, s"$v != $res")
  }

  /** Checks that all methods for evaluating `e`, return `res` */
  def assertAllEvaluateTo[A](es: Iterable[Expr[A]], in: Ident => Option[Any])(
      res: Result[A]): Unit = {
    val v = checkedMultiEval(es, in.asInstanceOf[Ident => Option[Value]])
    assert(v == res, s"$v != $res")
  }

  def allEquals[A](as: List[A]): Boolean = as match {
    case Nil            => true
    case _ :: Nil       => true
    case a :: b :: tail => a == b && allEquals(b :: tail)
  }

  trait Evaluator {
    def eval(e: Expr[Any], in: Ident => Option[Value]): LambdaInterpreter.Result[Value]
  }
  object Evaluator {

    val afterElimDyna = new Evaluator {
      override def eval(e: Expr[Any],
                        in: Ident => Option[Value]): LambdaInterpreter.Result[Value] = {
        val parsed = API.parse(e)
        API.eliminitateDynamics(parsed).eval(LambdaInterpreter.partialEvalAlgebra2(in))
      }
    }
    val afterExpandLambdas = new Evaluator {
      override def eval(e: Expr[Any],
                        in: Ident => Option[Value]): LambdaInterpreter.Result[Value] = {
        val parsed = API.parse(e)
        API
          .expandLambdas(API.eliminitateDynamics(parsed))
          .eval(LambdaInterpreter.partialEvalAlgebra2(in))
      }
    }
    val afterTotalEvaluator = new Evaluator {
      override def eval(e: Expr[Any],
                        in: Ident => Option[Value]): LambdaInterpreter.Result[Value] = {
        API
          .parseAndProcess(e)
          .eval(Interpreter.evalAlgebra(in.andThen(_.get))) match {
          case IR(_, false, _) => LambdaInterpreter.Empty
          case IR(_, _, false) => LambdaInterpreter.ConstraintViolated
          case IR(v, _, _)     => LambdaInterpreter.Res(v)
        }
      }
    }
    val baseEvaluators = List(afterElimDyna, afterExpandLambdas, afterTotalEvaluator)
    val multiEvaluator = new Evaluator {
      override def eval(e: Expr[Any],
                        in: Ident => Option[Value]): LambdaInterpreter.Result[Value] = {
        val evaluations = baseEvaluators.map(ev => ev.eval(e, in))
        assert(allEquals(evaluations), s"Not all evaluations are equal: $evaluations")
        evaluations.head
      }
    }

  }

}
