package dahu.model.validation

import dahu.model.input.{Expr, Ident, TypedIdent}
import dahu.model.interpreter.{Interpreter, LambdaInterpreter}
import dahu.model.interpreter.LambdaInterpreter._
import dahu.model.math.{bool, int}
import dahu.model.problem.{API, LazyTree}
import dahu.model.problem.SatisfactionProblem.IR
import dahu.model.types.Tag._
import dahu.model.types._
import dahu.recursion.FAlgebra

import scala.collection.mutable
import scala.util.Random

object Validation {

  /** Checks that all possible methods to evaluate `e` give the same result and return it. */
  def checkedEval[A](e: Expr[A],
                     in: TypedIdent[Any] => Option[Value]): LambdaInterpreter.Result[Value] = {
    Evaluator.multiEvaluator.eval(e, in)(e)
  }
  def deepCheckedEval[A](e: Expr[A],
                         in: TypedIdent[Any] => Option[Value]): LambdaInterpreter.Result[Value] = {
    val evaluator = Evaluator.multiEvaluator.eval(e, in)
    Expr.dagInstance.topologicalOrderFromRoot(e).reverse.foreach { se =>
      println(se)
      evaluator(se)
    }
    evaluator(e)
  }

  /** Checks that all expression yield the same result. */
  def checkedMultiEval[A](es: Iterable[Expr[A]],
                          in: TypedIdent[Any] => Option[Value]): Result[Value] = {
    val evals = es.toList.map(e => checkedEval(e, in))
    assert(allEquals(evals))
    evals.head
  }

  /** Checks that all methods for evaluating `e`, return `res` */
  def assertEvaluatesTo[A](e: Expr[A], in: TypedIdent[Any] => Option[Any])(res: Result[A]): Unit = {
    val v = checkedEval(e, in.asInstanceOf[TypedIdent[Any] => Option[Value]])
    assert(v == res, s"$v != $res")
  }

  /** Checks that all methods for evaluating `e`, return `res` */
  def assertAllEvaluateTo[A](es: Iterable[Expr[A]], in: TypedIdent[Any] => Option[Any])(
      res: Result[A]): Unit = {
    val v = checkedMultiEval(es, in.asInstanceOf[TypedIdent[Any] => Option[Value]])
    assert(v == res, s"$v != $res")
  }

  def allEquals[A](as: List[A]): Boolean = as match {
    case Nil            => true
    case _ :: Nil       => true
    case a :: b :: tail => a == b && allEquals(b :: tail)
  }

  def fuzzedEvalCheck(expr: Expr[Any], numFuzz: Int = 10): Unit = {
    for(seed <- 1 to numFuzz) {
      val input = fuzzer(seed)
      checkedEval(expr, input)
    }
  }
  def deepFuzzedEvalCheck(expr: Expr[Any], numFuzz: Int = 10): Unit = {
    for(seed <- 1 to numFuzz) {
      val input = fuzzer(seed)
      deepCheckedEval(expr, input)
    }
  }

  def fuzzer(seed: Int): TypedIdent[Any] => Option[Value] = {
    val rand = new Random(seed)
    rand.nextInt(); rand.nextInt() // had some experience in the past were the first generations were not random
    val map = mutable.Map[Ident, Value]()

    tid =>
      {
        map.get(tid.id) match {
          case Some(v) => Some(v)
          case None =>
            val optV = tid.typ match {
              case Tag.ofBoolean => Some(Value(rand.nextBoolean()))
              case t: TagIsoInt[_] =>
                val intervalSize = t.max - t.min + 1
                val v = rand.nextInt(intervalSize) + t.min
                assert(t.min <= v && v <= t.max)
                Some(Value(v))
              case x =>
                dahu.utils.debug.warning(s"Fuzzer does not support this Tag: $x")
                None
            }
            optV.foreach(v => map.update(tid.id, v))
            optV
        }
      }
  }

  trait Evaluator {
    def eval(e: Expr[Any],
             in: TypedIdent[Any] => Option[Value]): Expr[Any] => LambdaInterpreter.Result[Value]
  }
  object Evaluator {

    val afterElimDyna = new Evaluator {
      override def eval(
          e: Expr[Any],
          in: TypedIdent[Any] => Option[Value]): Expr[Any] => LambdaInterpreter.Result[Value] = {
        val parsed = API.parse(e)
        val ev =
          API.eliminitateDynamics(parsed).tree.cata(LambdaInterpreter.partialEvalAlgebra2(in))
        (x: Expr[Any]) =>
          ev.get(x)
      }
    }
    val afterExpandLambdas = new Evaluator {
      override def eval(
          e: Expr[Any],
          in: TypedIdent[Any] => Option[Value]): Expr[Any] => LambdaInterpreter.Result[Value] = {
        val parsed = API.parse(e)
        val ev = API
          .expandLambdas(API.eliminitateDynamics(parsed))
          .tree
          .cata(LambdaInterpreter.partialEvalAlgebra2(in))
        x =>
          ev.get(x)
      }
    }
    val afterTotalEvaluator = new Evaluator {
      override def eval(
          e: Expr[Any],
          in: TypedIdent[Any] => Option[Value]): Expr[Any] => LambdaInterpreter.Result[Value] = {
        val ev = API
          .parseAndProcess(e)
          .tree
          .cata(Interpreter.evalAlgebra(in.andThen(_.get)))

        x =>
          ev.get(x) match {
            case IR(_, false, _) => LambdaInterpreter.Empty
            case IR(_, _, false) => LambdaInterpreter.ConstraintViolated
            case IR(v, _, _)     => LambdaInterpreter.Res(v)
          }
      }
    }
    val baseEvaluators = List(afterElimDyna, afterExpandLambdas, afterTotalEvaluator)
    val multiEvaluator = new Evaluator {
      override def eval(
          e: Expr[Any],
          in: TypedIdent[Any] => Option[Value]): Expr[Any] => LambdaInterpreter.Result[Value] = {
        val evaluators = baseEvaluators.map(ev => ev.eval(e, in))

        x =>
          {
            val evals = evaluators.map(f => f(x))
            assert(allEquals(evals), s"Not all evaluations are equal: $evaluators")
            evals.head
          }
      }
    }

  }

}
