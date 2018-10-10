package dahu.model

import cats.Id
import dahu.utils._
import dahu.model.functions.->:
import dahu.model.input._
import dahu.model.interpreter._
import dahu.model.ir._
import dahu.model.problem.API
import dahu.model.types.Tag._
import dahu.model.types.Value
import dahu.model.validation.Validation
import utest._

/** TODO: (pure)
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
          if(a == b) FEval(true)
          else FEval(false))
      }
    }

    "lambda-eval" - {
      case class App[A, B](lbd: Expr[A ->: B], in: A) {

        def evalsTo(expected: PEval[B]): Unit = {
          val regular = API.eval(lbd, _ => ???).apply(FEval(in))
          regular ==> expected
          val irMatch: PartialFunction[Any, Unit] = expected match {
            case PEmpty              => { case IR(_, FEval(false), _)                    => }
            case PConstraintViolated => { case IR(_, FEval(true), FEval(false))          => }
            case x                   => { case IR(y, FEval(true), FEval(true)) if x == y => }
          }
          val ev = API.evalTotal(lbd, _ => ???)
          val it = TotalValue[cats.Id, Value](Value(in), true, true)

          val ir = ev.smap(_.apply(FEval(it)))
          assertMatch(ir)(irMatch)
        }

        def evalsTo(b: B): Unit = evalsTo(FEval(b))
      }

      "lesser-than" - {
        val lt3: Expr[Int ->: Boolean] = Lambda(i => i < Cst(3))

        App(lt3, 3) evalsTo false
        App(lt3, 2) evalsTo true
      }
      "partial" - {
        val partial: Expr[Int ->: Boolean] = Lambda(i => i.subjectTo(_ =!= Cst(0)) > 5)
        "valid" - {
          App(partial, 1) evalsTo false
          App(partial, 5) evalsTo false
          App(partial, 6) evalsTo true
        }
        "invalid" - {
          App(partial, 0) evalsTo PConstraintViolated
        }
      }
      "optional" - {
        val optional: Expr[Int ->: Boolean] =
          Lambda(i => (Optional(i, i =!= 0): Expr[Int]) > Cst(5))
        "present" - {
          App(optional, 1) evalsTo false
          App(optional, 5) evalsTo false
          App(optional, 6) evalsTo true
        }
        "invalid" - {
          App(optional, 0) evalsTo PEmpty
        }
      }

    }
  }

}
  */
