package dahu.model.input

import dahu.model.interpreter.{FEval, Interpreter}
import dahu.utils._
import dahu.utils.debug._
import dahu.utils.errors._
import dahu.model.types._
import dahu.model.input.dsl._
import dahu.model.interpreter.Interpreter.{ConstraintViolated, Res}
import dahu.model.problem.API
import dahu.model.problem.SatisfactionProblem.IR
import dahu.model.products.FieldAccess
import utest._

object Products extends TestSuite {

  def liftNameMap(m: Any => Value): TypedIdent[Any] => Value = {
    case TypedIdent(Ident(s), _) => m(s)
  }

  def tests = Tests {

    "interval" - {

      case class Interval[F[_]](start: F[Int], end: F[Int])
      object Interval {
        val Start = FieldAccess[Interval, Int]("start", 0)
        val End = FieldAccess[Interval, Int]("end", 1)
      }

      "product-tag" - {
        implicitly[ProductTag[Interval]].ignoreResult
      }

      val i = Interval[Expr](Input[Int]("s"), Input[Int]("e"))
      val prod = Product(i)

      "eval (<)" - {
        val constrained = prod.subjectTo(x => Interval.Start(x) < Interval.End(x))
        val ast = dahu.model.compiler.Algebras.parse(constrained)
        Interpreter.eval(ast)(_ => Value(0)) ==> None
        Interpreter.eval(ast)(_ => Value(1)) ==> None

        API.eval(constrained, _ => Value(0)) ==> ConstraintViolated
        API.eval(constrained, _ => Value(1)) ==> ConstraintViolated

        val nameMaps: Any => Value = {
          case "s" => Value(0)
          case "e" => Value(1)
          case _   => unexpected
        }

        val inputs: TypedIdent[Any] => Value = liftNameMap(nameMaps)

        Interpreter.eval(ast)((vid: ast.VID) => inputs(ast.variables(vid).id)) ==> Some(
          Interval[cats.Id](0, 1))

        API.eval(constrained, inputs) ==> Res(Interval[cats.Id](0, 1))
        API.evalTotal(constrained, inputs) ==> IR(Interval[cats.Id](0, 1), true, true)
          .smap(FEval(_))
      }
      "eval (<=)" - {
        val constrained = prod.subjectTo(x => Interval.Start(x) <= Interval.End(x))
        val ast = dahu.model.compiler.Algebras.parse(constrained)
        Interpreter.eval(ast)(_ => Value(0)) ==> Some(Interval[cats.Id](0, 0))
        Interpreter.eval(ast)(_ => Value(1)) ==> Some(Interval[cats.Id](1, 1))

        val inputs: ast.VID => Value = x =>
          liftNameMap({
            case "s" => Value(0)
            case "e" => Value(1)
            case _   => unexpected
          })(ast.variables(x).id)
        Interpreter.eval(ast)(inputs) ==> Some(Interval[cats.Id](0, 1))
      }
    }
  }
}
