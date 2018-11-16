package dahu.model.input

import cats.Id
import dahu.model.interpreter.{FEval, Interpreter, PConstraintViolated}
import dahu.utils._
import dahu.utils.debug._
import dahu.utils.errors._
import dahu.model.types._
import dahu.model.input.dsl._
import dahu.model.products.{Field, FieldAccess, GenConstructor, ProductTag}
import utest._

object Products extends TestSuite {

  def liftNameMap(m: Any => Value): TypedIdent => Value = {
    case TypedIdent(Ident(_, s), _) => m(s)
  }

  def tests = Tests {

    "interval" - {

      case class Interval[F[_]](start: F[Int], end: F[Int]) {
        private def this(arr: Array[Any]) =
          this(arr(0).asInstanceOf[F[Int]], arr(1).asInstanceOf[F[Int]])
      }
      object Interval {
        implicit val tag: ProductTag[Interval] =
          ProductTag.build[Interval]("interval", "start" -> Tag.ofInt, "end" -> Tag.ofInt)

        val Start = tag.getAccessor[Int]("start")
        val End = tag.getAccessor[Int]("end")
      }

      "product-tag" - {
        implicitly[ProductTag[Interval]].ignoreResult
      }

      val i = Interval[Expr](Input[Int]("s", Scope.root), Input[Int]("e", Scope.root))
      val prod = Product(i)

      "eval (<)" - {
        val constrained = prod
        val ast = dahu.model.compiler.Algebras.parse(constrained)
        Interpreter.eval(ast)(_ => Value(0)) ==> None
        Interpreter.eval(ast)(_ => Value(1)) ==> None

        // TODO: we miss evaluation at the API level
//        API.eval(constrained, _ => Value(0)) ==> PConstraintViolated
//        API.eval(constrained, _ => Value(1)) ==> PConstraintViolated

        val nameMaps: Any => Value = {
          case "s" => Value(0)
          case "e" => Value(1)
          case _   => unexpected
        }

        val inputs: TypedIdent => Value = liftNameMap(nameMaps)

        Interpreter.eval(ast)((vid: ast.VID) => inputs(ast.variables(vid).id)) ==> Some(
          Interval[cats.Id](0, 1))

        // TODO: add eval to API
//        API.eval(constrained, inputs) ==> FEval(Interval[cats.Id](0, 1))
//        API.evalTotal(constrained, inputs) ==> IR(Interval[cats.Id](0, 1), true, true)
//          .smap(FEval(_))
      }
      "eval (<=)" - {
        val constrained = prod
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
