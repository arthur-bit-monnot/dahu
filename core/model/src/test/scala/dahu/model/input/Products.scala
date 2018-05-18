package dahu.model.input

import dahu.model.interpreter.Interpreter
import dahu.utils.debug._
import dahu.utils.errors._
import dahu.model.types._
import dahu.model.input.dsl._
import utest._

object Products extends TestSuite {

  def tests = Tests {

    "interval" - {

      case class Interval[F[_]](start: F[Int], end: F[Int])

      "product-tag" - {
        implicitly[ProductTag[Interval]].ignoreResult
      }

      val i = Interval[Expr](Input[Int]("s"), Input[Int]("e"))
      val prod = Product(i)

      "eval (<)" - {
        val constrained = prod.subjectTo(x => x.value.start < x.value.end)
        val ast = dahu.model.compiler.Algebras.parse(constrained)
        Interpreter.eval(ast)(_ => Value(0)) ==> None
        Interpreter.eval(ast)(_ => Value(1)) ==> None

        val inputs: ast.VID => Value = x =>
          ast.variables(x).id match {
            case Ident("s") => Value(0)
            case Ident("e") => Value(1)
            case _          => unexpected
        }
        Interpreter.eval(ast)(inputs) ==> Some(Interval[cats.Id](0, 1))
      }
      "eval (<=)" - {
        val constrained = prod.subjectTo(x => x.value.start <= x.value.end)
        val ast = dahu.model.compiler.Algebras.parse(constrained)
        Interpreter.eval(ast)(_ => Value(0)) ==> Some(Interval[cats.Id](0, 0))
        Interpreter.eval(ast)(_ => Value(1)) ==> Some(Interval[cats.Id](1, 1))

        val inputs: ast.VID => Value = x =>
          ast.variables(x).id match {
            case Ident("s") => Value(0)
            case Ident("e") => Value(1)
            case _          => unexpected
        }
        Interpreter.eval(ast)(inputs) ==> Some(Interval[cats.Id](0, 1))
      }
    }
  }
}
