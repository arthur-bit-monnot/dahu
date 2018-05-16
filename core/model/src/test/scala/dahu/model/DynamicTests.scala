package dahu.model

import dahu.model.input.{Ident, SubjectTo}
import dahu.model.ir._
import dahu.model.math.bool
import dahu.model.types._
import dahu.recursion._
import dahu.utils.Vec
import utest._

object DynamicTests extends TestSuite {

  type E = Fix[ExprF]
  implicit def unfix2fix(e: ExprF[Fix[ExprF]]): Fix[ExprF] = e

  val True: E = CstF[E](Value(true), typeOf[Boolean])
  val x: E = CstF[E](Value(1), typeOf[Int])
  val y: E = CstF[E](Value(2), typeOf[Int])
  val dec: E = InputF[E](Ident("decision"), typeOf[Int])

  val xProvider: E = DynamicProviderF(True, x, typeOf[Boolean])
  val yProvider: E = DynamicProviderF(True, y, typeOf[Boolean])
  val inProvided: E = DynamicF(Vec(dec), null, typeOf[Boolean])

  val result: E =
    Partial(dec,
            ComputationF(bool.And, Vec(inProvided, xProvider, yProvider), typeOf[Boolean]): E,
            typeOf[Int])

  def collectProvided[F](e: F, coalg: F => ExprF[F]): Set[ExprF[F]] = ???
  def closeWorld[F](e: ExprF[F], coalg: F => ExprF[F]): (F, F => StaticF[F]) = {
    // remove providers && instantiate dynamics
    ???

  }

  def tests = Tests {
    "e" - {
      assert(true)
    }
  }
}
