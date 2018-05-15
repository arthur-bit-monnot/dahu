package dahu.model

import dahu.model.input.{Ident, SubjectTo}
import dahu.model.ir._
import dahu.model.math.bool
import dahu.model.types._
import dahu.utils.Vec
import utest._

object DynamicTests extends TestSuite {

  val True = CstF(Value(true), typeOf[Boolean])
  val x = CstF(Value(1), typeOf[Int])
  val y = CstF(Value(2), typeOf[Int])
  val dec = InputF(Ident("decision"), typeOf[Int])

  val xProvider = DynamicProviderF(True, x, typeOf[Boolean])
  val yProvider = DynamicProviderF(True, y, typeOf[Boolean])
  val inProvided = DynamicF(Vec(dec), null, typeOf[Boolean])

  val result =
    Partial(dec,
            ComputationF(bool.And, Vec(inProvided, xProvider, yProvider), typeOf[Boolean]),
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
