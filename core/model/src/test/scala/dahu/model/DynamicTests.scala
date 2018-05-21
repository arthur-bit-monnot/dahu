package dahu.model

import dahu.model.compiler.Algebras
import dahu.model.functions.->
import dahu.model.input._
import dahu.model.ir._
import dahu.model.math.{bool, int}
import dahu.model.problem.{IDTop, LazyTree, StaticProblem}
import dahu.model.types._
import dahu.model.types.Tag._
import dahu.recursion._
import dahu.utils.{SubSubInt, Vec}
import utest._

object DynamicTests extends TestSuite {
  import dsl._

  type E = Fix[ExprF]

  val equal: Expr[Int -> (Int -> Boolean)] =
    Lambda[Int, Int -> Boolean](i => Lambda[Int, Boolean](j => i === j))

  val True = Cst(true)
  val x = Cst(1)
  val y = Cst(2)
  val dec = Input[Int]("decision")

  val xProvider = DynamicProvider(True, x)
  val yProvider = DynamicProvider(True, y)
  val inProvided = Dynamic[Int, Int, Boolean](dec, equal, bool.Or)

  val result =
//    Computation(bool.And, Seq(inProvided, xProvider, yProvider))
    SubjectTo(dec, Computation(bool.And, Seq(inProvided, xProvider, yProvider)))

  sealed trait Marker
  val forest = LazyTree.parse(result, Algebras.coalgebra).fixID
  val root = forest.getTreeRoot(result)
//  println(forest.build(root))
  val staticTree = StaticProblem.underClosedWorld(root, forest.internalCoalgebra)
  println(staticTree.fullTree)

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
