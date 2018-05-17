package dahu.model

import dahu.model.compiler.Algebras
import dahu.model.input._
import dahu.model.ir._
import dahu.model.math.bool
import dahu.model.problem.{IDTop, LazyTree, StaticProblem}
import dahu.model.types._
import dahu.recursion._
import dahu.utils.{SubSubInt, Vec}
import utest._

object DynamicTests extends TestSuite {
  import dsl._

  type E = Fix[ExprF]
  implicit def unfix2fix(e: ExprF[Fix[ExprF]]): Fix[ExprF] = e

  val inst = new DynamicInstantiator[Int, Boolean] {
    override def typ: Tag[Boolean] = Tag[Boolean]

    override def toString: String = "has-support"
  }

  val True = Cst(true)
  val x = Cst(1)
  val y = Cst(2)
  val dec = Input[Int]("decision")

  val xProvider = DynamicProvider(True, x)
  val yProvider = DynamicProvider(True, y)
  val inProvided = Dynamic(dec, inst)

  val result =
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
