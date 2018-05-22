package dahu.model.problem

import cats.{Functor, Id}
import dahu.model.ir._
import dahu.model.problem.ContextualLazyForestMap.ContextualPreprocessor

object ExpandLambdas {

  def expandLambdas[X, Opt[_]: Functor](
      lazyTree: LazyTree[X, StaticF, Opt, _]): LazyTree[X, NoApplyF, Opt, _] = {
    val tree = lazyTree.fixID
    val forest =
      tree.tree.mapContextualized(compiler, MyPrepro[tree.ID]())

    LazyTree(forest)(tree.root)
  }

  case class MyPrepro[I <: IDTop](map: Map[I, I] = Map[I, I]())
      extends ContextualPreprocessor[StaticF, I] {

    override def prepro(i: I): I = map.getOrElse(i, i)

    override def subPreprocessor(coalg: I => StaticF[I],
                                 fi: StaticF[I]): ContextualPreprocessor[StaticF, I] = fi match {
      case x @ ApplyF(lbd, param, _) =>
        def getLambdaParam(i: I, nesting: Int): I = coalg(i) match {
          case LambdaF(arg, _, _) if nesting == 0 => arg
          case LambdaF(_, tree, _)                => getLambdaParam(tree, nesting - 1)
          case ApplyF(x, _, _)                    => getLambdaParam(x, nesting + 1)
          case x                                  => dahu.utils.errors.unexpected(x.toString)
        }
        val lambdaArg = getLambdaParam(lbd, nesting = 0)
        MyPrepro(map + ((lambdaArg, param)))
      case _ => this
    }
  }

  def compiler[I <: IDTop](
      genCtx: LazyForestGenerator.Context[NoApplyF, I]): StaticF[cats.Id[I]] => cats.Id[I] = {
    case x @ ApplyF(lambda, param, typ) =>
      // recurse on the tree of lambda, replacing, lbd.in by param
      genCtx.retrieve(lambda) match {
        case LambdaF(_, tree, _) => tree
        case _                   => dahu.utils.errors.unexpected
      }
    case x: NoApplyF[Id[I]] => genCtx.record(x)
  }
}
