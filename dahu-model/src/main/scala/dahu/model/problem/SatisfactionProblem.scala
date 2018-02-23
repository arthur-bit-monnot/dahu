package dahu.model.problem

import cats.Functor
import dahu.maps.{ArrayMap, IMapBuilder, SubInt}
import dahu.model.ir._
import dahu.recursion._

import scala.reflect.ClassTag

object SatisfactionProblem {

  def extractCoalgebra[ID <: SubInt, F[_], O](base: AttributeAlgebra[ID, F, O],
                                              coalg: FCoalgebra[F, ID])(
      root: ID)(implicit F: Functor[F], ct: ClassTag[O]): (O, ArrayMap[O]) = {
    val memory = new IMapBuilder[O]()

    val alg: EnvT[ID, F, O] => O = env => {
      memory.getOrElseUpdate(env.ask, base(env))
    }
    val co = coalg.toAttributeCoalgebra
    val tmp = Recursion.hylo(co, alg)(root)
    (tmp, memory.toImmutableArray)
  }

  case class Node[ID](id: ID, value: Total[ID], constraint: Seq[ID])

  def encode(ast: AST[_]): (Node[ast.ID], ArrayMap[Node[ast.ID]]) = {
    type ID = ast.ID
    type PB = Node[ID] // EnvT[ID, Node, ID]

    val ALG: AttributeAlgebra[ID, ExprF, PB] = {
      case EnvT(id, Partial(value, condition, _)) =>
        Node(id, value.value, value.constraint ++ condition.constraint :+ condition.id)
      case EnvT(id, x: InputF[PB]) => Node(id, x, Seq())
      case EnvT(id, x: CstF[PB])   => Node(id, x, Seq())
      case EnvT(id, ComputationF(f, args, t)) =>
        Node(id, ComputationF(f, args.map(_.id), t), args.flatMap(_.constraint))
      case EnvT(id, ProductF(members, t)) =>
        Node(id, ProductF(members.map(_.id), t), members.flatMap(_.constraint))
    }

    val (root, coalg) = extractCoalgebra(ALG, ast.tree.asFunction)(ast.root)
    assert(root.id == ast.root)
    (root, coalg)
  }

}
