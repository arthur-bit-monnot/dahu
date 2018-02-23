package dahu.solvers.problem

import cats.Functor
import dahu.model.ir._
import dahu.recursion._

import scala.collection.mutable

object ADT {


  type ID = Int

  case class Node[ID](id: ID, value: Total[ID], constraint: Seq[ID])
  type PB2 = Node[ID] // EnvT[ID, Node, ID]

  def extractCoalgebra[ID, F[_], O](base: AttributeAlgebra[ID, F, O], coalg: FCoalgebra[F, ID])(root: ID)(implicit F: Functor[F]): (O, ID => O) = {
    val memory = mutable.Map[ID, O]()
    val alg: EnvT[ID, F, O] => O = env => {
      memory.getOrElseUpdate(env.ask, base(env))
    }
    val co = coalg.toAttributeCoalgebra
    val tmp = Recursion.hylo(co, alg)(root)
    (tmp, x => memory(x))
  }

  val ALG: AttributeAlgebra[ID, ExprF, PB2] = {
    case EnvT(id, Partial(value, condition, _)) => Node(id, value.value, value.constraint ++ condition.constraint :+ condition.id)
    case EnvT(id, x: InputF[PB2]) => Node(id, x, Seq())
    case EnvT(id, x: CstF[PB2]) => Node(id, x, Seq())
    case EnvT(id, ComputationF(f, args, t)) =>
      Node(id, ComputationF(f, args.map(_.id), t), args.flatMap(_.constraint))
    case EnvT(id, ProductF(members, t)) =>
      Node(id, ProductF(members.map(_.id), t), members.flatMap(_.constraint))
  }

}