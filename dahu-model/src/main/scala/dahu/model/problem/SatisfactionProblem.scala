package dahu.model.problem

import cats.Functor
import cats.implicits._
import cats.syntax._
import cats.free.Cofree
import dahu.maps.{ArrayMap, IMapBuilder, SubInt}
import dahu.model.input.SubjectTo
import dahu.model.ir._
import dahu.model.math.bool
import dahu.model.types.{Tag, TagIsoInt}
import dahu.recursion._

import scala.reflect.ClassTag

object SatisfactionProblem {

  def extractCoalgebra[ID <: SubInt, F[_], O](base: AttributeAlgebra[ID, F, O],
                                              coalg: FCoalgebra[F, ID])(
      root: ID)(implicit F: Functor[F], ct: ClassTag[F[O]]): (O, ArrayMap[F[O]]) = {
    val memory = new IMapBuilder[F[O]]()

    val alg: EnvT[ID, F, O] => O = env => {
      val fo = memory.getOrElseUpdate(env.ask, env.lower)
      base(env)
    }
    val co = coalg.toAttributeCoalgebra
    val tmp = Recursion.hylo(co, alg)(root)
    (tmp, memory.toImmutableArray)
  }

  case class IC[ID](id: ID, consts: Seq[ID])

  type PB = Partial[Fix[Total]]

  private object Utils {
    import scala.language.implicitConversions
    implicit def autoFix[F[_]](x: F[Fix[F]]): Fix[F] = Fix(x)

    def and(conjuncts: Fix[Total]*): Fix[Total] = {
      conjuncts.forall(c => c.unfix.typ == Tag.ofBoolean)
      ComputationF(bool.And, conjuncts.toSeq, Tag.ofBoolean)
    }
  }
  import Utils._

  val ALG: FAlgebra[ExprF, PB] = {
    case Partial(value, condition, tpe) =>
      Partial(value.value, and(value.condition, condition.condition, condition.value), tpe)
    case x: InputF[PB] => Partial(Fix(x), and(), x.typ)
    case x: CstF[PB]   => Partial(Fix(x), and(), x.typ)
    case ComputationF(f, args, t) =>
      Partial(ComputationF(f, args.map(a => a.value), t), and(args.map(_.condition): _*), t) //args.flatMap(_.condition.unfix), t)
    case ProductF(members, t) =>
      Partial(ProductF(members.map(a => a.value), t), and(members.map(_.condition): _*), t)
  }

  def encode[X](root: X, coalgebra: FCoalgebra[ExprF, X]): PB = {
    val pb = Recursion.hylo(coalgebra, ALG)(root)
    val simplified = dahu.recursion.Recursion.cata[Total, Fix[Total]](
      dahu.model.compiler.Optimizations.simplificationAlgebra)(pb.condition)
    pb.copy(condition = simplified)
  }

}
