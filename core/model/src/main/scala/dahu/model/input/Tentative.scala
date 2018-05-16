package dahu.model.input

import cats.Id
import dahu.utils._
import dahu.graphs.DAG
import dahu.model.functions._
import dahu.model.ir.DynamicProviderF
import dahu.model.types._
import spire.sp

import scala.reflect.ClassTag

/** Evaluation yields an Either[ConstraintViolated, T] */
sealed trait Tentative[T] {
  def typ: Tag[T]
}
object Tentative {
  import scala.language.implicitConversions
  implicit def toAny[T](x: Tentative[T]): Tentative[Any] = x.asInstanceOf[Tentative[Any]]

  implicit def dagInstance: DAG[Id, Tentative[Any]] = new DAG[Id, Tentative[Any]] {
    override def algebra: Tentative[Any] => Id[Tentative[Any]] = x => x
    override def children(graph: Id[Tentative[Any]]): Set[Tentative[Any]] = graph match {
      case SubjectTo(value, condition) => Set(value, condition)
      case x: Product[_]               => x.members.toSet
      case x: Input[_]                 => Set()
      case x: Cst[_]                   => Set()
      case x: Computation[_]           => x.args.toSet
      case Optional(value, present)    => Set(value, present)
      case ITE(cond, onTrue, onFalse)  => Set(cond, onTrue, onFalse)
      case Dynamic(p, _)               => Set(p)
      case DynamicProvider(e, prov)    => Set(e, prov)
    }
  }
}

/** Evaluation: eval(condition).flatMap(eval(value)) */
final case class SubjectTo[T](value: Tentative[T], condition: Tentative[Boolean])
    extends Tentative[T] {
  override def typ: Tag[T] = value.typ
}

final case class Optional[T](value: Tentative[T], present: Tentative[Boolean])
    extends Tentative[T] {
  override def typ: Tag[T] = value.typ
}

sealed abstract class Term[T] extends Tentative[T]

/** Evaluation yields a Right[T] */
final case class Input[T: Tag](id: Ident) extends Term[T] {
  override def typ: Tag[T] = Tag[T]
}
object Input {
  def apply[T: Tag](name: String): Tentative[T] = new Input[T](Ident(name))
  def apply[T: Tag](): Tentative[T] = new Input[T](Ident.anonymous())
}

/** Evaluation returns a Right[T](value) */
final case class Cst[T: Tag](value: T) extends Term[T] {
  override def typ: Tag[T] = Tag[T]
}
object Cst {
  def apply[T: Tag](v: T): Tentative[T] = new Cst(v)
}

final case class ITE[T](cond: Tentative[Boolean], onTrue: Tentative[T], onFalse: Tentative[T])
    extends Tentative[T] {
  override def typ: Tag[T] = onTrue.typ
}

final case class Present(value: Tentative[_]) extends Term[Boolean] {
  override def typ: Tag[Boolean] = Tag.ofBoolean
}
final case class Valid(value: Tentative[_]) extends Term[Boolean] {
  override def typ: Tag[Boolean] = Tag.ofBoolean
}

sealed abstract class Computation[O] extends Tentative[O] {
  override def typ: Tag[O] = f.outType
  def f: Fun[O]
  def args: Seq[Tentative[Any]]

  override def toString: String = s"$f(${args.mkString(", ")})"
}

final case class Product[T[_[_]]](value: T[Tentative])(implicit tt: ProductTag[T])
    extends Tentative[T[Id]] {
  override def typ: ProductTag[T] = tt
  def members: Vec[Tentative[Any]] = tt.exprProd.extractTerms(value)
  def buildFromVals(terms: Vec[Any]): T[Id] = tt.idProd.buildFromTerms(terms)
  def buildFromExpr(terms: Vec[Tentative[Any]]): T[Tentative] = tt.exprProd.buildFromTerms(terms)
}
object Product {
  def fromSeq[T](seq: Seq[Tentative[T]])(implicit ev: ProductTag[ProductTag.Sequence[?[_], T]])
    : Product[ProductTag.Sequence[?[_], T]] =
    new Product[ProductTag.Sequence[?[_], T]](seq)(ev)

  import scala.reflect.runtime.universe
  def fromMap[K: ClassTag, V: ClassTag](map: Map[K, Tentative[V]])(
      implicit tt: universe.WeakTypeTag[Map[K, Id[V]]]): Product[PMap[K, ?[_], V]] = {
    type M[F[_]] = PMap[K, F, V]
    val keys: Vec[K] = Vec.fromSeq(map.keys.toSeq)
    val values: Vec[Tentative[Any]] = keys.map(map(_).asInstanceOf[Tentative[Any]])

    // build a specific type tag that remembers the keys of the original map.
    val tag = new ProductTag[M] {
      override def exprProd: ProductExpr[M, Tentative] = new ProductExpr[M, Tentative] {
        override def extractTerms(prod: M[Tentative])(
            implicit ct: ClassTag[Tentative[Any]]): Vec[Tentative[Any]] = {
          assert(prod == map)
          values
        }

        override def buildFromTerms(terms: Vec[Tentative[Any]]): M[Tentative] = {
          assert(terms == values)
          map
        }
      }

      override def idProd: ProductExpr[M, Id] = new ProductExpr[M, Id] {
        override def extractTerms(prod: M[Id])(implicit ct: ClassTag[Id[Any]]): Vec[Id[Any]] = {
          assert(prod.keys == map.keys)
          keys.map(k => prod(k))
        }
        override def buildFromTerms(terms: Vec[Id[Any]]): M[Id] = {
          assert(terms.size == values.size)
          keys.zip(terms.map(_.asInstanceOf[Id[V]])).toMap
        }
      }

      override def typ: Tag.Type = tt.tpe
    }

    Product[M](map)(tag)
  }

  type PMap[K, F[_], V] = Map[K, F[V]]
}

trait ProductExpr[P[_[_]], F[_]] {
  def extractTerms(prod: P[F])(implicit ct: ClassTag[F[Any]]): Vec[F[Any]]
  def buildFromTerms(terms: Vec[F[Any]]): P[F]
  def buildFromValues(terms: Vec[F[Value]]): P[F] =
    buildFromTerms(terms.asInstanceOf[Vec[F[Any]]])
}

object ProductExpr {

  def apply[P[_[_]], F[_]](implicit instance: ProductExpr[P, F]): ProductExpr[P, F] = instance

  import shapeless._

  trait HListExtract[H <: HList, F[_]] {
    def terms(h: H): List[F[Any]]
    def fromTerms(l: Seq[F[Any]]): H
  }

  implicit def genPE[P[_[_]], F[_], H <: HList](implicit gen: Generic.Aux[P[F], H],
                                                hListExtract: HListExtract[H, F]) =
    new ProductExpr[P, F] {
      override def extractTerms(prod: P[F])(implicit ct: ClassTag[F[Any]]): Vec[F[Any]] =
        Vec.fromSeq(hListExtract.terms(gen.to(prod)))
      override def buildFromTerms(terms: Vec[F[Any]]): P[F] =
        gen.from(hListExtract.fromTerms(terms.toSeq))
    }

  implicit def peOfHNil[F[_]]: HListExtract[HNil, F] = new HListExtract[HNil, F] {
    override def terms(h: HNil): List[F[Any]] = Nil
    override def fromTerms(l: Seq[F[Any]]): HNil = {
      require(l.isEmpty)
      HNil
    }
  }
  implicit def peOfHlist[H, T <: HList, F[_]](
      implicit t: HListExtract[T, F]): HListExtract[F[H] :: T, F] =
    new HListExtract[F[H] :: T, F] {
      override def terms(l: F[H] :: T): List[F[Any]] =
        l.head.asInstanceOf[F[Any]] :: t.terms(l.tail)
      override def fromTerms(l: Seq[F[Any]]): F[H] :: T =
        l.head.asInstanceOf[F[H]] :: t.fromTerms(l.tail)
    }
}

object Computation {
  def apply[I, O](f: Fun1[I, O], in: Tentative[I]): Computation1[I, O] =
    Computation1(f, in)

  def apply[I1, I2, O](f: Fun2[I1, I2, O],
                       in1: Tentative[I1],
                       in2: Tentative[I2]): Computation2[I1, I2, O] =
    Computation2(f, in1, in2)

  def apply[I1, I2, I3, O](f: Fun3[I1, I2, I3, O],
                           in1: Tentative[I1],
                           in2: Tentative[I2],
                           in3: Tentative[I3]): Computation3[I1, I2, I3, O] =
    Computation3(f, in1, in2, in3)

  def apply[I, O](fun: FunN[I, O], arguments: Seq[Tentative[I]]): Computation[O] =
    new Computation[O] {
      override def f: Fun[O] = fun
      override def args: Seq[Tentative[Any]] = arguments.asInstanceOf[Seq[Tentative[Any]]]
    }
}
final case class Computation1[I, O](f: Fun1[I, O], in: Tentative[I]) extends Computation[O] {
  override val args: Seq[Tentative[Any]] = Seq(in).asInstanceOf[Seq[Tentative[Any]]]
}

final case class Computation2[I1, I2, O](f: Fun2[I1, I2, O], in: Tentative[I1], in2: Tentative[I2])
    extends Computation[O] {
  override val args: Seq[Tentative[Any]] = Seq(in, in2).asInstanceOf[Seq[Tentative[Any]]]
}

final case class Computation3[I1, I2, I3, O](f: Fun3[I1, I2, I3, O],
                                             in: Tentative[I1],
                                             in2: Tentative[I2],
                                             in3: Tentative[I3])
    extends Computation[O] {
  override val args: Seq[Tentative[Any]] = Seq(in, in2, in3).asInstanceOf[Seq[Tentative[Any]]]
}

final case class Computation4[I1, I2, I3, I4, O](f: Fun4[I1, I2, I3, I4, O],
                                                 in: Tentative[I1],
                                                 in2: Tentative[I2],
                                                 in3: Tentative[I3],
                                                 in4: Tentative[I4])
    extends Computation[O] {
  override val args: Seq[Tentative[Any]] = Seq(in, in2, in3, in4).asInstanceOf[Seq[Tentative[Any]]]
}

trait DynamicInstantiator[Params, Out] {
  def typ: Tag[Out]
}

final case class Dynamic[Params, Out](params: Tentative[Params],
                                      dynamicInstantiator: DynamicInstantiator[Params, Out])
    extends Tentative[Out] {
  override def typ: Tag[Out] = dynamicInstantiator.typ
}

final case class DynamicProvider[A, Provided](e: Tentative[A], provided: Tentative[Provided])
    extends Tentative[A] {
  override def typ: Tag[A] = e.typ
}
