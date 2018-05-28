package dahu.model.input

import cats.Id
import dahu.utils._
import dahu.graphs.DAG
import dahu.model.types.Tag.{Type, _}
import dahu.model.functions._
import dahu.model.math.Monoid
import dahu.model.types._

import scala.reflect.ClassTag

/** Evaluation yields an Either[ConstraintViolated, T] */
sealed trait Expr[+T] {
  def typ: Tag[T] //@uncheckedVariance
}
object Expr {

  implicit def dagInstance: DAG[Id, Expr[Any]] = new DAG[Id, Expr[Any]] {
    override def algebra: Expr[Any] => Id[Expr[Any]] = x => x
    override def children(graph: Id[Expr[Any]]): Iterable[Expr[Any]] = graph match {
      case SubjectTo(value, condition) => Iterable(value, condition)
      case x: Product[_]               => x.members.toIterable
      case x: Input[_]                 => Iterable.empty
      case x: Cst[_]                   => Iterable.empty
      case x: Computation[_]           => x.args
      case Optional(value, present)    => Iterable(value, present)
      case ITE(cond, onTrue, onFalse)  => Iterable(cond, onTrue, onFalse)
      case Dynamic(l, _, _)            => Iterable(l)
      case DynamicProvider(e, prov)    => Iterable(e, prov)
      case Apply(l, i)                 => Iterable(l, i)
      case x @ Lambda(_, _)            => Iterable(x.parameterizedTree, x.inputVar)
      case Lambda.Param(_)             => Iterable.empty
      case Present(v)                  => Iterable(v)
      case Valid(v)                    => Iterable(v)
      case Sequence(ms)                => ms.toIterable
      case MapSeq(target, f)           => Iterable(target, f)
    }
  }
}

/** Evaluation: eval(condition).flatMap(eval(value)) */
final case class SubjectTo[T](value: Expr[T], condition: Expr[Boolean]) extends Expr[T] {
  override def typ: Tag[T] = value.typ
}

final case class Optional[T](value: Expr[T], present: Expr[Boolean]) extends Expr[T] {
  override def typ: Tag[T] = value.typ
}

sealed abstract class Term[T] extends Expr[T]

/** Evaluation yields a Right[T] */
final case class Input[T: Tag](id: Ident) extends Term[T] {
  override def typ: Tag[T] = Tag[T]
}
object Input {
  def apply[T: Tag](name: String): Expr[T] = new Input[T](Ident(name))
  def apply[T: Tag](): Expr[T] = new Input[T](Ident.anonymous())
}

/** Evaluation returns a Right[T](value) */
final case class Cst[T: Tag](value: T) extends Term[T] {
  override def typ: Tag[T] = Tag[T]
}
object Cst {
  def apply[T: Tag](v: T): Expr[T] = new Cst(v)
}

final case class ITE[T](cond: Expr[Boolean], onTrue: Expr[T], onFalse: Expr[T]) extends Expr[T] {
  override def typ: Tag[T] = onTrue.typ
}

final case class Present(value: Expr[_]) extends Term[Boolean] {
  override def typ: Tag[Boolean] = Tag.ofBoolean
}
final case class Valid(value: Expr[_]) extends Term[Boolean] {
  override def typ: Tag[Boolean] = Tag.ofBoolean
}

sealed abstract class Computation[O] extends Expr[O] {
  override def typ: Tag[O] = f.outType
  def f: Fun[O]
  def args: Seq[Expr[Any]] // TODO: make args a Vec

  override def toString: String = s"$f(${args.mkString(", ")})"
}

final case class Sequence[T: Tag](members: Vec[Expr[T]])(implicit ct: ClassTag[Vec[T]])
    extends Expr[Vec[T]] {
  override def typ: SequenceTag[T] = SequenceTag[T]
}
object Sequence {
  def apply[T: Tag](members: Seq[Expr[T]])(implicit classTag: ClassTag[Vec[T]]): Sequence[T] =
    Sequence(Vec.fromSeq(members))
}

final case class MapSeq[A, B: Tag](target: Expr[Vec[A]], f: Expr[A ->: B]) extends Expr[Vec[B]] {
  override def typ: SequenceTag[B] = SequenceTag[B]
}

final case class Product[T[_[_]]](value: T[Expr])(implicit tt: ProductTag[T]) extends Expr[T[Id]] {
  require(tt != null)
  override def typ: ProductTag[T] = tt
  def members: Vec[Expr[Any]] = {
    tt.exprProd.extractTerms(value)
  }
  def buildFromVals(terms: Vec[Any]): T[Id] = tt.idProd.buildFromTerms(terms)
  def buildFromExpr(terms: Vec[Expr[Any]]): T[Expr] = tt.exprProd.buildFromTerms(terms)
}
object Product {
  def fromSeq[T](seq: Seq[Expr[T]])(implicit ev: ProductTag[ProductTag.Sequence[?[_], T]])
    : Product[ProductTag.Sequence[?[_], T]] =
    new Product[ProductTag.Sequence[?[_], T]](seq)(ev)

  import scala.reflect.runtime.universe
  def fromMap[K: ClassTag, V: ClassTag](map: Map[K, Expr[V]])(
      implicit tt: universe.WeakTypeTag[Map[K, Id[V]]]): Product[PMap[K, ?[_], V]] = {
    type M[F[_]] = PMap[K, F, V]
    val keys: Vec[K] = Vec.fromSeq(map.keys.toSeq)
    val values: Vec[Expr[Any]] = keys.map(map(_))

    // build a specific type tag that remembers the keys of the original map.
    val tag = new ProductTag[M] {
      override def exprProd: ProductExpr[M, Expr] = new ProductExpr[M, Expr] {
        override def extractTerms(prod: M[Expr])(
            implicit ct: ClassTag[Expr[Any]]): Vec[Expr[Any]] = {
          assert(prod == map)
          values
        }

        override def buildFromTerms(terms: Vec[Expr[Any]]): M[Expr] = {
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
  def apply[I, O](f: Fun1[I, O], in: Expr[I]): Computation1[I, O] =
    Computation1(f, in)

  def apply[I1, I2, O](f: Fun2[I1, I2, O], in1: Expr[I1], in2: Expr[I2]): Computation2[I1, I2, O] =
    Computation2(f, in1, in2)

  def apply[I1, I2, I3, O](f: Fun3[I1, I2, I3, O],
                           in1: Expr[I1],
                           in2: Expr[I2],
                           in3: Expr[I3]): Computation3[I1, I2, I3, O] =
    Computation3(f, in1, in2, in3)

  def apply[I, O](fun: FunN[I, O], arguments: Seq[Expr[I]]): Computation[O] =
    new Computation[O] {
      override def f: Fun[O] = fun
      override def args: Seq[Expr[Any]] = arguments
    }
}
final case class Computation1[I, O](f: Fun1[I, O], in: Expr[I]) extends Computation[O] {
  override val args: Seq[Expr[Any]] = Seq(in)
}

final case class Computation2[I1, I2, O](f: Fun2[I1, I2, O], in: Expr[I1], in2: Expr[I2])
    extends Computation[O] {
  override val args: Seq[Expr[Any]] = Seq(in, in2)
}

final case class Computation3[I1, I2, I3, O](f: Fun3[I1, I2, I3, O],
                                             in: Expr[I1],
                                             in2: Expr[I2],
                                             in3: Expr[I3])
    extends Computation[O] {
  override val args: Seq[Expr[Any]] = Seq(in, in2, in3)
}

final case class Computation4[I1, I2, I3, I4, O](f: Fun4[I1, I2, I3, I4, O],
                                                 in: Expr[I1],
                                                 in2: Expr[I2],
                                                 in3: Expr[I3],
                                                 in4: Expr[I4])
    extends Computation[O] {
  override val args: Seq[Expr[Any]] = Seq(in, in2, in3, in4)
}

final case class Dynamic[Provided, Out: Tag](f: Expr[Provided ->: Out],
                                             comb: Monoid[Out],
                                             accept: Option[Tag[Provided] => Boolean])
    extends Expr[Out] {
  override def typ: Tag[Out] = Tag[Out]
}

final case class DynamicProvider[A, Provided](e: Expr[A], provided: Expr[Provided])
    extends Expr[A] {
  override def typ: Tag[A] = e.typ
}

final case class Lambda[I: Tag, O: Tag](private val f: Expr[I] => Expr[O],
                                        name: Option[String] = None)
    extends Expr[I ->: O] {
  def outTag: Tag[O] = Tag[O]

  def id: Ident = Ident(this)

  val inputVar: Lambda.Param[I] = Lambda.Param(this)

  // f might be side effectfull in the sense that it may generate (identity-full) functions/closures
  val parameterizedTree: Expr[O] = f(inputVar)

  override def typ: LambdaTag[I, O] = LambdaTag[I, O]

  override def toString: String = name.getOrElse(super.toString)

  def named(name: String): Lambda[I, O] = this.copy(name = Some(name))
}

object Lambda {
  final case class Param[I: Tag](lambda: Lambda[I, _]) extends Expr[I] {
    override def typ: Tag[I] = Tag[I]
  }
}

final case class Apply[I, O: Tag](l: Expr[I ->: O], in: Expr[I]) extends Expr[O] {
  override def typ: Tag[O] = Tag[O]
}
