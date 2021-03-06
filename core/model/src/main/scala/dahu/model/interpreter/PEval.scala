package dahu.model.interpreter

import dahu.model.input.Lambda.LambdaIdent
import dahu.model.input.TypedIdent
import dahu.utils._

/** A possibly partially evaluated expression.
  * The unevaluated leaves are identified by LambdaIdents.
  *
  * TODO: this is now useless with the new pure model.
  * */
sealed trait PEval[+A] {
  def bind(id: LambdaIdent, v: PEval[Any]): PEval[A]
  def apply(v: PEval[Any]): PEval[A]
  def applicationStack: List[LambdaIdent]

  def castUnsafe[B]: PEval[B] = this.asInstanceOf[PEval[B]]
}

case object PConstraintViolated extends PEval[Nothing] {
  override def bind(id: LambdaIdent, v: PEval[Any]): PEval[Nothing] = this
  override def apply(v: PEval[Any]): PEval[Nothing] = this
  override def applicationStack: Nil.type = Nil
}

case object PEmpty extends PEval[Nothing] {
  override def bind(id: LambdaIdent, v: PEval[Any]): PEval[Nothing] = this
  override def apply(v: PEval[Any]): PEval[Nothing] = this
  override def applicationStack: Nil.type = Nil
}

final case class FEval[A](v: A) extends PEval[A] {
  override def bind(id: LambdaIdent, bindValue: PEval[Any]): FEval[A] = this
  override def apply(v: PEval[Any]): PEval[A] = this
  override def applicationStack: Nil.type = Nil
}

final case class Unknown(unboundVars: Set[TypedIdent]) extends PEval[Nothing] {
  require(unboundVars.nonEmpty)
  override def bind(id: LambdaIdent, v: PEval[Any]): Unknown = this
  override def apply(v: PEval[Any]): PEval[Nothing] = this
  override def applicationStack: Nil.type = Nil
}

sealed trait Pending[+A] extends PEval[A]

final case class PEFunc[A](id: LambdaIdent, tree: PEval[A]) extends Pending[A] {
  override def bind(bindId: LambdaIdent, v: PEval[Any]): PEval[A] =
    if(bindId == id) tree.bind(id, v)
    else PEFunc(id, tree.bind(bindId, v))

  override def apply(v: PEval[Any]): PEval[A] = bind(id, v)
  override def applicationStack: List[LambdaIdent] = id :: tree.applicationStack
}

final case class LambdaParamPlaceHolder[A](id: LambdaIdent) extends Pending[A] {
  override def bind(bindId: LambdaIdent, v: PEval[Any]): PEval[A] =
    if(id == bindId) v.asInstanceOf[PEval[A]]
    else this
  override def apply(v: PEval[Any]): PEval[A] = this
  def applicationStack: Nil.type = Nil
}

final case class FlatMapped[A, B] private (pe: PEval[A],
                                           f: A => PEval[B],
                                           applicationStack: List[LambdaIdent])
    extends Pending[B] {
  require(pe.isInstanceOf[Pending[_]])
  override def bind(id: LambdaIdent, v: PEval[Any]): PEval[B] =
    pe.bind(id, v) match {
      case FEval(a)            => f(a).bind(id, v)
      case PEmpty              => PEmpty
      case PConstraintViolated => PConstraintViolated
      case x: Unknown          => x
      case x                   => FlatMapped(x, f.andThen(_.bind(id, v)), applicationStack)
    }

  override def apply(v: PEval[Any]): PEval[B] =
    applicationStack match {
      case Nil       => this
      case h :: tail => FlatMapped(pe.bind(h, v), f.andThen(_.apply(v)), tail)
    }
}
object FlatMapped {
  def apply[A, B](pe: PEval[A], f: A => PEval[B], appStack: List[LambdaIdent]): PEval[B] =
    pe match {
      case FEval(v)            => f(v)
      case PEmpty              => PEmpty
      case PConstraintViolated => PConstraintViolated
      case x: Unknown          => x
      case _                   => new FlatMapped(pe, f, appStack)
    }
}

object PEval {

  implicit val classTagK: ClassTagK[PEval] = ClassTagK[PEval]

  implicit val applicativeInstance: SApplicative[PEval] = new SApplicative[PEval] {
    override def pure[A: ClassTag](x: A): PEval[A] = FEval(x)

    override def smap[@sp(Int) A, @sp(Int) B: ClassTag](fa: PEval[A])(f: A => B): PEval[B] =
      fa match {
        case PEmpty              => PEmpty
        case PConstraintViolated => PConstraintViolated
        case _                   => FlatMapped(fa, f.andThen(pure(_)), fa.applicationStack)

      }

    override def ap[A, B: ClassTag](ff: PEval[A => B])(fa: PEval[A]): PEval[B] = {
      (ff, fa) match {
        case (FEval(f), FEval(a))     => FEval(f(a))
        case (PEmpty, _)              => PEmpty
        case (_, PEmpty)              => PEmpty
        case (PConstraintViolated, _) => PConstraintViolated
        case (_, PConstraintViolated) => PConstraintViolated
        case (Unknown(x), Unknown(y)) => Unknown(x ++ y)
        case (l: Unknown, _)          => l
        case (_, r: Unknown)          => r
        case _ =>
          FlatMapped[A => B, B](ff, f => smap(fa)(f), fa.applicationStack ++ ff.applicationStack)
      }
    }
  }
}
