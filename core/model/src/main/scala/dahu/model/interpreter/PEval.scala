package dahu.model.interpreter

import dahu.model.input.Lambda.LambdaIdent
import dahu.utils._

/** A possibly partially evaluated expression.
  * The unevaluated leaves are identified by LambdaIdents. */
trait PEval[+A] {
  def bind(id: LambdaIdent, v: Any): PEval[A]
  def map[B](f: A => B): PEval[B] = Mapped(this, f)
}

final case class FEval[A](v: A) extends PEval[A] {
  override def bind(id: LambdaIdent, bindValue: Any): FEval[A] = this
  override def map[B](f: A => B): FEval[B] = FEval(f(v))
}

final case class PEFunc[A](id: LambdaIdent, tree: PEval[A]) extends PEval[A] {
  override def bind(bindId: LambdaIdent, v: Any): PEval[A] = {
    if(bindId == id)
      tree.bind(id, v)
    else
      PEFunc(id, tree.bind(bindId, v))
  }
  def apply(v: Any): PEval[A] = bind(id, v)
}

final case class Mapped[A, B](pe: PEval[A], f: A => B) extends PEval[B] {
  override def bind(id: LambdaIdent, v: Any): PEval[B] =
    pe.bind(id, v) match {
      case FEval(a) => FEval(f(a))
      case x        => Mapped(x, f)
    }

  override def map[C](f2: B => C): PEval[C] = Mapped(pe, f.andThen(f2))
}

object PEval {
  def sequence[A: ClassTag](vs: Vec[PEval[A]]): PEval[Vec[A]] = {
    if(vs.forall(_.isInstanceOf[FEval[A]]))
      FEval(vs.map(_.asInstanceOf[FEval[A]].v))
    else {
      new PEval[Vec[A]] {
        override def bind(id: LambdaIdent, v: Any): PEval[Vec[A]] =
          sequence(vs.map(_.bind(id, v)))
      }
    }
  }

  implicit val applicativeInstance: SApplicative[PEval] = new SApplicative[PEval] {
    override def pure[A: ClassTag](x: A): PEval[A] = FEval(x)

    override def smap[@sp(Int) A, @sp(Int) B: ClassTag](fa: PEval[A])(f: A => B): PEval[B] =
      fa.map(f)

    override def ap[A, B: ClassTag](ff: PEval[A => B])(fa: PEval[A]): PEval[B] = ff match {
      case FEval(f) => fa.map(f)
      case _ =>
        new PEval[B] {
          override def bind(id: LambdaIdent, v: Any): PEval[B] = ap(ff.bind(id, v))(fa.bind(id, v))
        }
    }
  }
}
