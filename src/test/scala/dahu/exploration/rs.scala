package dahu.exploration

import scalaz._
import Scalaz._
import scala.collection.mutable

object rs extends App {

  sealed trait Lambda[A]
  final case class Cst[A](v: Int) extends Lambda[A]
  final case class Add[A](l: A, r: A) extends Lambda[A]


  final case class Fix[F[_]](unFix: F[Fix[F]])
  final case class Cofree[F[_], A](head: A, unFix: F[Cofree[F, A]])

  type Lam = Fix[Lambda]
  type EvalLam = Cofree[Lambda, Int]

  val lambda: Lam = Fix(Add(Fix(Cst(1)), Fix(Add(Fix(Cst(1)), Fix(Cst(2))))))

  type Algebra[F[_], A] = F[A] => A

  val inTreeEval: Lam => EvalLam = _.unFix match {
    case Cst(v) => Cofree(v, Cst(v))
    case Add(Fix(l), Fix(r)) =>
      val el = inTreeEval(Fix(l))
      val er = inTreeEval(Fix(r))
      Cofree(el.head + er.head, Add(el, er))
  }



  inTreeEval(lambda)

  val eval: Algebra[Lambda, Int] = _ match {
    case Cst(v) => v
    case Add(l, r) => l + r
  }

  def cataLambda[A](e: Fix[Lambda], f: Algebra[Lambda, A]): A = e.unFix match {
    case Cst(x) => f(Cst(x))
    case Add(l, r) => f(Add(cataLambda(l, f), cataLambda(r, f)))
  }
  def cata[F[_], A](e: Fix[F])(f: Algebra[F, A])(implicit F: Functor[F]): A =
    f(F.map(e.unFix)(cata(_)(f)))

  val x = cataLambda(lambda, eval)
  println(x)

  val functor: Functor[Lambda] = new Functor[Lambda] {
    override def map[A, B](fa: Lambda[A])(f: A => B): Lambda[B] = fa match {
      case Cst(v) => Cst(v)
      case Add(l, r) => Add(f(l), f(r))
    }
  }
  implicit val traverse: Traverse[Lambda] = new Traverse[Lambda] {
    override def traverseImpl[G[_], A, B](fa: Lambda[A])(f: A => G[B])(implicit G: Applicative[G]): G[Lambda[B]] = fa match {
      case Cst (v) => G.point(Cst(v))
      case Add(l, r) =>
        (f(l) |@| f(r))(Add(_, _))
    }
  }

  assert(cata(lambda)(eval) == cataLambda(lambda, eval))


  // building Cofree structures


  def annotateLambda[A](e: Fix[Lambda])(f: Algebra[Lambda, A]): Cofree[Lambda, A] = e.unFix match {
    case x @ Cst(v) => Cofree(f(Cst(v)), Cst(v))
    case x @ Add(l, r) =>
      val al = annotateLambda(l)(f)
      val ar = annotateLambda(r)(f)
      Cofree(f(Add(al.head, ar.head)), Add(al, ar))
  }

  def annotate[F[_], A](e:Fix[F])(f: Algebra[F, A])(implicit F: Functor[F]): Cofree[F, A] = {
    val tmp: F[Cofree[F, A]] = F.map(e.unFix)(annotate(_)(f))
    val tmp2: F[A] = F.map(tmp)(_.head)
    val tmp3: A = f(tmp2)
    Cofree(tmp3, tmp)
  }


  println(annotate(lambda)(eval))


  // foldable
  type Key = Int
  type Store = Map[Lambda[Key], Key]


  def flatten(fa: Fix[Lambda]) : (Key, Map[Key, Lambda[Key]]) = {
    val store = mutable.HashMap[Lambda[Key], Key]()
    val alg: Algebra[Lambda, Key] = e => {
      if(!store.contains(e))
        store += ((e, store.size))
      store(e)
    }
    val res = cata(fa)(alg)
    (res, store.toList.map(_.swap).toMap)
  }

  println(flatten(lambda))
}
