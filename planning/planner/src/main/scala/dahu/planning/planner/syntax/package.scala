package dahu.planning.planner

import dahu.model.input.{Computation, Cst, Tentative}
import dahu.model.math._
import dahu.planning.model.common.Interval

package object syntax {

  trait Comparable[A, B] {
    type C
    def leq(a: A, b: B): C
    def lt(a: A, b: B): C
    def geq(a: A, b: B): C
    def gt(a: A, b: B): C
  }

  trait Ordered[A] extends Comparable[A, A] {
    override def geq(a: A, b: A): C = leq(b, a)
    override def gt(a: A, b: A): C = lt(b, a)
  }
  object Ordered {
    type Aux[A, C0] = Ordered[A] { type C = C0 }

    implicit val ofTentativeInt: Aux[Tentative[Int], Tentative[Boolean]] =
      new Ordered[Tentative[Int]] {
        type C = Tentative[Boolean]
        override def leq(a: Tentative[Int], b: Tentative[Int]): Tentative[Boolean] =
          Computation(int.LEQ, a, b)

        override def lt(a: Tentative[Int], b: Tentative[Int]): Tentative[Boolean] =
          leq(Computation(int.Add, Seq(a, Cst(1))), b)
      }

    implicit def intervalComparable[A, C](implicit C: Ordered[A]) =
      new Ordered[Interval[A]] {
        type C = C.C
        override def leq(a: Interval[A], b: Interval[A]): C =
          C.leq(a.end, b.start)

        override def lt(a: Interval[A], b: Interval[A]): C =
          if(a.isRightOpen || b.isLeftOpen)
            C.leq(a.end, b.start)
          else
            C.lt(a.end, b.start)
      }

    implicit class OrderedOps[A](private val a: A) extends AnyVal {
      def <=(b: A)(implicit C: Ordered[A]): C.C = C.leq(a, b)
      def :<(b: A)(implicit C: Ordered[A]): C.C = C.lt(a, b)
    }

  }

}
