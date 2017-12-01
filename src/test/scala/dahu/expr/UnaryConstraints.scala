package dahu.expr

import org.scalatest.FunSpec


import scala.reflect.runtime.universe.{Type, TypeTag, typeOf}

class UnaryConstraints extends FunSpec {


//  sealed abstract class Val[+A] {
//    def flatMap[B](f: A => Val[B]): Val[B]
//  }
//  final case class Present[T](value: T) extends Val[T] {
//    override def flatMap[B](f: T => Val[B]): Val[B] = f(value)
//  }
//  final case object Absent extends Val[Nothing] {
//    override def flatMap[B](f: Nothing => Val[B]): Val[B] = this
//  }
//  type Val[T] = Option[T]


//  def lift[T](value: T): Val[T] = Some(value)
//  def subjectTo[T](value: Expr[T], constraint: Expr[Boolean]): Expr[Val[T]] = {
//    dsl.IF(constraint, Some(value), None)
//  }

//  def lift[F, I1, O](f: Fun1[I1, O])(implicit ev: TypeTag[Val[O]]): Fun1[Val[I1],Val[O]] = {
//    new Fun1[Val[I1], Val[O]] {
//      override def apply(in: Val[I1]): Val[O] = in.map(f.apply)
//
//      override def name: String = f.name
//    }
//  }
}
