package dahu.refinement
import cats.Id
import dahu.model.functions.->:
import dahu.model.input._
import dahu.model.products.{Field, ProductTag}
import dahu.model.types
import dahu.model.types.{Bool, Tag}
import dahu.model.types.Tag.Type
import dahu.utils._
import dahu.model.input.dsl._
import dahu.model.math.bool

import scala.reflect.ClassTag

//object Test {
//  type R = Double
//  implicit val rTag: Tag[R] = Tag.ofDouble
//
//  case class TimeStampedF[F[_], S[_[_]]](time: F[Float], s: F[S[F]])
//  type TimeStamped[S[_[_]]] = TimeStampedF[Id, S]
//
//  case class DStateF[F[_]](moving: F[Bool])
//  type DState = DStateF[Id]
////  case class CStateF[F[_]](x: F[Float], y: F[Float])
////  type CState = CStateF[Id]
//  type TCState = TimeStamped[CF]
//
////  case class BandF[F[_]](ds: F[DState], cs: F[Vec[TCState]])
////  type Band = BandF[Id]
//
//  type CF[X[_]] = Vec[X[R]]
//  type C = CF[Id] // Vec[Float]
//  type Band = Vec[C]
//
//  implicit val xx = new ProductTag[CF] {
//    override val fields: Vec[Field] = Vec(Field("x", rTag, 0), Field("y", rTag, 1))
//    override def fromValues(fields: Vec[Any]): CF[Id] = fields.map(x => x.asInstanceOf[R])
//    override def getFields(prod: CF[Expr]): Vec[Expr[Any]] = prod.upcast
//    override def getFieldsIdentity(prod: C): Vec[Any] = prod.upcast
//    override def clazz: ClassTag[CF[Id]] = implicitly[ClassTag[CF[Id]]]
//    override def typ: Type = ???
//  }
//  val getX = xx.getAccessor[R]("x")
//  val getY = xx.getAccessor[R]("y")
//
//  type Constraint = Expr[C ->: Bool]
//  type EvolErr = Expr[TCState ->: TCState ->: R]
//
//  val xBelow10: Constraint = Lambda[C, Bool](s => getX(s) <= Cst(10.0))
//
//  val band = Input[Band]("band", Scope.root)
//
////  val atOrig: Expr[C ->: Bool] = Lambda(c => getX(c) === Cst(0.0) && getY(c) === Cst(0.0))
////  val startAtOrig: Expr[Band ->: Bool] =
////    Lambda[Band, Bool](band => band.first.present && atOrig(band.first.value))
////
////  val c1 = band.forall(xBelow10)
////  val c2 = startAtOrig(band)
//}
