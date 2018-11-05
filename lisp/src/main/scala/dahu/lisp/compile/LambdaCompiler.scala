package dahu.lisp.compile
import dahu.model.functions.{->:, Fun1, Fun2, FunAny}
import dahu.model.input.{Expr, Lambda}
import dahu.model.products.{Field, ProductTag, ProductTagAny}
import dahu.model.types.{Tag, Value}
import dahu.utils.Vec

//object LambdaCompiler {
//
//  // base  = rec { a, b }
//  // proxy = mem => rec { mem(0), mem(1) }
//
//  // compile: mem => f(proxy(mem))
//
//  // f: rec => X
//  //    load: rec => Vec
//  //    unload: Vec => rec
//  //    f': Vec => X
//
//  class Proxy(tpe: RecordType) extends FunAny {
//    override def compute(args: Vec[Value]): Any = args match {
//      case Vec(mem: Mem) =>
//        tpe.fields.map {
//          case Field(name, Tag.ofDouble, position) => mem.readDouble(position)
//          case _                                   =>
//        }
//      case _ => ???
//    }
//    override def name: String = "proxy"
//    override def outType = tpe
//  }
//
//  case class DPairF[F[_]](a: F[Double], b: F[Double])
//  type DPair = DPairF[cats.Id]
//  object DPairF {
//    implicit val tag = ProductTag.build[DPairF]("a" -> Tag.ofDouble, "b" -> Tag.ofDouble)
//  }
//  val Pair: Expr[Double ->: Double ->: DPair] = ???
//  import dahu.model.input.dsl._
//  val x: Expr[Mem ->: DPair] = Lambda(mem => Pair(ReadDouble(mem, 0), ReadDouble(mem, 1)))
//
//}
//
//class Mem {
//  def readDouble(i: Int): Double = ???
//}
//object Mem {
//  implicit def tag: Tag[Mem] = Tag.default[Mem]
//}
//
//object ReadDouble extends Fun2[Mem, Int, Double] {
//  override def of(in1: Mem, in2: Int): Double = in1.readDouble(in2)
//  override def name: String = "read-double"
//}
