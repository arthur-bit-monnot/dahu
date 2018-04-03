package dahu.planner

import copla.lang
import copla.lang.model.core
import dahu.model.functions.{WrappedFun2, WrappedFunction}
import dahu.model.input._
import dahu.model.math.int
import dahu.model.types.{Tag, TagIsoInt}

import scala.collection.mutable

object Main extends App {

  val pb = """
      |type X;
      |type Y < X;
      |type Z < X;
      |type W < Y;
      |instance X x1, x2, x3;
      |
      |constant X v1;
      |constant X v2;
      |constant X v3;
      |
      |v1 != v2;
      |v2 != v3;
      |v3 != v1;
    """.stripMargin

  val parsed = lang.parse(pb)

  println(parsed)
  import copla.lang.model.core._
  import dahu.model.input.dsl._

  implicit class TentativeInstanceOps(private val lhs: Tentative[Instance]) extends AnyVal {
    def ===(rhs: Tentative[Instance]): Tentative[Boolean] = {
      val tag = (lhs.typ, rhs.typ) match {
        case (t1: TagIsoInt[Instance], t2: TagIsoInt[Instance]) =>
          if(t1.min <= t2.min && t2.max <= t1.max) t1
          else if(t2.min <= t1.min && t1.max <= t2.max) t2
          else ???
        case _ => ???
      }
      val f = WrappedFunction.wrap(int.EQ)(tag, implicitly[TagIsoInt[Boolean]])
      Computation(f, lhs, rhs)
    }
  }

  parsed match {
    case lang.Success(res) =>
      val ctx = ProblemContext.extract(res)
      val result = res.foldLeft(Chronicle.empty(ctx)) {
        case (chronicle, block) => chronicle.extended(block)
      }
      println(result)
      val solution = Planner.solve(result)
      println(solution)
      solution
    case x => sys.error(x.toString)
  }

}
