package dahu.lisp.compile
import java.io.File

import fastparse.core.Parsed

import scala.util.{Failure, Success}

object Main extends App {

  val env = Env.default()
  val ctx = new Context(env)

  def evalFile(filename: String): Unit = {
    println(s"Parsing $filename")
    val ast = dahu.lisp.parser.parseFile(new File(filename)) match {
      case Parsed.Success(sexprs, _) => sexprs
      case x =>
        println("Failed to parse:")
        println(x)
        sys.exit(1)
    }
    for(e <- ast) {
      dahu.lisp.compile.eval(e, ctx) match {
        case Success(res) =>
          println(res)
        case Failure(e) =>
          e.printStackTrace()
          sys.exit(1)
      }
    }
  }

  evalFile("refinement/domains/prelude.clj")
  evalFile("refinement/domains/car.clj")

}
