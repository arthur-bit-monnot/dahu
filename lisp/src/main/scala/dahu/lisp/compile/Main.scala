package dahu.lisp.compile
import java.io.File

import fastparse.core.Parsed

import scala.util.{Failure, Success}

object Main extends App {

  val env = Env.default()
  implicit val ctx = new Context(env)

  evalFile("refinement/domains/prelude.clj")
  evalFile("refinement/domains/car.clj")

}
