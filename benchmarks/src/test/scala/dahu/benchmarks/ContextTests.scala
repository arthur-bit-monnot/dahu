package dahu.benchmarks

import dahu.model.problem.{API, Group}

object ContextTests extends App {

  Optionals.instancesMap.foreach {
    case (name, pb) =>
      println("===== name ====")
      val ast = API.parse(pb.pb)
      Group.process2(ast)

      println("")
  }

}
