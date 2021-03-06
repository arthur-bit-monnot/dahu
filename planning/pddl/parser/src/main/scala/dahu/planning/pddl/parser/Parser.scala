package dahu.planning.pddl.parser

import java.io.File

import com.sun.net.httpserver.Authenticator.Failure
import dahu.planning.model.core.CoreModel
import dahu.planning.model.full.Model
import dahu.planning.model.transforms.FullToCore
import dahu.planning.pddl.parser.optim.{ActionRewrite, InvariantInference}

import scala.io.Source
import scala.util.{Failure, Success, Try}

class Parser(implicit opt: Options) {

  implicit val predef: PddlPredef = PddlPredef(opt.discretization)

  def parseToFull(domain: String, problem: String) = Try {
    val parser = new fr.uga.pddl4j.parser.Parser()
    parser.parseFromString(domain, problem)
    val factory = new ModelFactory(predef)
    factory.loadDomain(parser.getDomain)
    Option(parser.getProblem)
      .foreach(pb => factory.loadProblem(pb))

    factory.result
  }

  def parse(domain: String, problem: String): Try[CoreModel] = {
    parseToFull(domain, problem).flatMap { m =>
      val coreM = FullToCore.trans(m)
      val result =
        if(opt.optimize)
          optim.all(coreM)
        else
          Success(coreM)
      result
    }
  }
}
