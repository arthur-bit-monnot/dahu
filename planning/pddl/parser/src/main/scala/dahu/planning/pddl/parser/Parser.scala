package dahu.planning.pddl.parser

import java.io.File

import com.sun.net.httpserver.Authenticator.Failure
import dahu.planning.model.core.CoreModel
import dahu.planning.model.full.Model
import dahu.planning.model.transforms.FullToCore
import dahu.planning.pddl.parser.optim.ActionRewrite

import scala.util.{Failure, Success, Try}

class Parser(opt: Options) {

  implicit val predef: PddlPredef = PddlPredef(opt.discretization)

  def parseToFull(domain: File, problem: File): Try[Model] = Try {
    val parser = new fr.uga.pddl4j.parser.Parser()
    parser.parse(domain, problem)
    val factory = new ModelFactory(predef)
    factory.loadDomain(parser.getDomain)
    factory.loadProblem(parser.getProblem)

    factory.result
  }

  def parse(domain: File, problem: File): Try[CoreModel] = {
    parseToFull(domain, problem).flatMap { m =>
      val coreM = FullToCore.trans(m)
      if(opt.optimize)
        new ActionRewrite(opt).optimize(coreM)
      else
        Success(coreM)
    }
  }
}
