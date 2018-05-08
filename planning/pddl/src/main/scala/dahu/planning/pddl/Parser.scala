package dahu.planning.pddl

import java.io.File

import dahu.planning.model.core.CoreModel
import dahu.planning.model.full.Model
import dahu.planning.model.transforms.FullToCore

import scala.util.Try

class Parser(opt: Options) {

  implicit private val predef: PddlPredef = PddlPredef(opt.discretization)

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
      Try(FullToCore.trans(m))
    }
  }

}
