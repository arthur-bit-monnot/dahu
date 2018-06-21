package dahu.planning.pddl.problems

import scala.io.Source

case class PddlProblem(domain: String, problem: String)

object Extractor {

  def extract(domain: String, problem: String): Either[String, PddlProblem] = {
    val (domPath, pbPath) =
      if(domain == "tests")
        (s"tests/$problem.dom.pddl", s"tests/$problem.pb.pddl")
      else
        (s"$domain/$domain.dom.pddl", s"$domain/$domain.$problem.pb.pddl")

    for {
      dom <- loadFromResource(domPath)
      pb <- loadFromResource(pbPath)
    } yield PddlProblem(dom, pb)
  }

  private def loadFromResource(path: String): Either[String, String] =
    Option(Source.fromResource(path)) match {
      case Some(r) =>
        assert(r != null)
        Right(r.mkString)
      case None => Left(s"Could not load resource: $path")
    }

}
