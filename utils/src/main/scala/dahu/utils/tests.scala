package dahu.utils

import scala.util.{Failure, Success, Try}

object tests {

  /** Runs a given test on all members of a sequence.
    * The test is considered failed if it throws an exception.
    *
    * In case of failure of one or more test, a summary of all failures/successes is printed to stderr.
    * In case of success of all tests, A summary string is returned.
    * */
  def subtests[T](targets: Seq[T], test: T => Unit, name: T => String): String = {
    val results =
      targets.map(t => (name(t), Try { test(t) }))

    val failures =
      results.map(_._2).collect { case Failure(e) => e }

    if(failures.nonEmpty) {
      // print summary of successes/failures and throw the first error
      for((name, res) <- results) {
        res match {
          case Success(_) => System.err.println(s"+ $name")
          case Failure(_) => System.err.println(s"- $name")
        }
      }
      failures.foreach(throw _)
      dahu.utils.errors.unexpected
    } else {
      // everything went fine, return a string recap of the problems tackled
      val stringResults: Seq[String] = for((name, res) <- results) yield {
        res match {
          case Success(_) => s"+ $name"
          case Failure(_) => dahu.utils.errors.unexpected
        }
      }
      stringResults.mkString("\n")
    }
  }
}
