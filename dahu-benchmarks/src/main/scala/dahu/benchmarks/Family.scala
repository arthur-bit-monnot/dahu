package dahu.benchmarks

import scala.collection.mutable

class Family(val familyName: String) {
  private var counter = 0
  val instancesMap: mutable.Map[String, SatProblem] = mutable.LinkedHashMap()

  def instance(instanceName: String)(f: => SatProblem): Unit = instances(instanceName)(Seq(f))

  def instances(instanceName: String)(f: => Seq[SatProblem]): Unit = {
    val pbs = f
    for(pb <- pbs) {
      val name = s"$counter-$instanceName"
      instancesMap += ((name, pb))
      counter += 1
    }
  }
}
