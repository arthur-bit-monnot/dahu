package dahu.benchmarks

import dahu.model.input._
import dahu.model.input.dsl._

object Jobshop extends Family("jobshop") {

  val START = Cst(1)
  val END = Input[Int]("makespan").subjectTo(START <= _)

  def input(): Expr[Int] = Input()
  def tp()= input().subjectTo(x => START <= x && x <= END)

  case class Job(jobNumber: Int,
                 numInJob: Int,
                 duration: Int,
                 interval: Interval,
                 machine: Expr[Int])

  final case class JobShopInstance(numMachines: Int,
                                   jobs: Seq[Seq[Int]],
                                   optimalMakespan: Option[Int]) {

    def numJobs = jobs.size
  }

  case class Interval(start: Expr[Int], end: Expr[Int]) {
    def duration: Expr[Int] = end - start
    def <(o: Interval): Expr[Boolean] = end < o.start
    def >(o: Interval): Expr[Boolean] = o < this
  }
  def interval(duration: Int): Interval = {
    val start = tp()
    val end = tp().subjectTo(x => x === start + Cst(duration) -1)
    Interval(start, end)
  }

  def jobShopModel(instance: JobShopInstance): SatProblem = {
    val jobs =
      for(i <- instance.jobs.indices; j <- instance.jobs(i).indices) yield {
        val int = interval(instance.jobs(i)(j))
        val machine = Input[Int](s"machine($i,$j)").subjectTo(x => x >= 1 && x <= instance.numMachines)

        Job(i, j, instance.jobs(i)(j), int, machine)
      }

    val constraint = jobs.indices
      .map { i =>
        val job = jobs(i)
        job.interval.duration === Cst(job.duration - 1) && // TODO: we should be able to remove it
          (if(job.numInJob >= 1) jobs(i - 1).interval.end < job.interval.start else Cst(true))
      }
      .fold(Cst(true))(_ && _)

    val nonOverlappingSeq: Seq[Expr[Boolean]] = for(j1 <- jobs; j2 <- jobs; if j1 != j2) yield {
      j1.machine =!= j2.machine || j1.interval < j2.interval || j1.interval > j2.interval
    }
    val nonOverlapping = nonOverlappingSeq.fold(Cst(true))(_ && _)

    SatProblem.fromSat(constraint && nonOverlapping, NumSolutions.AtLeast(1))
  }

  val problems = Seq(
    JobShopInstance(1, List(List(2)), None),
    JobShopInstance(1, List(List(2, 4)), None),
    JobShopInstance(1, List(List(2), List(4)), None),
    JobShopInstance(2, List(List(2), List(4)), None),
    JobShopInstance(1, List(List(2, 2), List(4)), None),
    JobShopInstance(2, List(List(2, 2), List(4)), None),
     JobShopInstance(2, List(List(2, 4), List(4, 3)), None),
     JobShopInstance(2, List(List(2, 4), List(4, 3, 3)), None),
     JobShopInstance(4, List(List(2, 4, 2, 1), List(5, 3, 2), List(3, 5, 7)), Some(14)),
  )

  instances("simple")(problems.map(jobShopModel))

}
