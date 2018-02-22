package dahu.benchmarks

import dahu.model.compiler.Algebras
import dahu.model.input._
import dahu.model.input.dsl._

object Jobshop extends Family("jobshop") {
  private var varCounter = -1

  val START = Cst(0)
  val END = input().subjectTo(START <= _)

  def input(): Input[Int] = { varCounter += 1; Input(s"_v$varCounter") }
  def tp(): SubjectTo[Int] = input().subjectTo(x => START <= x && x <= END)

  case class Job(jobNumber: Int,
                 numInJob: Int,
                 duration: Int,
                 interval: Interval,
                 machine: Input[Int])

  final case class JobShopInstance(numMachines: Int,
                                   jobs: Seq[Seq[Int]],
                                   optimalMakespan: Option[Int]) {

    def numJobs = jobs.size
  }

  case class Interval(start: Input[Int], end: Input[Int]) {
    def duration: Expr[Int] = end - start
    def <(o: Interval): Expr[Boolean] = end < o.start
    def >(o: Interval): Expr[Boolean] = o < this
  }
  def interval(): Interval = Interval(input(), input())

  def jobShopModel(instance: JobShopInstance): SatProblem = {
    val jobs =
      for(i <- instance.jobs.indices; j <- instance.jobs(i).indices) yield {
        val int = interval()
        val machine = Input[Int](s"machine($i,$j)") // todo: (1 to instance.numMachines).toSet)

        Job(i, j, instance.jobs(i)(j), int, machine)
      }

    val constraint = jobs.indices
      .map { i =>
        val job = jobs(i)
        job.interval.duration === job.duration - 1 &&
        Cst(job.numInJob >= 1) ==> (jobs(i - 1).interval.end < job.interval.start) &&
        job.machine >= 1 && job.machine <= instance.numMachines

      }
      .fold(Cst(true))(_ && _)

    val nonOverlappingSeq: Seq[Expr[Boolean]] = for(j1 <- jobs; j2 <- jobs; if j1 != j2) yield {
      j1.machine =!= j2.machine || j1.interval < j2.interval || j1.interval > j2.interval
    }
    val nonOverlapping = nonOverlappingSeq.fold(Cst(true))(_ && _)

    SatProblem(constraint && nonOverlapping, NumSolutions.AtLeast(1))
  }

  val problems = Seq(
    JobShopInstance(1, List(List(2)), None),
    JobShopInstance(1, List(List(2, 4)), None),
    JobShopInstance(1, List(List(2), List(4)), None),
    JobShopInstance(2, List(List(2), List(4)), None),
    JobShopInstance(1, List(List(2, 2), List(4)), None),
    JobShopInstance(2, List(List(2, 2), List(4)), None)
    // JobShopInstance(2, List(List(2, 4), List(4, 3)), None), // very simple instance to avoid taking time in unit tests
    // JobShopInstance(2, List(List(2, 4), List(4, 3, 3)), None), // very simple instance to avoid taking time in unit tests
    // JobShopInstance(4, List(List(2, 4, 2, 1), List(5, 3, 2), List(3, 5, 7)), Some(14)),
  )

  instances("simple")(problems.map(jobShopModel))

}
