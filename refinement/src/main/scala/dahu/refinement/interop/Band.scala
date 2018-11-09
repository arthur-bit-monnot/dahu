package dahu.refinement.interop

trait Types {
  type DS
  type S
  type S1
  type S2
  type T
  type B

  type Constraint0 = DS => S => B
  type Constraint1 = DS => S1 => B
  type Constraint2 = DS => S2 => B
}

class BandsContext[X <: Types](val types: X) {
  import types._

  case class TimedState(state: S, timestamp: T)

  case class Band(
      dstate: DS,
      cstates: Seq[TimedState] // pointer to continuous state and timestamp
  ) {

    def states0: Seq[S] = cstates.map(_.state)

    def states1(f: (S, S, T) => S1): Seq[S1] = {
      for(i <- 0 until cstates.size - 1) yield {
        val c1 = cstates(i)
        val c2 = cstates(i + 1)
        f(c1.state, c2.state, c2.timestamp)
      }
    }

    def states2(f: (S, S, S, T, T) => S2, previous: S, next: S): Seq[S2] = {
      val cs = previous +: cstates :+ next
      for(i <- 0 until cs.size - 2) yield {
        val c1 = cstates(i)
        val c2 = cstates(i + 1)
        val c3 = cstates(i + 2)
        f(c1.state, c2.state, c3.state, c2.timestamp, c3.timestamp)
      }
    }
  }

  case class Bands(
      bands: Seq[Band],
      first: S,
      last: S
  )

  def init(discretes: Seq[DS], genS: () => S, genT: () => T, first: S): Bands = {

    val (last, bands) = discretes.foldLeft((first, List[Band]())) {
      case ((previous, bands), ds) =>
        val lastOfThis = genS()
        val band = Band(ds,
                        Seq(
                          TimedState(previous, genT()),
                          TimedState(lastOfThis, genT())
                        ))
        (lastOfThis, bands :+ band)
    }

    Bands(bands, first, last)
  }

}

object BandTests extends App {

  object types extends Types {
    override type DS = Unit
    override type S = Int
    override type T = Int
  }

  import types._
  var nextS = 0
  var nextT = 0
  def genS(): S = { nextS += 1; nextS - 1 }
  def genT(): S = { nextT += 1; nextT - 1 }

  val ctx = new BandsContext[types.type](types)

  val bands = ctx.init(Seq((), ()), genS, genT, genS())

  println(bands)
}
