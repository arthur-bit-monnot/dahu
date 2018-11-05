package dahu.refinement

object common {
  type Addr = Int
  type R = Double

  type Params = Array[Addr]
  type Values = Array[R]

  /** Infinitesimal change for computing finite differences.
    * Equal sqrt(10**-16) = 10**-8 (see Numerical Optimization Chap 7.1 for explanation)  */
  val epsilon: R = 1e-5
}
