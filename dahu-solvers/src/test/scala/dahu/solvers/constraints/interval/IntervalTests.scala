package dahu.constraints.interval

import utest._

object IntervalTests extends TestSuite {
  private val I = Interval
  private val empty = I.empty

  def tests = Tests {
    "interval" - {

      "building" - {
        assert(I(0, -1) == I(0, -2))
        I(-5, 10).lb ==> -5
        I(-5, 10).ub ==> 10
      }

      "empty" - {
        empty.size ==> 0
        I(10, 5)   ==> empty
      }

      "size" - {
        I(0, 10).size ==> 11
        I(10, 5).size ==> 0
      }

      "intersection" - {
        I(0, 10).inter(I(5, 20))  ==> I(5, 10)
        I(0, 2).inter(I(2, 5))    ==> I(2, 2)
        I(0, 5).inter(I(6, 10))   ==> empty
        I(-100, 100).inter(empty) ==> empty
      }

      "union" - {
        (empty unionApproximation I(0, 10))     ==> I(0, 10)
        (I(0, 10) unionApproximation empty)     ==> I(0, 10)
        (I(0, 10) unionApproximation I(11, 20)) ==> I(0, 20)
        (I(0, 10) unionApproximation I(4, 20))  ==> I(0, 20)
        I(0, 10).unionApproximation(I(20, 30))  ==> I(0, 30) //Expected behavior due to approximation
      }

      "without" - {
        empty.withoutApproximation(I(0, 0))      ==> empty
        I(0, 10).withoutApproximation(empty)     ==> I(0, 10)
        I(0, 10).withoutApproximation(I(5, 10))  ==> I(0, 4)
        I(0, 10).withoutApproximation(I(-5, -1)) ==> I(0, 10)
        I(0, 1).withoutApproximation(I(0))       ==> I(1)
        I(0, 1).withoutApproximation(I(1))       ==> I(0)
        I(0, 2).withoutApproximation(I(1))       ==> I(0, 2) // Expected result due to approximation
      }

      "values" - {
        empty.values   ==> Seq()
        I(0, 3).values ==> Seq(0, 1, 2, 3)
      }

      "min" - {
        I(0, 10).min(I(5, 7))   ==> I(0, 7)
        I(0, 10).min(I(20, 25)) ==> I(0, 10)
        I(0, 10).min(empty)     ==> empty
      }

      "max" - {
        I(0, 10).max(I(5, 7))   ==> I(5, 10)
        I(0, 10).max(I(20, 25)) ==> I(20, 25)
        I(0, 10).max(empty)     ==> empty
      }

      "minus" - {
        I(0, 3).minus(I(1))    ==> I(-1, 2)
        I(0, 3).minus(I(0, 1)) ==> I(-1, 3)

        val i1 = I(45, 49)
        val i2 = I(45)
        val o = I(92)
        (i1 inter (o minus i2)) ==> I(47)
        (i2 inter (o minus i1)) ==> I(45)
      }
    }
  }
}
