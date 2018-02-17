package dahu.constraints.interval

import org.scalatest.FreeSpec

class IntervalTests extends FreeSpec {
  val I     = Interval
  val empty = I.empty

  "interval" - {

    "building" in {

      assert(I(0, -1) == I(0, -2))
      assert(I(-5, 10).lb == -5)
      assert(I(-5, 10).ub == 10)
    }

    "empty" in {
      assert(empty.size == 0)
      assert(I(10, 5) == empty)
    }

    "size" in {
      assert(I(0, 10).size == 11)
      assert(I(10, 5).size == 0)
    }

    "intersection" in {
      assert(I(0, 10).inter(I(5, 20)) == I(5, 10))
      assert(I(0, 2).inter(I(2, 5)) == I(2, 2))
      assert(I(0, 5).inter(I(6, 10)) == empty)
      assert(I(-100, 100).inter(empty) == empty)
    }

    "union" in {
      assertResult(I(0, 10))(empty unionApproximation I(0, 10))
      assertResult(I(0, 10))(I(0, 10) unionApproximation empty)
      assertResult(I(0, 20))(I(0, 10) unionApproximation I(11, 20))
      assertResult(I(0, 20))(I(0, 10) unionApproximation I(4, 20))
      assert(I(0, 10).unionApproximation(I(20, 30)) == I(0, 30),
             "Expected behavior due to approximation")
    }

    "without" in {
      assert(empty.withoutApproximation(I(0, 0)) == empty)
      assert(I(0, 10).withoutApproximation(empty) == I(0, 10))
      assert(I(0, 10).withoutApproximation(I(5, 10)) == I(0, 4))
      assert(I(0, 10).withoutApproximation(I(-5, -1)) == I(0, 10))
      assert(I(0, 1).withoutApproximation(I(0)) == I(1))
      assert(I(0, 1).withoutApproximation(I(1)) == I(0))
      assert(I(0, 2).withoutApproximation(I(1)) == I(0, 2), "Expected result due to approximation")
    }

    "values" in {
      assert(Seq() == empty.values)
      assert(Seq(0, 1, 2, 3) == I(0, 3).values)
    }

    "min" in {
      assert(I(0, 10).min(I(5, 7)) == I(0, 7))
      assert(I(0, 10).min(I(20, 25)) == I(0, 10))
      assert(I(0, 10).min(empty) == empty)
    }

    "max" in {
      assert(I(0, 10).max(I(5, 7)) == I(5, 10))
      assert(I(0, 10).max(I(20, 25)) == I(20, 25))
      assert(I(0, 10).max(empty) == empty)
    }

    "minus" in {
      assert(I(0, 3).minus(I(1)) == I(-1, 2))
      assert(I(0, 3).minus(I(0, 1)) == I(-1, 3))

      val i1 = I(45, 49)
      val i2 = I(45)
      val o  = I(92)
      assert((i1 inter (o minus i2)) == I(47))
      assert((i2 inter (o minus i1)) == I(45))
    }

  }
}
