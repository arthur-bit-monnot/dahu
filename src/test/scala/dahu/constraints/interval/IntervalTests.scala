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
      assert(I(0, 10).intersection(I(5, 20)) == I(5, 10))
      assert(I(0, 2).intersection(I(2, 5)) == I(2, 2))
      assert(I(0, 5).intersection(I(6, 10)) == empty)
      assert(I(-100, 100).intersection(empty) == empty)
    }

    "union" in {
      assertResult(I(0,10))(empty union I(0, 10))
      assertResult(I(0,10))(I(0, 10) union empty)
      assertResult(I(0, 20))(I(0, 10) union I(11, 20))
      assertResult(I(0, 20))(I(0, 10) union I(4, 20))
    }

    "values" in {
      assert(Seq() == empty.values)
      assert(Seq(0, 1, 2, 3) == I(0, 3).values)
    }

    "without" in {
      assert(empty \ I(0, 0) == empty)
      assert(I(0, 10) \ empty == I(0,10))
      assert(I(0, 10) \ I(5,10) == I(0,4))
      assert(I(0, 10) \ I(-5,-1) == I(0, 10))
    }

    "min" in {
      assert(I(0, 10).min(I(5, 7)) == I(0, 7))
      assert(I(0, 10).min(I(20, 25)) == I(0, 10))
      assert(I(0, 10).min(empty) == empty)
    }

  }
}
