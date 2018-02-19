package dahu.arrows

import org.scalatest.FreeSpec

class ArrowsTests extends FreeSpec {

  "arrows" - {

    "dependent-types" in  {
      val builder = new IntFuncBuilder[Boolean]
      for(i <- 0 until 10) {
        builder += (i, i % 2 == 0)
      }
      val isOdd: ArrayIntFunc[Boolean] =
        ArrayIntFunc.build(0 until 10, _ % 2 == 0)

      assert(isOdd.domain.size == 10)
      assert((0 until 10).forall(isOdd.isInDomain))
      assert(!isOdd.isInDomain(-1))
      assert(!isOdd.isInDomain(10))

      val typeTest: debox.Set[isOdd.Key] = isOdd.domain

      val isEven = isOdd.map(!_)

      implicitly[isOdd.Key =:= isEven.Key]
      val typeTest2: debox.Set[isOdd.Key] = isEven.domain
      val typeTest3: debox.Set[isEven.Key] = isEven.domain

      for(i <- isOdd.domain) {
        if(i % 2 == 0) {
          assert(isOdd(i) && !isEven(i))
        } else {
          assert(!isOdd(i) && isEven(i))
        }
      }

      assert(isEven.get(0) == Some(false))
      assert(isEven.get(1) == Some(true))
      assert(isEven.get(10) == None)
    }


    "builder" in {
      val builder = new IntFuncBuilder[Boolean]
      for(i <- 0 until 10) {
        builder += (i, i % 2 == 0)
      }
      val isOddFromBuilder = builder.toImmutableArray

      val isOdd: ArrayIntFunc[Boolean] =
        ArrayIntFunc.build(0 until 10, _ % 2 == 0)

      assert(isOdd.hashCode() == isOddFromBuilder.hashCode())
      assert(isOdd == isOddFromBuilder)

      val isEven = isOdd.map(!_)
      assert(isOdd != isEven)
    }

    "subtyping" in {
      val isOdd = ArrayIntFunc.build(0 until 10, _ % 2 == 0)

      val odds = isOdd.filter(identity)
      val evens = isOdd.filter(!_)

      assert(odds.domain.forall(_ % 2 == 0))
      implicitly[odds.Key <:< isOdd.Key]
      assertDoesNotCompile("implicitly[odds.Key =:= isOdd.Key]")

      implicitly[evens.Key <:< isOdd.Key]
      assertDoesNotCompile("implicitly[evens.Key =:= isOdd.Key]")

      assertDoesNotCompile("implicitly[evens.Key =:= odds.Key]")
      assertDoesNotCompile("implicitly[odds.Key <:< evens.Key]")
      assertDoesNotCompile("implicitly[evens.Key <:< odds.Key]")

      for(i <- odds.domain) {
        assert(odds(i))
        assert(isOdd(i)) // checks that odds.
        assertTypeError("evens(i)")
      }
      val x: evens.Key = evens.domain.toIterable().head
      assertTypeError("val y: odds.Key = evens.domain.toIterable().head")

    }
  }

}
