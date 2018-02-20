package dahu.arrows

import dahu.utils.debug._
import utest._

object ArrowsTests extends TestSuite {
  /** Alias to the regular scala assert since the utest macro seems to create typing problems in some cases. */
  def sassert(x: Boolean) = scala.Predef.assert(x)

  implicit class CompileErrorOps(val x: CompileError) extends AnyVal {
    def isTypeError: Unit = x match {
      case CompileError.Type(_, _) => ()
      case _ => assert(false)
    }
  }

  def tests = Tests {
    "arrows" - {

      "dependent-types" - {
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

        val typeTest: debox.Set[isOdd.K] = isOdd.domain

        val isEven = isOdd.map(!_)

        implicitly[isOdd.K =:= isEven.K]
        val typeTest2: debox.Set[isOdd.K]  = isEven.domain
        val typeTest3: debox.Set[isEven.K] = isEven.domain

        for(i <- isOdd.domain) {
          if(i % 2 == 0) {
            sassert(isOdd(i) && !isEven(i))
          } else {
            sassert(!isOdd(i) && isEven(i))
          }
        }

        assert(isEven.get(0) == Some(false))
        assert(isEven.get(1) == Some(true))
        assert(isEven.get(10) == None)
      }

      "builder" - {
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

      "subtyping" - {
        val isOdd = ArrayIntFunc.build(0 until 10, _ % 2 == 0)

        val odds  = isOdd.filter(identity)
        val evens = isOdd.filter(!_)

        sassert(odds.domain.forall(_ % 2 == 0))
        implicitly[odds.K <:< isOdd.K]

        compileError("implicitly[odds.Key =:= isOdd.Key]")

        implicitly[evens.K <:< isOdd.K]
        compileError("implicitly[evens.Key =:= isOdd.Key]")

        compileError("implicitly[evens.Key =:= odds.Key]")
        compileError("implicitly[odds.Key <:< evens.Key]")
        compileError("implicitly[evens.Key <:< odds.Key]")

        for(i <- odds.domain) {
          sassert(odds(i))
          sassert(isOdd(i)) // compiler checks that odds.K <: isOdd.K
          compileError("evens(i)").isTypeError
        }
        val x: evens.K = evens.domain.toIterable().head
        compileError("val y: odds.Key = evens.domain.toIterable().head").isTypeError
        ()
      }
    }
  }

}
