package dahu.utils

import structures._
import debox._
import spire.implicits._
import utest._

object StructureTests extends TestSuite {

  override def tests = Tests {
    "debox-buffer" - {
      "indices" - {
        assert(Buffer(1, 2, 3).indices == Seq(0, 1, 2))
        assert(Buffer[Int]().indices == Seq())
        assert(Buffer(10).indices == Seq(0))
      }

      "forall" - {
        val buff = Buffer(1, 2, 3, 4, 5)
        assert(buff.forall(_ < 6))
        assert(!buff.forall(_ < 5))
        assert(!buff.forall(_ < 3))
      }

      "sortby" - {
        val buff = Buffer(3, 2, 1)
        assert(buff.sortedBy(-_) == buff)
        assert(buff.sortedBy(identity) == Buffer(1, 2, 3))

      }
    }
  }
}
