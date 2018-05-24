package dahu.utils

import utest._
import scala.collection.immutable.Iterable

object BagTests extends TestSuite {

  override def tests = Tests {

    "does not loose items" - {
      val l = List(1, 2, 3)
      Bag(l).toSeq.sorted ==> l.sorted

      val l2 = Set(1, 2, 3)
      Bag(l2).toSet ==> l2
      Bag(l2).size  ==> l2.size
    }

    "iterables" - {
      "list" - {
        val l1 = List(1, 2, 3)
        val l2 = List(4, 5, 6)
        Bag.fromIterables(l1 :: l2 :: Nil).toSeq.sorted ==> List(1, 2, 3, 4, 5, 6)
      }
      "bags" - {
        val l1 = Bag.empty
        val l2 = Bag(1, 2, 3)
        val l3 = Bag.empty

        Bag.fromIterables(l2 :: Nil).toSeq.sorted             ==> List(1, 2, 3)
        Bag.fromIterables(l2 :: l3 :: Nil).toSeq.sorted       ==> List(1, 2, 3)
        Bag.fromIterables(l1 :: l2 :: Nil).toSeq.sorted       ==> List(1, 2, 3)
        Bag.fromIterables(l1 :: l2 :: l3 :: Nil).toSeq.sorted ==> List(1, 2, 3)
        Bag.fromIterables(Iterable(l1, l2, l3)).toSeq.sorted  ==> List(1, 2, 3)
      }
    }

  }
}
