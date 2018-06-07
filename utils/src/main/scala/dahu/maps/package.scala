package dahu

import dahu.utils._
import algebra.Order

import scala.reflect.ClassTag

package object maps {

  class Counter {
    type ID = Int with Wrapped[this.type]
    private var nextValue: Int = 0
    def next(): ID = { nextValue += 1; nextValue.asInstanceOf[ID] }
  }
  object Counter {
    type Aux[ID0] = Counter { type ID = ID0 }
  }
}
