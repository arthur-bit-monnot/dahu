package dahu.constraints

import dahu.constraints.domains.{IntDomain, IntervalDomain}
import org.scalacheck._
import org.scalacheck.Prop._
//import org.scalacheck.Prop.BooleanOperators
//import org.scalatest.FreeSpec
//import org.scalatest.prop.PropertyChecks

object DomainProperties extends Properties("test") {

  implicit val arbDom: Arbitrary[IntDomain] = Arbitrary(for {
    size <- Gen.choose(-1, 5)
    start <- Gen.choose(0, 1000)
  } yield IntervalDomain(start, start + size))

  property("size") = forAll { (a: IntDomain) =>
    !a.isEmpty || (a.size == 0)
  }

  property("intersection") = forAll { (a: IntDomain, b: IntDomain) =>
    val inter = a & b
    (inter.size <= a.size + b.size) &&
    a.values.forall(v => !b.contains(v) || inter.contains(v))

  }

  this.properties

}
