package dahu.constraints

import org.scalacheck._
import org.scalacheck.Prop._
//import org.scalacheck.Prop.BooleanOperators
//import org.scalatest.FreeSpec
//import org.scalatest.prop.PropertyChecks

object DomainProperties extends Properties("test")  {

  implicit val arbDom = Arbitrary(for {
    size <- Gen.choose(-1, 5)
    start <- Gen.choose(0, 1000)
  } yield IntDomain(start, start+size) )

  property("size") =  forAll { (a: IntDomain) =>
    new ExtendedBoolean(a.isEmpty) ==> (a.size == 0)
  }

  property("intersection") = forAll { (a: IntDomain, b: IntDomain) =>
    (a inter b).size <= a.size + b.size

  }

  this.properties

}
