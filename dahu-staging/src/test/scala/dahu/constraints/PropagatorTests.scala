package dahu.constraints

import dahu.constraints.interval._
import dahu.expr._
import dahu.model.types.TagIsoInt
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop._
import org.scalactic.anyvals.PosZDouble
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

import scala.collection.mutable
import scala.language.implicitConversions

class PropagatorTests extends FunSuite with PropertyChecks {
  val numTrials = 10000

  // we assume that all types we deal with here are isomorphic to Int
  // i.e. all Tag[_] instance are in fact TagIsoInt[_] instances
  implicit def typetag2tagisohint(t: Type): TagIsoInt[_] = t.asInstanceOf[TagIsoInt[_]]
  implicit def typetag2tagisohint[F[_]](t: F[Type]): F[TagIsoInt[_]] =
    t.asInstanceOf[F[TagIsoInt[_]]]

  def generator[I](implicit tag: TagIsoInt[I]): Gen[I] =
    Gen.choose(tag.min, tag.max).map(tag.fromInt)

  val samples: mutable.Map[TagIsoInt[_], Seq[Int]] = mutable.HashMap()

  /** Creates a generator for domains of particular type. */
  def gen(tag: TagIsoInt[_]): Gen[Interval] = {
    // random and global point of interests when generating domains.
    // those are useful to make sure some generated instances are overlapping/close when
    // considering type with very large domains (e.g. integers)
    val poi =
      samples.getOrElseUpdate(tag, (0 to 5).flatMap(_ => Gen.choose(tag.min, tag.max).sample.toSeq))
    for {
      size   <- Gen.choose(-1, math.min(5, tag.numInstances))
      offset <- Gen.choose(0, 0) //size + 1)
      min    <- Gen.oneOf(tag.min, math.max(tag.min, -10))
      max    <- Gen.oneOf(tag.max, math.min(tag.max, 50))
      // choose starts points, biased around point of interest
      start <- Gen.chooseNum(min - offset, math.max(min, max - size) - offset, poi: _*)
      st = start + offset
      ed = start + offset + size - 1
      _ = {
        assert(tag.min <= st)
        assert(ed <= tag.max)
      }
    } yield Interval(st, ed)
  }

  /** Generator for a seq of domains to be used as the input of the given function.*/
  def inputGenerator(f: Fun[_]): Gen[Array[Interval]] = f match {
    case x: Fun1[_, _] => Gen.sequence[Array[Interval], Interval](Array(gen(x.inType)))
    case x: Fun2[_, _, _] =>
      Gen.sequence[Array[Interval], Interval](Array(gen(x.inType1), gen(x.inType2)))
    case x: FunN[_, _] => // uses an arbitrary number of arguments in [0, 4]
      Gen
        .choose(0, 4)
        .flatMap(n => Gen.sequence[Array[Interval], Interval](Array.fill(n)(gen(x.inTypes))))
  }
  def outputGenerator(f: Fun[_]): Gen[Interval] = gen(f.outType)

  def typeTags(f: Fun[_], domains: Seq[Interval]): Seq[TagIsoInt[_]] = f match {
    case x: Fun1[_, _]       => Seq(x.inType)
    case x: Fun2[_, _, _]    => Seq(x.inType1, x.inType2)
    case x: Fun3[_, _, _, _] => Seq(x.inType1, x.inType2, x.inType3)
    case x: FunN[_, _]       => Seq.fill(domains.size)(x.inTypes)
  }

  /** From a sequence of domains, generate all possible combination of inputs */
  def combinations(domains: Seq[Set[Int]], acc: Set[Seq[Int]] = Set(Seq())): Set[Seq[Int]] = {
    if(domains.isEmpty)
      return acc
    val accumulated = for(seq <- acc; value <- domains.head) yield seq :+ value
    combinations(domains.tail, accumulated)
  }
  test("combination generation") {
    assert(Set(Seq(1, 3), Seq(2, 3)) == combinations(Seq(Set(1, 2), Set(3))))
    assert(Set(Seq(1, 3), Seq(1, 4)) == combinations(Seq(Set(1), Set(3, 4))))
    assert(Set() == combinations(Seq(Set(1, 2), Set())))
  }

  def forward(f: Fun[_], fw: ForwardPropagator): Unit = {

    val inputsGen: Gen[Array[Interval]] = inputGenerator(f)

    // generate a set of inputs for the function
    val testSet: Array[Array[Interval]] = Array.fill(numTrials)(inputsGen.sample.toArray).flatten

    for(doms <- testSet) {
      val types        = typeTags(f, doms)
      val resultDomain = fw.propagate(doms, identity[Interval])

      // for all possible argument sequence from the domains, check that the result of the function is indeed in the
      // generated domain.
      val inputSet = combinations(doms.map(_.values.toSet))
      for(input <- inputSet) {
        val convertedInputs = types.zip(input).map { case (t, i) => Labels.Value(t.fromInt(i)) }
        val out             = f.compute(convertedInputs)
        val outInt          = f.outType.toIntUnsafe(out)
        assert(resultDomain.contains(outInt))
      }
    }
  }

  def backward(f: Fun[_], bw: BackwardPropagator): Unit = {
    val inputsGen: Gen[Array[Interval]] = inputGenerator(f)
    val outputGen: Gen[Interval]        = outputGenerator(f)

    val testSet: Array[(Array[Interval], Interval)] =
      Array
        .fill(numTrials)(
          inputsGen.sample
            .flatMap(inputs => outputGen.sample.map(output => (inputs, output)))
            .toArray)
        .flatten

    for((inputDomains, outputDomain) <- testSet) {
      val types         = typeTags(f, inputDomains)
      val resultDomains = bw.propagate(inputDomains, outputDomain, identity[Interval])

      // check that propagation never increases the domain, which in fact enforced by the CSP and not a requirement for propagators
      // inputDomains.zip(resultDomains).foreach{
      //   case (in, out) => assert(in.contains(out))
      // }
      val removedValues: Array[Set[Int]] = inputDomains.zip(resultDomains).map {
        case (in, out) => in.values.toSet.filterNot(out.contains(_))
      }

      // if a value was removed from a domain, check that it could not be used to generate a value in the function's codomain
      for(focus <- removedValues.indices) {
        val testInputDomains =
          removedValues.indices.map(i =>
            if(i == focus) removedValues(i) else inputDomains(i).values.toSet)
        val testInputs = combinations(testInputDomains)
        for(input <- testInputs) {
          val convertedInputs = types.zip(input).map { case (t, i) => Labels.Value(t.fromInt(i)) }
          val out             = f.compute(convertedInputs)
          val outInt          = f.outType.asInstanceOf[TagIsoInt[_]].toIntUnsafe(out)

          // for debug
          val inDomainsStr: String = inputDomains.map(_.show).mkString("[", ", ", "]")
          val outDomainStr: String = outputDomain.show

          assert(!outputDomain.contains(outInt), s"${outputDomain.show} does contains $outInt")
        }

      }
    }
  }

  val functions = Seq(
    int.LEQ,
    int.EQ,
    int.Add,
    bool.And,
    bool.Or,
    bool.Not
  )

  for(f <- functions) {
    check(f)
  }

  def check(f: Fun[_]): Unit = {
    test(s"$f: => default forward propagator") {
      forward(f, Propagator.forward(f))
    }
    test(s"$f: <= default backward propagator") {
      backward(f, Propagator.backward(f))
    }
  }
}
