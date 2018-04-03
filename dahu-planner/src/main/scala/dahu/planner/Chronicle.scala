package dahu.planner

import copla.lang.model.core._
import dahu.model.functions.WrappedFunction
import dahu.model.input._
import dahu.model.math.{bool, int}
import dahu.model.types.TagIsoInt

import scala.collection.mutable

case class Bind(cst: Any, variable: Any)

case class ProblemContext(topTag: TagIsoInt[Instance],
                          specializedTags: Type => TagIsoInt[Instance]) {

  private val Equality = WrappedFunction.wrap(int.EQ)(topTag, implicitly[TagIsoInt[Boolean]])

  def encode(v: Var): Tentative[Instance] =
    v match {
      case lv @ LocalVar(_, tpe) => Input(Ident(lv))(specializedTags(tpe))
      case i @ Instance(_, tpe)  => Cst(i)(specializedTags(tpe))
    }

  def eqv(lhs: Var, rhs: Var): Tentative[Boolean] = eqv(encode(lhs), encode(rhs))
  def eqv(lhs: Tentative[Instance], rhs: Tentative[Instance]): Tentative[Boolean] =
    Computation(Equality, lhs, rhs)

  def neq(lhs: Var, rhs: Var): Tentative[Boolean] = neq(encode(lhs), encode(rhs))
  def neq(lhs: Tentative[Instance], rhs: Tentative[Instance]): Tentative[Boolean] =
    Computation(bool.Not, eqv(lhs, rhs))

}

object ProblemContext {
  def extract(m: Seq[InModuleBlock]): ProblemContext = {
    val types = m.collect { case TypeDeclaration(t) => t }
    val subtypes = mutable.LinkedHashMap[Type, mutable.Set[Type]]()
    val instances = m
      .collect { case InstanceDeclaration(i) => i }
      .map { case i @ Instance(_, t) => (t, i) }
      .groupBy(_._1)
      .mapValues(vs => vs.map(_._2).sortBy(_.id.name))

    for(t <- types) {
      subtypes.getOrElseUpdate(t, mutable.Set())
      t.parent match {
        case Some(parent) =>
          subtypes.getOrElseUpdate(parent, mutable.Set()) += t
        case None =>
      }
    }
    val x = mutable.ArrayBuffer[Type]()
    val roots = types.collect { case t @ Type(_, None) => t }.toList

    def proccess(t: Type): Unit = {
      assert(!x.contains(t))
      x += t
      subtypes(t).foreach(proccess)
    }
    roots.foreach(proccess)

    val tmp = x.toList.flatMap(t => instances.getOrElse(t, Seq()).toList).zipWithIndex
    val fromIndex = tmp.map(_.swap).toMap
    val toIndex = tmp.toMap
    assert(toIndex.size == fromIndex.size)

    def tagOf(t: Type): TagIsoInt[Instance] = {
      def instancesOf(t: Type): Seq[Instance] =
        instances.getOrElse(t, Seq()) ++ subtypes(t).flatMap(instancesOf)
      def continuousMinMax(is: Seq[Instance]): (Int, Int) = {
        val sorted = is.sortBy(t => toIndex(t)).toList
        sorted match {
          case Nil          => (0, -1)
          case head :: tail =>
            // assert that all instances have a continuous index space
            var curr = toIndex(head)
            for(next <- tail) {
              assert(toIndex(next) == curr + 1)
              curr = curr + 1
            }
            (toIndex(sorted.head), toIndex(sorted.last))
        }
      }
      val is = instancesOf(t)
      val (minId, maxId) = continuousMinMax(is)

      new TagIsoInt[Instance] {
        override def toInt(t: Instance): Int = {
          val ret = toIndex(t)
          assert(min <= ret && ret <= max)
          ret
        }

        override def fromInt(i: Int): Instance = {
          assert(min <= i && i <= max)
          fromIndex(i)
        }

        override val max: Int = maxId
        override val min: Int = minId

        override def typ: Tag.Type = ???

        override def toString: String = s"${t.id}[$min,$max]"
      }
    }
    val XX = x.map(t => (t, tagOf(t)))
    println(XX)

    val memo = mutable.Map[Type, TagIsoInt[Instance]]()
    val specializedTag = (t: Type) => memo.getOrElseUpdate(t, tagOf(t))
    val topTag = new TagIsoInt[Instance] {
      override def toInt(t: Instance): Int = {
        val ret = toIndex(t)
        assert(min <= ret && ret <= max)
        ret
      }

      override def fromInt(i: Int): Instance = {
        assert(min <= i && i <= max)
        fromIndex(i)
      }

      override val min: Int = toIndex.values.min
      override val max: Int = toIndex.values.max

      override def typ: Tag.Type = ???

      override def toString: String = s"TOP[$min,$max]"
    }

    ProblemContext(topTag, specializedTag)
  }
}

case class Chronicle(ctx: ProblemContext,
                     binds: Map[Constant, Tentative[Instance]],
                     constraints: List[Tentative[Boolean]]) {

  import ctx._

  def extended(e: InModuleBlock): Chronicle = e match {
    case _: TypeDeclaration      => this
    case _: InstanceDeclaration  => this
    case _: TimepointDeclaration => this
    case _: FunctionDeclaration  => this
    case _: LocalVarDeclaration  => this
    case BindAssertion(c, v) =>
      binds.get(c) match {
        case Some(other) =>
          this.copy(
            constraints = ctx.eqv(encode(v), other) :: constraints
          )
        // TODO: handled case with parameters
        case None =>
          this.copy(
            binds = binds.updated(c, encode(v))
          )
      }
    case StaticDifferentAssertion(lhs, rhs) =>
      copy(
        constraints = ctx.neq(lhs, rhs) :: constraints
      )
  }

  def toSatProblem = {
    val prod = Product.fromMap(binds)
    SubjectTo(Product.fromMap(binds), Computation(bool.And, constraints))
  }
}

object Chronicle {
  def empty(ctx: ProblemContext): Chronicle = Chronicle(ctx, Map(), Nil)
}
