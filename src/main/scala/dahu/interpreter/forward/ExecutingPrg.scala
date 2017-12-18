package dahu.interpreter.forward

import dahu.interpreter._
import dahu.interpreter.ast.AST

import scala.collection.mutable
import scala.util.control.NonFatal
import cats.syntax.either._
import dahu.dataframe.{ColumnF, ColumnView}
import dahu.dataframe.vector.NullableVec
import dahu.dataframe.vector.mutable.NullableArray
import dahu.utils.Errors._


final case class PrgState private (memory: Vector[V])
object PrgState {
  def init(code: AST): PrgState = {

    val mem = code(AST.ExprKey).values.map {
      case cst: Cst => cst.value
      case _        => null
    }
    PrgState(mem.toVector)
  }
}

final class ExecutionState(ast: AST) {
  val topo: Array[VarID] = dahu.utils.Graph.topologicalOrder(ast.dependencyGraph.zipWithIndex.map(_.swap).toMap)
      .getOrElse(sys.error("AST is not a DAG")).toArray

  val memory: NullableArray[V] = NullableArray.ofSize[V](topo.size)

  {
    var i = 0
    while(i < memory.size) {
      ast.code.vector(i) match {
        case cs: Cst => memory.set(i, cs.value)
        case _ =>
      }
      i += 1
    }
  }

  private var nextInTopo = 0
  def cur: VarID = topo(nextInTopo)
  def hasNext: Boolean = nextInTopo < topo.length-1
  def gotoNext(): Unit = nextInTopo += 1

  def update(v: VarID, value: V): Unit = {
    assert(value != null)
    assert(v < memory.size)
    assert(!memory.isSet(v))

    memory.set(v, value)
    while(memory.isSet(cur) && hasNext) {
      gotoNext()
      ast.code.vector(cur) match {
        case f: Fun =>
          assert(f.args.forall(memory.isSet), "One of the dependencies of a function is not set when reaching it")
          val evaluated = f.fun.compute(f.args.map(memory.getUnsafe))
          memory.set(cur, evaluated)
        case _ =>
      }
    }
  }

  def get(v: VarID) = memory.get(v)
}

final case class ExecutingPrg private (ast: AST, partiallyEvaluated: Vector[V]) {
  type F[x] = Vector[x]
  private val vec : NullableVec[F] = implicitly[NullableVec[F]]

  def get(v: VarID): Res[V] = {
    if(v >= vec.size(partiallyEvaluated))
      Left(Err(s"Address out of Memory: $v"))
    else
      vec.get(partiallyEvaluated, v).toRight(err(s"Not evaluated yet: $v"))
  }

  def update(v: VarID, value: V): Res[ExecutingPrg] = {
    val stack = mutable.Set[(VarID, V)]((v, value))

    var memory = partiallyEvaluated
    try {

      def initialized(v: VarID): Boolean = memory(v) != null

      def computable(v: FunID): Boolean = {
        ast.funAt(v).valueOr(throw _).args.forall(initialized)
      }

      def eval(v: FunID): V = {
        val f    = ast.funAt(v).valueOr(throw _)
        val args = f.args.map(memory(_))
        assert(!args.contains(null))
        f.fun.compute(args)
      }

      while(stack.nonEmpty) {
        val p @ (variable, value) = stack.head
        stack -= p
        if(value != memory(variable)) {
          memory = memory.updated(variable, value)
          val dependencies           = ast.dependencyGraph(variable)
          val dependenciesWithArgs   = dependencies.map(x => ast.at(x))
          val computableDependencies = dependencies.filter(fID => computable(fID))
          for(fid <- computableDependencies) {
            stack += ((fid, eval(fid)))
          }
        }
      }
      Right(this.copy(partiallyEvaluated = memory))
    } catch {
      case NonFatal(e) => Left(e)
    }
  }
}

object ExecutingPrg {

  def apply(ast: AST)(implicit vec: NullableVec[Vector]) = new ExecutingPrg(ast, vec.init(ast.code.vector.size))
}
