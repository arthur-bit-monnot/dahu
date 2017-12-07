package dahu.interpreter.forward

import dahu.interpreter._
import dahu.interpreter.ast.AST

import scalaz._
import Scalaz._
import scala.collection.mutable
import scala.util.control.NonFatal

final case class PrgState private(memory: Vector[V])
object PrgState {
  def init(code: Code): PrgState = {
    val mem = code.forward.map {
      case cst: Cst => cst.value
      case _ => null
    }
    PrgState(mem)
  }
}

final case class ExecutingPrg(ast: AST, state: PrgState) {

  def get(v: VarID): Res[V] = {
    state.memory.apply(v).right
  }

  def update(v: VarID, value: V): Res[ExecutingPrg] = {
    if(v >= state.memory.size)
      return Err(s"Address out of Memory: $v").left

    val stack = mutable.Set[(VarID, V)]((v, value))

    var memory = state.memory
    try {

      def initialized(v: VarID): Boolean = memory(v) != null

      def computable(v: FunID): Boolean = {
        println(s"$v ${ast.funAt(v).valueOr(throw _).args.map(initialized)}")
        ast.funAt(v).valueOr(throw _).args.forall(initialized)
      }

      def eval(v: FunID): V = {
        val f = ast.funAt(v).valueOr(throw _)
        val args = f.args.map(memory(_))
        assert(!args.contains(null))
        f.fun.compute(args)
      }

      while (stack.nonEmpty) {
        val p@(variable, value) = stack.head
        stack -= p
        if (value != memory(variable)) {
          memory = memory.updated(variable, value)
          println(s"Updtated: $value -> $variable: ${ast.at(variable)}")
          println(memory)
          println("deps: " + ast.graph.varFunEdges.get(variable))
          val dependencies = ast.graph.varFunEdges.getOrElse(variable, Set())
          val dependenciesWithArgs = dependencies.map(x => ast.at(x))
          val computableDependencies = dependencies.filter(fID => computable(fID))
          for (fid <- computableDependencies) {
            stack += ((fid, eval(fid)))
          }
        }
      }
      this.copy(state = PrgState(memory)).right
    } catch {
      case NonFatal(e) => e.left
    }
  }
}
