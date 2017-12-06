package dahu

import dahu.recursion.{ComputationF, CstF, InputF, ResultF}

import scalaz._
import Scalaz._
import scala.collection.mutable

package object interpreter {



  type VarID = Int
  type FunID = Int
  type Expr = ResultF[VarID]
  type Fun = ComputationF[VarID]
  type Input = InputF[VarID]
  type Cst = CstF[VarID]
  type Code = Map[VarID, Expr]

  type VarName = String

  type Res[T] = \/[Throwable, T]
  object Res {
    def right[T](value: T): Res[T] = \/-(value)
  }


  sealed abstract class Language
  final case class SetVar(input: Input, value: V) extends Language
  final case class GetVar(output: VarID) extends Language

  type Hist = List[SetVar]

  case class Environment(inputs: Map[VarName, V])
  object Environment {
    def apply(binds: (VarName, V)*): Environment = Environment(binds.toMap)
  }

  final case class Err(msg: String) extends Throwable(msg)

  def evaluate(ast: AST, environment: Environment): Res[V] = {
    def get(address: VarID): Res[V] =
      ast.at(address).flatMap {
        _ match {
          case x: Input => environment.inputs.get(x.name) match {
            case Some(x) => x.right
            case None => Err(s"Input $x was not set").left
          }
          case x: Cst => x.value.right
          case x: Fun => x.args.traverse(get(_)).map(x.fun.compute(_))
        }
      }

    get(ast.head)
  }

  final case class ComputationGraph(code: Code) {
    val varFunEdges: Map[VarID, Set[VarID]] =
      code.toSeq.collect {
        case (id, x: Fun) => x.args.map(varID => (varID, id)).toVector
      }.flatten
        .groupBy(_._1)
        .mapValues(_.map(_._2).toSet)
  }

  final case class AST(head: VarID, code: Code) {
    override def toString: String = s"AST: $head\n" + code.mkString("  ", "\n  ", "")

    lazy val inputs: Seq[Input] = code.values.collect { case x: Input => x }.toSeq
    def address(i: Input): Res[VarID] = code.collectFirst{ case (k, v) if i == v => k } match {
      case Some(x) => x.right
      case None => Err(s"No input $i").left
    }

    def at(address: VarID): Res[Expr] = code.get(address) match {
      case Some(x) => x.right
      case None => Err(s"Address out of bounds: $address").left
    }

    def inputOf(name: String): Res[Input] =
      inputs.collectFirst { case x if x.name == name => x }.toRightDisjunction(Err(s"Found no input named $name"))

    lazy val graph = ComputationGraph(code)
  }


  type V = Any

//  case class Memory(mem: Vector[V])

  case class Interpreter(ast: AST, memory: Vector[V]) {

    def set(v: Input, value: V): Res[Interpreter] = {
      ast.address(v)
        .map(k => this.copy(memory = memory.updated(k, value)))
    }

    def get(k: VarID): Res[V] = {
      memory(k).right
    }
  }

  type ValueMemory = Vector[V]
  type ValueFlags = Vector[Int]
  val dirty = 1
  val clean = 0

  final case class PrgState(memory: Vector[V], flags: ValueFlags)

  final case class ExecutingPrg(ast: AST, state: PrgState) {

    def update(v: VarID, value: V): Res[ExecutingPrg] = {
      if(v <= state.memory.size && v == state.flags.size)
        return Err(s"Address out of Memory: $v").left

      val stack = mutable.Set[(VarID, V)]((v, value))

      var memory = state.memory
      var flags = state.flags

      def initialized(v: VarID): Boolean = memory(v) != null
      def computable(v: Fun): Boolean = v.args.forall(initialized)
      def eval(v: FunID): V = {
        val f = ast.graph.comp(v)
        val args = f.args.map(memory(_))
        assert(!args.contains(null))
        f.fun.compute(args)
      }

      while(stack.nonEmpty) {
        val p@(variable, value) = stack.head
        stack -= p
        if(value != memory(variable)) {
          memory = memory.updated(variable, value)
          val computableDependencies =
            ast.graph.varFunEdges(variable).filter(fID => computable(ast.graph.comp(fID)))

        }


      }

    }
  }




}
