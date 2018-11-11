package dahu.refinement.interop
import dahu.model.functions
import dahu.model.input.Lambda.LambdaIdent
import dahu.model.input._
import dahu.model.ir._
import dahu.model.products.{FieldAccessAny, ProductTagAny, RecordType}
import dahu.model.types._
import dahu.refinement.common.{R, Values}
import dahu.refinement.{Fun, RMemory}
import dahu.utils._
import dahu.utils.errors._

import scala.collection.mutable

object evaluation {

  implicit val memType = Tag.default[RMemory]

  type Env = EvalEnv

  class EvalEnv(binds: Map[LambdaIdent, Value] = Map(), stack: List[Value] = Nil) {
    def get(id: LambdaIdent): Value = binds(id)
    def push(value: Value): EvalEnv = new EvalEnv(binds, value :: stack)
    def bind(lambdaIdent: LambdaIdent): Option[EvalEnv] = {
      stack match {
        case head :: tail => Some(new EvalEnv(binds.updated(lambdaIdent, head), tail))
        case Nil          => None
      }
    }
  }

  object EvalEnv {
    def empty(): EvalEnv = new EvalEnv()
  }

  def eval[I](coalg: I => ExprF[I], valueOf: TypedIdent => Value)(e: Env)(i: I): Value = {
    def ev(i: I, e: Env): Value = eval(coalg, valueOf)(e)(i)

    coalg(i) match {
      case InputF(id, _) =>
        //        println(s"$id: ${valueOf(id)}")
        valueOf(id)
      case CstF(v, _) => v
      case ComputationF(f, args, _) =>
        Value(f.compute(args.smap(ev(_, e))))
      case SequenceF(members, _) => Value(members.smap(ev(_, e)))
      case NoopF(x, _)           => ev(x, e)
      case ITEF(cond, onTrue, onFalse, _) =>
        ev(cond, e) match {
          case true  => ev(onTrue, e)
          case false => ev(onFalse, e)
          case _     => ???
        }
      case ApplyF(lbd, param, _) =>
        val arg = ev(param, e)
        val subEnv = e.push(arg)
        ev(lbd, subEnv)
      case LambdaF(_, tree, id, _) =>
        e.bind(id) match {
          case Some(subEnv) => ev(tree, subEnv)
          case None => // we have to construct a lambda...
            val f: Value => Value = (in: Value) => {
              e.push(in).bind(id) match {
                case Some(subEnv) => ev(tree, subEnv)
                case None         => ???
              }
            }
            Value(functions.lift(f))
        }

      case LambdaParamF(id, _) =>
        e.get(id)

      case ProductF(ms, tpe) =>
        val members = ms.map(ev(_, e))
        Value(tpe.buildFromValues(members))

    }

  }

  type Mem = Array[Double]
  type Addr = dahu.refinement.common.Addr
  type NumParam = Int
  case class Read(relativeStateId: Int, offset: Int)

  class CompileEnv[I]() {
    val reads = mutable.ArrayBuffer[Read]()

    def argIdOf(stateRelativePos: Int, offset: Int): Int = {
      val r = Read(stateRelativePos, offset)
      if(!reads.contains(r))
        reads += r
      reads.indexOf(r)
    }
  }

  def read(mem: Mem, addr: Addr): Double = ???

  abstract class CompiledFun {
    def eval(mem: Mem): Any
  }
  class ParamRead(argId: Int) extends CompiledFun {
    override def eval(mem: Mem): Double = {
      mem(argId)
    }
  }
  class Constant(v: Any) extends CompiledFun {
    override def eval(mem: Mem): Any = v
  }
  class Compute(f: Vec[Any] => Any, others: Vec[CompiledFun]) extends CompiledFun {
    override def eval(mem: Mem): Any = {
      val evs = others.map(_.eval(mem))
      f(evs)
    }
  }
  class ComputeITE(c: CompiledFun, t: CompiledFun, f: CompiledFun) extends CompiledFun {
    override def eval(mem: Mem): Any = {
      val cond = c.eval(mem)
      cond match {
        case true | 1  => t.eval(mem)
        case false | 0 => f.eval(mem)
        case _         => unexpected
      }
    }
  }

  case class CompiledFunBridge(
      f: CompiledFun,
      numParams: Int,
      read: Array[Read]
  ) extends Fun {
    override def eval(params: Values): R =
      f.eval(params).asInstanceOf[R]

    val isConstant = read.isEmpty
    val belowStateOffset = read.map(_.relativeStateId).foldLeft(0)(math.min)
    val aboveStateOffset = read.map(_.relativeStateId).foldLeft(0)(math.max)
  }

  def compile[I](coalg: I => ExprF[I], cstate: ProductTagAny)(i: I): CompiledFunBridge = {
    val env = new CompileEnv[I]()

    val f = compileImpl(coalg, cstate)(env)(i)
    val numParams = env.reads.size

    CompiledFunBridge(f, numParams, env.reads.toArray)
  }

  private def compileImpl[I](coalg: I => ExprF[I], cstate: ProductTagAny)(e: CompileEnv[I])(
      i: I): CompiledFun = {

    def ev(i: I): CompiledFun = compileImpl(coalg, cstate)(e)(i)

    coalg(i) match {
      case InputF(id, _) => ???
      case CstF(v, _) =>
        v match {
          case ProductF(Vec(stateRelId: Int, field: String), tpe: RecordType)
              if tpe.name == "read-cont" =>
            val offset =
              cstate
                .fieldPosition(field)
                .getOrElse(unexpected(s"Type $tpe has no field named $field"))
            val argId = e.argIdOf(stateRelId, offset)
            new ParamRead(argId)
          case _ => new Constant(v)
        }

      case ProductF(Vec(sId, fieldName), tpe: RecordType) =>
        val stateRelId = coalg(sId) match {
          case CstF(x: Int, Tag.ofInt) => x
          case a =>
            println(a)
            ???
        }
        val field = coalg(fieldName) match {
          case CstF(x: String, Tag.ofString) => x
          case _                             => ???
        }
        require(tpe.name == "read-cont")
        val offset =
          cstate.fieldPosition(field).getOrElse(unexpected(s"Type $tpe has no field named $field"))
        val argId = e.argIdOf(stateRelId, offset)
        new ParamRead(argId)

      case ComputationF(f, args, _) =>
        val X = args.smap(ev)
//        println(X)
        new Compute(f.computeFromAny, args.smap(ev))
      case SequenceF(members, _) => ???
      case NoopF(x, _)           => ev(x)
      case ITEF(cond, onTrue, onFalse, _) =>
        new ComputeITE(ev(cond), ev(onTrue), ev(onFalse))
      case ApplyF(lbd, param, _)       => ???
      case LambdaF(parap, tree, id, _) => ???
      case LambdaParamF(id, _)         => ???
      case ProductF(ms, tpe)           => ???
    }

  }

}
