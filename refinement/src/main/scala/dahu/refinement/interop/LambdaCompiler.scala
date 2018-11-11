package dahu.refinement.interop
import dahu.model.input.Lambda.LambdaIdent
import dahu.model.ir._
import dahu.model.products.FieldAccessAny
import dahu.model.types._
import dahu.refinement.Fun
import dahu.refinement.common.{R, Values}
import dahu.utils._
import dahu.utils.errors._

import scala.collection.mutable

object LambdaCompiler {

  type Mem = Array[Double]
  type Addr = dahu.refinement.common.Addr
  type NumParam = Int
  case class Read(paramNumber: Int, offset: Int)

  class CompileEnv[I]() {

    val lambdaParamNumber: mutable.Map[I, NumParam] = mutable.Map[I, NumParam]()
    val lambdaIdentToPosition: mutable.Map[LambdaIdent, NumParam] = mutable.Map()
    private var _isSealed: Boolean = false

    val reads = mutable.ArrayBuffer[Read]()

    def addParam(i: I, id: LambdaIdent): Unit = {
      assert(!lambdaParamNumber.contains(i))
      assert(!lambdaIdentToPosition.contains(id))
      val pos = lambdaParamNumber.size
      lambdaParamNumber(i) = pos
      lambdaIdentToPosition(id) = pos
    }

    def lambdaParamPosition(i: I) = lambdaParamNumber(i)
    def lambdaIdentPosition(id: LambdaIdent) = lambdaIdentToPosition(id)

    def argIdOf(lbdParam: I, offset: Int): Int = {
      val r = Read(lambdaParamPosition(lbdParam), offset)
      if(!reads.contains(r))
        reads += r
      reads.indexOf(r)
    }
    def argIdOf(id: LambdaIdent, offset: Int): Int = {
      val r = Read(lambdaIdentToPosition(id), offset)
      if(!reads.contains(r))
        reads += r
      reads.indexOf(r)
    }

    def seal: Unit = _isSealed = true

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
  }

  def compile[I](coalg: I => ExprF[I])(i: I): CompiledFunBridge = {
    val env = new CompileEnv[I]()

    val f = compileImpl(coalg)(env)(i)
    val numParams = env.reads.size

    CompiledFunBridge(f, numParams, env.reads.toArray)
  }

  private def compileImpl[I](coalg: I => ExprF[I])(e: CompileEnv[I])(i: I): CompiledFun = {

    def ev(i: I): CompiledFun = compileImpl(coalg)(e)(i)

    coalg(i) match {
      case InputF(id, _) => ???
      case CstF(v, _) =>
        e.seal
        new Constant(v)
      case ComputationF(f: FieldAccessAny, Vec(a), _) =>
        e.seal
        assert(f.outType == Tag.ofDouble)
        assert(e.lambdaParamNumber.contains(a))
        val argId = e.argIdOf(a, f.fieldPosition)
        new ParamRead(argId)
      case ComputationF(f, args, _) =>
        e.seal
        new Compute(f.computeFromAny, args.smap(ev))
      case SequenceF(members, _) => ???
      case NoopF(x, _)           => ev(x)
      case ITEF(cond, onTrue, onFalse, _) =>
        e.seal
        new ComputeITE(ev(cond), ev(onTrue), ev(onFalse))
      case ApplyF(lbd, param, _) => ???
      case LambdaF(parap, tree, id, _) =>
        e.addParam(parap, id)
        ev(tree)

      case LambdaParamF(id, _) =>
        e.seal
        val argId = e.argIdOf(id, 0)
        new ParamRead(argId)

      case ProductF(ms, tpe) => ???
    }

  }

}
