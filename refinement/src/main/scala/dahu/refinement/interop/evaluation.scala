package dahu.refinement.interop
import dahu.model.functions
import dahu.model.input.Lambda.LambdaIdent
import dahu.model.input._
import dahu.model.ir._
import dahu.model.types._
import dahu.refinement.RMemory

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

}
