package dahu.model.interpreter

import dahu.model.input.TypedIdent
import dahu.model.ir._
import dahu.model.types._
import dahu.recursion.FAlgebra
import dahu.utils._
import dahu.utils.errors._

object LambdaInterpreter {

  def partialEvalAlgebraAny(valueOf: TypedIdent => Option[Value]): FAlgebra[StaticF, PEval[Any]] =
    partialEvalAlgebra(valueOf).asInstanceOf[FAlgebra[StaticF, PEval[Any]]]

  def partialEvalAlgebra(valueOf: TypedIdent => Option[Value]): FAlgebra[StaticF, PEval[Value]] =
    e => {
      val res: PEval[Any] = e match {

        case ApplyF(lbd, param, _) =>
          lbd.apply(param)

        case LambdaF(p, ast, id, _) =>
          ast match {
            case PEmpty              => PEmpty
            case PConstraintViolated => PConstraintViolated
            case x                   => PEFunc(id, x)
          }

        case LambdaParamF(id, _) => LambdaParamPlaceHolder(id)

        case x @ InputF(id, _) =>
          valueOf(id) match {
            case Some(v) => FEval(v)
            case None    => Unknown(Set(id))
          }
        case CstF(v, _) => FEval(v)
        case ComputationF(f, args, _) =>
          args.ssequence.smap { as =>
            f.compute(as)
          }
        case ProductF(members, t) =>
          members.ssequence.smap { ms =>
            t.buildFromValues(ms)
          }
        case SequenceF(members, t) =>
          members.ssequence
        case ITEF(cond, onTrue, onFalse, _) =>
          assert(cond.applicationStack.isEmpty)
          assert(onTrue.applicationStack == onFalse.applicationStack)
          FlatMapped[Value, Value](cond, {
            case true  => onTrue
            case false => onFalse
            case _     => dahu.utils.errors.unexpected
          }, onTrue.applicationStack)
      }
      res.asInstanceOf[PEval[Value]]
    }
}
