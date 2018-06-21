package dahu.model.interpreter

import dahu.model.input.TypedIdent
import dahu.model.ir._
import dahu.model.types._
import dahu.recursion.FAlgebra
import dahu.utils._
import dahu.utils.errors._

object LambdaInterpreter {

  def partialEvalAlgebraAny(
      valueOf: TypedIdent[Any] => Option[Value]): FAlgebra[StaticF, PEval[Any]] =
    partialEvalAlgebra(valueOf).asInstanceOf[FAlgebra[StaticF, PEval[Any]]]

  def partialEvalAlgebra(
      valueOf: TypedIdent[Any] => Option[Value]): FAlgebra[StaticF, PEval[Value]] =
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
          args.sequence.smap { as =>
            f.compute(as)
          }
        case ProductF(members, t) =>
          members.sequence.smap { ms =>
            t.idProd.buildFromValues(ms)
          }
        case SequenceF(members, t) =>
          members.sequence
        case ITEF(cond, onTrue, onFalse, _) =>
          assert(cond.applicationStack.isEmpty)
          assert(onTrue.applicationStack == onFalse.applicationStack)
          FlatMapped[Value, Value](cond, {
            case true  => onTrue
            case false => onFalse
            case _     => dahu.utils.errors.unexpected
          }, onTrue.applicationStack)
        case Partial(value, cond, _) =>
          assert(cond.applicationStack.isEmpty || cond.applicationStack == value.applicationStack)
          cond match {
            case PEmpty => value
            case PConstraintViolated =>
              FlatMapped[Value, Value](value, _ => PConstraintViolated, value.applicationStack)
            case _ =>
              FlatMapped[Value, Value](cond, {
                case true => value
                case false =>
                  FlatMapped[Value, Value](value, _ => PConstraintViolated, value.applicationStack)
                case _ => unexpected
              }, value.applicationStack)
          }
        case UniversalPartial(value, cond, _) =>
          assert(cond.applicationStack.isEmpty || cond.applicationStack == value.applicationStack)
          cond match {
            case PEmpty => value
            case PConstraintViolated =>
              FlatMapped[Value, Value](value, _ => PConstraintViolated, value.applicationStack)
            case _ =>
              FlatMapped[Value, Value](cond, {
                case true  => value
                case false => PConstraintViolated
                case _     => unexpected
              }, value.applicationStack)
          }
        case OptionalF(value, present, _) =>
          assert(
            present.applicationStack.isEmpty || present.applicationStack == value.applicationStack)
          present match {
            case PEmpty              => PEmpty
            case PConstraintViolated => PConstraintViolated
            case _ =>
              FlatMapped[Value, Value](present, {
                case true  => value
                case false => PEmpty
                case x     => unexpected(s"Present does not evaluates to a boolean but to: $x")
              }, value.applicationStack)
          }
        case PresentF(v) =>
          v match {
            case PEmpty              => FEval(false)
            case PConstraintViolated => PConstraintViolated
            case FEval(_)            => FEval(true)
          }
        case ValidF(v) =>
          v match {
            case PEmpty              => PEmpty
            case PConstraintViolated => FEval(false)
            case FEval(_)            => FEval(true)
          }
      }
      res.asInstanceOf[PEval[Value]]
    }
}
