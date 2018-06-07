package dahu.model.interpreter

import dahu.model.input.TypedIdent
import dahu.model.ir._
import dahu.model.types._
import dahu.recursion.FAlgebra
import dahu.utils._
import dahu.utils.Vec.Vec2
import dahu.utils.errors._

object LambdaInterpreter {
  private type EvalStack[X] = Result[PEval[X]]
  private implicit val stackedApp: SApplicative[EvalStack] = SApplicative[Result].compose[PEval]

  private def stackSequenceMap[A: ClassTag, B: ClassTag](vfa: Vec[EvalStack[A]])(
      f: Vec[A] => B): EvalStack[B] = {
    val trav = STraverse[Vec].sequence[EvalStack, A](vfa)
    stackedApp.smap(trav)(f)
  }
  private def flatten[A](fa: EvalStack[EvalStack[A]]): EvalStack[A] = fa match {
    case Empty              => Empty
    case ConstraintViolated => ConstraintViolated
    case Res(v) =>
      v match {
        case FEval(x)                   => x
        case LambdaParamPlaceHolder(id) => Res(LambdaParamPlaceHolder(id))
        case Unknown(unboundVars)       => Res(Unknown(unboundVars))
        case PEFunc(id, tree) =>
          val flatTree = flatten(Res(tree))
          SApplicative[Result].smap(flatTree)(t => PEFunc(id, t))
        case Mapped(pe, f) => // pe: PEval[Y],  f: Y => EvalStack[A]
          // ???(Mapped((pe': PEval[Y],  f': Y => A)
          pe match {
            case FEval(v)                   => f(v)
            case LambdaParamPlaceHolder(id) => Res(LambdaParamPlaceHolder(id))
            case Unknown(unboundVars)       => Res(Unknown(unboundVars))
            case _                          => ???
          }
      }
  }
  def flatMap[A, B: ClassTag](fa: EvalStack[A])(f: A => EvalStack[B]): EvalStack[B] =
    flatten(stackedApp.smap(fa)(f))

  def partialEvalAlgebra(
      valueOf: TypedIdent[Any] => Option[Value]): FAlgebra[StaticF, Result[PEval[Value]]] =
    e => {
      val res: Result[PEval[Any]] = e match {

        case ApplyF(lbd, param, _) =>
          Vec(lbd, param).sequence
            .map {
              case Vec2(f: PEFunc[Value], p) => f(p)
              case _                         => unexpected

            }

        case LambdaF(p, ast, id, _) =>
          assert(p.isInstanceOf[Res[_]])
          ast.map(PEFunc(id, _))

        case LambdaParamF(id, _) => Res(LambdaParamPlaceHolder(id))

        case x @ InputF(id, _) =>
          valueOf(id) match {
            case Some(v) => Res(FEval(v))
            case None    => Res(Unknown(Set(id)))
          }
        case CstF(v, _) => Res(FEval(v))
        case ComputationF(f, args, _) =>
          stackSequenceMap(args) { as =>
            f.compute(as)
          }
        case ProductF(members, t) =>
          stackSequenceMap(members) { ms =>
            t.idProd.buildFromValues(ms)
          }
        case SequenceF(members, t) =>
          stackSequenceMap(members)(identity)
        case ITEF(cond, onTrue, onFalse, _) =>
          flatMap(cond) {
            case true  => onTrue
            case false => onFalse
            case _     => dahu.utils.errors.unexpected
          }
        case Partial(value, cond, _) =>
          cond match {
            case Empty              => value
            case ConstraintViolated => ConstraintViolated
            case _ =>
              flatMap(cond) {
                case true  => value
                case false => ConstraintViolated
                case _     => unexpected
              }
          }
        case OptionalF(value, present, _) =>
          present match {
            case Empty              => value
            case ConstraintViolated => ConstraintViolated
            case _ =>
              flatMap(present) {
                case true  => value
                case false => Empty
                case x     => unexpected(s"Present does not evaluates to a boolean but to: $x")
              }
          }
        case PresentF(v) =>
          v match {
            case Empty              => stackedApp.pure(false)
            case ConstraintViolated => ConstraintViolated
            case Res(_)             => stackedApp.pure(true)
          }
        case ValidF(v) =>
          v match {
            case Empty              => Empty
            case ConstraintViolated => stackedApp.pure(false)
            case Res(_)             => stackedApp.pure(true)
          }
      }
      res.asInstanceOf[EvalStack[Value]]
    }
}
