package dahu.model.interpreter

import cats.implicits._
import dahu.model.ir._
import dahu.model.types.Value
import dahu.utils.errors._
import dahu.recursion._
import dahu.recursion.Recursion._

object Interpreter {

  def eval(ast: TotalSubAST[_])(root: ast.ID, inputs: ast.VID => Value): Value = {
    val input: InputF[_] => Value = {
      val map: Map[InputF[_], Value] =
        ast.variables.domain.toIterable().map(i => (ast.variables(i), Value(inputs(i)))).toMap
      x =>
        map(x)
    }
    val alg: FAlgebra[Total, Value] = {
      case x: InputF[_]             => input(x)
      case CstF(v, _)               => v
      case ComputationF(f, args, _) => Value(f.compute(args))
      case ProductF(members, t)     => Value(t.idProd.buildFromValues(members))
    }
    hylo(ast.tree.asFunction, alg)(root)
  }

  /** Evaluates the given AST with the provided inputs.
    * Returns Some(v), v being the value of the root node if all encountered constraints were satisfied.
    * Returns None, if a constraint (condition of a `SubjectTo(value, condition)` node) was encountered.
    * To extract the cause of a failure, look at evalWithFailureCause.
    */
  def eval(ast: AST[_])(inputs: ast.VID => Value): Option[Value] = {
    val input: InputF[_] => Value = {
      val map: Map[InputF[_], Value] =
        ast.variables.domain.toIterable().map(i => (ast.variables(i), Value(inputs(i)))).toMap
      x =>
        map(x)
    }
    val alg: FAlgebra[ExprF, Option[Value]] = {
      case x: InputF[_] => Some(input(x))
      case CstF(v, _)   => Some(v)
      case ComputationF(f, args, _) =>
        val x: Option[List[Value]] = args.toList.sequence
        x match {
          case Some(actualArgs) => Some(Value(f.compute(actualArgs)))
          case None             => None
        }
      case ProductF(members, typ) =>
        val optMembers: Option[List[Value]] = members.toList.sequence
        optMembers
          .map(xs => typ.idProd.buildFromValues(xs))
          .map(Value(_))
      case Partial(value, cond, _) =>
        cond match {
          case Some(true)  => value
          case Some(false) => None
          case Some(x)     => unexpected(s"Condition does not evaluates to a boolean but to: $x")
          case None        => None
        }
      case OptionalF(value, cond, _) =>
        cond match {
          case Some(true)  => value.map(x => Value(Some(x)))
          case Some(false) => Some(Value(None))
          case Some(x)     => unexpected(s"Condition does not evaluates to a boolean but to: $x")
          case None        => None
        }
    }
    hylo(ast.tree.asFunction, alg)(ast.root)
  }

  case class ConstraintViolated[T](nodes: Seq[T])
  type Evaluation[Node, T] = Either[ConstraintViolated[Node], T]

  def evalWithFailureCause[T](ast: AST[T])(inputs: ast.VID => Value): Evaluation[T, Value] = {
    val input: InputF[_] => Value = {
      val map: Map[InputF[_], Value] =
        ast.variables.domain.toIterable().map(i => (ast.variables(i), Value(inputs(i)))).toMap
      x =>
        map(x)
    }
    val alg: AttributeAlgebra[ast.ID, ExprF, Evaluation[T, Value]] = x => {
      val res = x match {
        case EnvT(_, x: InputF[_]) => Right(input(x))
        case EnvT(_, CstF(v, _))   => Right(v)
        case EnvT(id, ComputationF(f, args, _)) =>
          val x: Evaluation[T, List[Value]] = args.toList.sequence
          x match {
            case Right(actualArgs) =>
              Right(Value(f.compute(actualArgs)))
            case Left(x) =>
              Left(x)
          }
        case EnvT(_, ProductF(members, t)) =>
          members.toList.sequence.map(ms => Value(t.idProd.buildFromValues(ms)))
        case EnvT(id, Partial(value, cond, _)) =>
          cond match {
            case Right(true) =>
              value
            case Right(false) =>
              Left(ConstraintViolated(ast.toInput(id)))
            case Right(x) => unexpected(s"Condition does not evaluates to a boolean but to: $x")
            case Left(x) =>
              Left(x)
          }
        case EnvT(_, OptionalF(value, present, _)) =>
          present match {
            case Right(true)  => value.map(x => Value(Some(x)))
            case Right(false) => Right(Value(None))
            case Left(x)      => Left(x)
          }
      }
      res
    }
    val coalg: AttributeCoalgebra[ExprF, ast.ID] = ast.tree.asFunction.toAttributeCoalgebra
    hylo(coalg, alg)(ast.root)

  }

}
