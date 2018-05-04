package dahu.planning.model.transforms

import dahu.planning.model
import dahu.planning.model._
import dahu.planning.model.common._

object FullToCore {

  private object ImplicitConversions {
    import scala.language.implicitConversions

    implicit def extractPredef(implicit ctx: Context): Predef = ctx.predef

    implicit class ExprOps(private val lhs: Expr) extends AnyVal {
      def ===(rhs: Expr)(implicit predef: Predef): Expr = Op2(operators.Eq, lhs, rhs)
      def <=(rhs: Expr)(implicit predef: Predef): Expr = Op2(operators.LEQ, lhs, rhs)
      def <(rhs: Expr)(implicit predef: Predef): Expr = Op2(operators.LT, lhs, rhs)

      def toStatement: core.Statement = core.StaticBooleanAssertion(lhs)
    }
  }
  import ImplicitConversions._

  case class Context(predef: Predef, scope: Scope, config: Config = Config())

  /** Monad that represent an expression of type A that is subject to the associated statements to restrict its values. */
  private case class CoreM[+A](value: A, statements: Seq[core.Statement]) {
    def flatMap[B](f: A => CoreM[B]): CoreM[B] = {
      val fb = f(value)
      CoreM(fb.value, statements ++ fb.statements)
    }
    def map[B](f: A => B): CoreM[B] = CoreM(f(value), statements)
  }
  private object CoreM {
    def pure[A](a: A): CoreM[A] = CoreM(a, Seq())
    def unit(statements: core.Statement*): CoreM[Unit] = CoreM((), statements)
  }

  private def f2c(expr: full.StaticExpr)(implicit ctx: Context): CoreM[Expr] = {
    expr match {
      case full.ConstantExpr(cst) => CoreM.pure(cst)
      case full.Variable(v)       => CoreM.pure(v)
      case full.Constant(template, params) =>
        for {
          params <- f2c(params.toList)
          cst = core.Constant(template, params)
          variable = LocalVar(ctx.scope.makeNewId(), cst.typ)
          _ <- CoreM.unit(core.LocalVarDeclaration(variable))
          _ <- CoreM.unit(core.BindAssertion(cst, variable))
        } yield variable
      case full.BinaryExprTree(op, lhs, rhs) =>
        for {
          l <- f2c(lhs)
          r <- f2c(rhs)
        } yield Op2(op, l, r)
      case full.UnaryExprTree(op, e) =>
        for {
          ec <- f2c(e)
        } yield Op1(op, ec)
    }

  }

  private def f2c(exprs: List[full.StaticExpr])(implicit ctx: Context): CoreM[List[Expr]] = {
    exprs match {
      case Nil => CoreM.pure(Nil)
      case head :: tail =>
        for {
          h <- f2c(head)
          t <- f2c(tail)
        } yield h :: t
    }
  }

  private def f2c(expr: full.TimedExpr)(implicit ctx: Context): CoreM[core.Fluent] = {
    expr match {
      case fluent: full.Fluent =>
        for {
          params <- f2c(fluent.params.toList)
        } yield core.Fluent(fluent.template, params)
    }
  }

  private def f2c(act: full.ActionTemplate)(implicit ctx: Context): core.ActionTemplate = {
    implicit val actionContext: Context = ctx.copy(scope = act.scope)
    val statements = act.store.blocks.flatMap {
      case full.ArgDeclaration(arg) => Seq(core.ArgDeclaration(arg))
      case x: full.Statement        => f2c(x)(actionContext).statements
    }
    core.ActionTemplate(act.scope, statements)(actionContext.predef)
  }

  private def f2c(block: full.Statement)(implicit ctx: Context): CoreM[Unit] = block match {
    case full.BooleanAssertion(e) =>
      for {
        ec <- f2c(e)
        _ <- CoreM.unit(core.StaticBooleanAssertion(ec))
      } yield ()

    case full.StaticAssignmentAssertion(lhs: full.Constant, full.ConstantExpr(rhs))
        if lhs.params.forall(_.isInstanceOf[full.ConstantExpr]) =>
      val boundCst =
        new model.core.BoundConstant(lhs.template, lhs.params.map {
          case full.ConstantExpr(x) => x
          case _                    => throw new RuntimeException("impossible")
        })
      CoreM.unit(core.StaticAssignmentAssertion(boundCst, rhs))
    case full.StaticAssignmentAssertion(_, _) =>
      throw new UnsupportedOperationException(
        s"Assignment assertions on constant functions are only supported when all parameters are declared instances: $block")

    case full.TemporallyQualifiedAssertion(qualifier, assertion) =>
      val startEnd: CoreM[(Expr, Expr)] =
        qualifier match {
          case full.Equals(interval)
              if ctx.config.mergeTimepoints && assertion.name.startsWith(reservedPrefix) =>
            // we are asked to merge timepoints and assertion was not given a name
            // use the timepoints from the interval instead of the one of the assertion
            for {
              start <- f2c(interval.start)
              end <- f2c(interval.end)
            } yield (start, end)
          case full.Equals(interval) =>
            for {
              itvStart <- f2c(interval.start)
              itvEnd <- f2c(interval.end)
              _ <- CoreM.unit(core.LocalVarDeclaration(assertion.start),
                              core.LocalVarDeclaration(assertion.end))
              _ <- CoreM.unit(core.StaticBooleanAssertion(itvStart === assertion.start))
              _ <- CoreM.unit(core.StaticBooleanAssertion(assertion.end === itvEnd))

            } yield (assertion.start, assertion.end)
          case full.Contains(interval) =>
            for {
              itvStart <- f2c(interval.start)
              itvEnd <- f2c(interval.end)
              _ <- CoreM.unit(
                core.LocalVarDeclaration(assertion.start),
                core.LocalVarDeclaration(assertion.end),
                core.StaticBooleanAssertion(itvStart <= assertion.start),
                core.StaticBooleanAssertion(assertion.end <= itvEnd)
              )
            } yield (assertion.start, assertion.end)
        }
      assertion match {
        case full.TimedEqualAssertion(fluent, value, _, _) =>
          for {
            se <- startEnd
            (start, end) = se
            coreFluent <- f2c(fluent)
            coreValue <- f2c(value)
            _ <- CoreM.unit(core.TimedEqualAssertion(start, end, coreFluent, coreValue),
                            core.StaticBooleanAssertion(start <= end))
          } yield ()

        case full.TimedAssignmentAssertion(fluent, value, _, _) =>
          for {
            se <- startEnd
            (start, end) = se
            coreFluent <- f2c(fluent)
            coreValue <- f2c(value)
            _ <- CoreM.unit(core.TimedAssignmentAssertion(start, end, coreFluent, coreValue),
                            core.StaticBooleanAssertion(start <= end))
          } yield ()

        case full.TimedTransitionAssertion(fluent, fromValue, toValue, _, _) =>
          for {
            se <- startEnd
            (start, end) = se
            coreFluent <- f2c(fluent)
            coreFrom <- f2c(fromValue)
            coreTo <- f2c(toValue)
            _ <- CoreM.unit(core.TimedTransitionAssertion(start, end, coreFluent, coreFrom, coreTo),
                            core.StaticBooleanAssertion(start < end))
          } yield ()
      }

    case full.LocalVarDeclaration(v) => CoreM.unit(core.LocalVarDeclaration(v))
  }

  def trans(model: full.Model, config: Config = Config())(
      implicit predef: Predef): Seq[core.InModuleBlock] = {
    model.store.blocks.flatMap {
      case full.FunctionDeclaration(x) => Seq(core.FunctionDeclaration(x))
      case full.InstanceDeclaration(x) => Seq(core.InstanceDeclaration(x))
      case full.TypeDeclaration(x)     => Seq(core.TypeDeclaration(x))
      case x: full.Statement           => f2c(x)(Context(predef, model.scope, config)).statements
      case x: full.ActionTemplate      => Seq(f2c(x)(Context(predef, model.scope, config)))
    }
  }

}
