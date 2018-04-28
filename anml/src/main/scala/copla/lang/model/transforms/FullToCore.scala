package copla.lang.model.transforms

import copla.lang.model
import copla.lang.model._
import copla.lang.model.common._

object FullToCore {

  private object ImplicitConversions {
    import scala.language.implicitConversions
    implicit def absolute2relativeTimepoint(tp: Timepoint): core.TPRef = core.TPRef(tp)
  }
  import ImplicitConversions._

  case class Context(scope: Scope, config: Config = Config())

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

  private def f2c(expr: full.StaticExpr)(implicit ctx: Context): CoreM[Term] = {
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
    }
  }

  private def f2c(expr: full.IntExpr)(implicit ctx: Context): CoreM[core.IntExpr] =
    expr match {
      case full.GenIntExpr(e) =>
        for {
          v <- f2c(e)
        } yield core.IntTerm(v)

      case full.Minus(e) =>
        for {
          ce <- f2c(e)
        } yield core.Minus(ce)

      case full.Add(lhs, rhs) =>
        for {
          el <- f2c(lhs)
          er <- f2c(rhs)
        } yield core.Add(el, er)

      case full.Mul(lhs, rhs) =>
        for {
          el <- f2c(lhs)
          er <- f2c(rhs)
        } yield core.Mul(el, er)

      case full.Div(lhs, rhs) =>
        for {
          el <- f2c(lhs)
          er <- f2c(rhs)
        } yield core.Div(el, er)
    }

  private def f2c(tp: full.TPRef)(implicit ctx: Context): CoreM[core.TPRef] =
    for {
      de <- f2c(tp.delay)
    } yield core.TPRef(tp.id, de)

  private def f2c(exprs: List[full.StaticExpr])(implicit ctx: Context): CoreM[List[Term]] = {
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
    core.ActionTemplate(act.scope, statements)
  }

  private def f2c(block: full.Statement)(implicit ctx: Context): CoreM[Unit] = block match {
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

    case x: full.StaticEqualAssertion =>
      for {
        lVar <- f2c(x.left)
        rVar <- f2c(x.right)
        _ <- CoreM.unit(core.StaticEqualAssertion(lVar, rVar))
      } yield ()

    case x: full.StaticDifferentAssertion =>
      for {
        lVar <- f2c(x.left)
        rVar <- f2c(x.right)
        _ <- CoreM.unit(core.StaticDifferentAssertion(lVar, rVar))
      } yield ()

    case full.TemporallyQualifiedAssertion(qualifier, assertion) =>
      val startEnd: CoreM[(core.TPRef, core.TPRef)] =
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
              _ <- CoreM.unit(core.TimepointDeclaration(assertion.start),
                              core.TimepointDeclaration(assertion.end))
              _ <- CoreM.unit(itvStart === assertion.start: _*)
              _ <- CoreM.unit(assertion.end === itvEnd: _*)

            } yield (assertion.start, assertion.end)
          case full.Contains(interval) =>
            for {
              itvStart <- f2c(interval.start)
              itvEnd <- f2c(interval.end)
              _ <- CoreM.unit(
                core.TimepointDeclaration(assertion.start),
                core.TimepointDeclaration(assertion.end),
                itvStart <= assertion.start,
                assertion.end <= itvEnd
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
                            start <= end)
          } yield ()

        case full.TimedAssignmentAssertion(fluent, value, _, _) =>
          for {
            se <- startEnd
            (start, end) = se
            coreFluent <- f2c(fluent)
            coreValue <- f2c(value)
            _ <- CoreM.unit(core.TimedAssignmentAssertion(start, end, coreFluent, coreValue),
                            start <= end)
          } yield ()

        case full.TimedTransitionAssertion(fluent, fromValue, toValue, _, _) =>
          for {
            se <- startEnd
            (start, end) = se
            coreFluent <- f2c(fluent)
            coreFrom <- f2c(fromValue)
            coreTo <- f2c(toValue)
            _ <- CoreM.unit(core.TimedTransitionAssertion(start, end, coreFluent, coreFrom, coreTo),
                            start < end)
          } yield ()
      }

    case full.TBefore(from, to) =>
      for {
        f <- f2c(from)
        t <- f2c(to)
        _ <- CoreM.unit(
          core.TBefore(f, t)
        )
      } yield ()

    case full.TimepointDeclaration(tp) => CoreM.unit(core.TimepointDeclaration(tp))
    case full.LocalVarDeclaration(v)   => CoreM.unit(core.LocalVarDeclaration(v))
  }

  def trans(model: full.Model, config: Config = Config()): Seq[core.InModuleBlock] = {
    model.store.blocks.flatMap {
      case full.FunctionDeclaration(x) => Seq(core.FunctionDeclaration(x))
      case full.InstanceDeclaration(x) => Seq(core.InstanceDeclaration(x))
      case full.TypeDeclaration(x)     => Seq(core.TypeDeclaration(x))
      case x: full.Statement           => f2c(x)(Context(model.scope, config)).statements
      case x: full.ActionTemplate      => Seq(f2c(x)(Context(model.scope, config)))
    }
  }

}
