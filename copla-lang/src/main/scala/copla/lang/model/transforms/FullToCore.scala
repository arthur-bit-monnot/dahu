package copla.lang.model.transforms

import copla.lang.model
import copla.lang.model._
import copla.lang.model.full.Scope
import landscaper.transformations.Trans
import shapeless.Poly1

object FullToCore {

  case class Context(scope: Scope, config: Config = Config())

  private def staticExprToVar(expr: full.StaticExpr)(
      implicit ctx: Context): (core.Var, Seq[core.Statement]) = {
    expr match {
      case x: core.Var => (x, Seq())
      case x: full.Constant =>
        val (params, statements) = staticExprsToVars(x.params)
        val cst = core.Constant(x.template, params)
        val variable = core.LocalVar(ctx.scope.makeNewId(), cst.typ)
        val declaration = core.LocalVarDeclaration(variable)
        val eq = core.BindAssertion(cst, variable)
        (variable, statements :+ declaration :+ eq)
    }
  }
  private def staticExprToVarTRANSM(expr: full.StaticExpr)(
      implicit ctx: Context): TransM[core.Var] = {
    expr match {
      case x: core.Var => TransM.pure(x)
      case x: full.Constant =>
        val (params, statements) = staticExprsToVars(x.params)
        val cst = core.Constant(x.template, params)
        val variable = core.LocalVar(ctx.scope.makeNewId(), cst.typ)
        val declaration = core.LocalVarDeclaration(variable)
        val eq = core.BindAssertion(cst, variable)
        TransM(variable, statements :+ declaration :+ eq)
    }
  }

  case class TransM[A](a: A, accumulatedStatement: Seq[core.Statement]) {
    def flatMap[B](f: A => TransM[B]): TransM[B] = {
      val ffb = f(a)
      TransM(ffb.a, accumulatedStatement ++ ffb.accumulatedStatement)
    }
    def map[B](f: A => B): TransM[B] = TransM(f(a), accumulatedStatement)
  }
  object TransM {
    def pure[A](a: A): TransM[A] = TransM(a, Seq())
    def unit(statements: core.Statement*): TransM[Unit] = TransM((), statements)
  }

  private def fullIntExprToCore(e: full.IntExpr)(implicit ctx: Context): TransM[core.IntExpr] =
    e match {
      case x: core.IntExpr => TransM.pure(x)
      case full.GenIntExpr(e) =>
        for {
          v <- staticExprToVarTRANSM(e)
        } yield core.VarIntExpr(v)
      case full.Minus(e) =>
        for {
          ce <- fullIntExprToCore(e)
        } yield core.Minus(ce)

      case full.Add(lhs, rhs) =>
        for {
          el <- fullIntExprToCore(lhs)
          er <- fullIntExprToCore(rhs)
        } yield core.Add(el, er)
    }

  private def fullToCoreTimepoint(tp: full.TPRef)(implicit ctx: Context): TransM[core.TPRef] =
    tp match {
      case x: core.TPRef => TransM.pure(x)
      case x: full.TPRef =>
        for {
          de <- fullIntExprToCore(x.delay)
        } yield core.TPRef(x.id, de)
    }

  private def staticExprsToVars(exprs: Seq[full.StaticExpr])(
      implicit ctx: Context): (Seq[core.Var], Seq[core.Statement]) =
    exprs
      .map(param => staticExprToVar(param))
      .foldLeft((Seq[core.Var](), Seq[core.Statement]())) {
        case ((params, statements), (param, additionalStmts)) =>
          (params :+ param, statements ++ additionalStmts)
      }
  private def staticExprsToVarsTRANSM(exprs: List[full.StaticExpr])(
      implicit ctx: Context): TransM[List[core.Var]] = {
    exprs match {
      case Nil => TransM.pure(Nil)
      case head :: tail =>
        for {
          h <- staticExprToVarTRANSM(head)
          t <- staticExprsToVarsTRANSM(tail)
        } yield h :: t
    }
  }

  def timedSymExpr2CoreFluent(expr: full.TimedSymExpr)(
      implicit ctx: Context): TransM[core.Fluent] = {
    expr match {
      case fluent: full.Fluent =>
        for {
          params <- staticExprsToVarsTRANSM(fluent.params.toList)
        } yield core.Fluent(fluent.template, params)
    }
  }

  def trans(act: full.ActionTemplate)(implicit ctx: Context): core.ActionTemplate = {
    val statements = act.store.blocks.flatMap {
      case x: core.Statement => Seq(x)
      case x: full.Statement => trans(x).accumulatedStatement
    }
    core.ActionTemplate(act.scope, statements)
  }

  def trans(block: full.Statement)(implicit ctx: Context): TransM[Unit] = block match {
    case x: core.Statement => TransM.unit(x)

    case x: full.StaticAssignmentAssertion =>
      (x.left, x.right) match {
        case (cst: full.Constant, inst: core.Term)
            if cst.params.forall(_.isInstanceOf[core.Term]) =>
          val boundCst =
            new model.core.BoundConstant(cst.template, cst.params.map(_.asInstanceOf[core.Term]))
          TransM.unit(core.StaticAssignmentAssertion(boundCst, inst))
        case _ =>
          throw new UnsupportedOperationException(
            s"Assignment assertions on constant functions are only supported when all parameters are declared instances: $block")
      }

    case x: full.StaticEqualAssertion =>
      for {
        lVar <- staticExprToVarTRANSM(x.left)
        rVar <- staticExprToVarTRANSM(x.right)
        _ <- TransM.unit(core.StaticEqualAssertion(lVar, rVar))
      } yield ()
//      val (lVar, lStmts) = staticExprToVar(x.left)
//      val (rVar, rStmts) = staticExprToVar(x.right)
//      lStmts ++ rStmts :+ core.StaticEqualAssertion(lVar, rVar)

    case x: full.StaticDifferentAssertion =>
      for {
        lVar <- staticExprToVarTRANSM(x.left)
        rVar <- staticExprToVarTRANSM(x.right)
        _ <- TransM.unit(core.StaticDifferentAssertion(lVar, rVar))
      } yield ()

    case full.TemporallyQualifiedAssertion(qualifier, assertion) =>
//      val (start, end, baseStatements: Seq[core.Statement]) =
//        qualifier match {
//          case full.Equals(interval) =>
//            if(ctx.config.mergeTimepoints && assertion.name.startsWith(reservedPrefix)) {
//              // we are asked to merge timepoints and assertion was not given a name
//              // use the timepoints from the interval instead of the one of the assertion
//              (interval.start, interval.end, Seq())
//            } else {
//              (assertion.start,
//               assertion.end,
//               Seq(
//                 Seq(core.TimepointDeclaration(assertion.start)),
//                 Seq(core.TimepointDeclaration(assertion.end)),
//                 interval.start === assertion.start,
//                 interval.end === assertion.end
//               ).flatten)
//            }
//          case full.Contains(interval) =>
//            (assertion.start,
//             assertion.end,
//             Seq(
//               core.TimepointDeclaration(assertion.start),
//               core.TimepointDeclaration(assertion.end),
//               interval.start <= assertion.start,
//               assertion.end <= interval.end
//             ))
//        }
      val startEnd: TransM[(core.TPRef, core.TPRef)] =
        qualifier match {
          case full.Equals(interval) =>
            if(ctx.config.mergeTimepoints && assertion.name.startsWith(reservedPrefix)) {
              // we are asked to merge timepoints and assertion was not given a name
              // use the timepoints from the interval instead of the one of the assertion
              for {
                start <- fullToCoreTimepoint(interval.start)
                end <- fullToCoreTimepoint(interval.end)
              } yield (start, end)
            } else {
              ???
//              (assertion.start,
//               assertion.end,
//               Seq(
//                 Seq(core.TimepointDeclaration(assertion.start)),
//                 Seq(core.TimepointDeclaration(assertion.end)),
//                 interval.start === assertion.start,
//                 interval.end === assertion.end
//               ).flatten)
            }
          case full.Contains(interval) =>
            for {
              itvStart <- fullToCoreTimepoint(interval.start)
              itvEnd <- fullToCoreTimepoint(interval.end)
              _ <- TransM.unit(
                core.TimepointDeclaration(assertion.start),
                core.TimepointDeclaration(assertion.end),
                itvStart <= assertion.start,
                assertion.end <= itvEnd
              )
            } yield (assertion.start, assertion.end)
//            (assertion.start,
//             assertion.end,
//             Seq(
//               core.TimepointDeclaration(assertion.start),
//               core.TimepointDeclaration(assertion.end),
//               interval.start <= assertion.start,
//               assertion.end <= interval.end
//             ))
        }
      assertion match {
        case full.TimedEqualAssertion(fluent, value, parent, name) =>
          for {
            se <- startEnd
            (start, end) = se
            coreFluent <- timedSymExpr2CoreFluent(fluent)
            coreValue <- staticExprToVarTRANSM(value)
            _ <- TransM.unit(
              core.TimedEqualAssertion(start, end, coreFluent, coreValue),
              start <= end
            )
          } yield ()
//          val (coreFluent, fluentStatement) = timedSymExpr2CoreFluent(fluent)
//          val (coreValue, valueStatements) = staticExprToVar(value)
//          baseStatements ++ fluentStatement ++ valueStatements :+ core
//            .TimedEqualAssertion(start, end, coreFluent, coreValue) :+
//            (start <= end)
        case full.TimedAssignmentAssertion(fluent, value, parent, name) =>
          for {
            se <- startEnd
            (start, end) = se
            coreFluent <- timedSymExpr2CoreFluent(fluent)
            coreValue <- staticExprToVarTRANSM(value)
            _ <- TransM.unit(
              core.TimedAssignmentAssertion(start, end, coreFluent, coreValue),
              start <= end
            )
          } yield ()
//          val (coreFluent, fluentStatement) = timedSymExpr2CoreFluent(fluent)
//          val (coreValue, valueStatements) = staticExprToVar(value)
//          baseStatements ++ fluentStatement ++ valueStatements :+ core
//            .TimedAssignmentAssertion(start, end, coreFluent, coreValue) :+
//            (start <= end)
        case full.TimedTransitionAssertion(fluent, fromValue, toValue, parent, name) =>
          for {
            se <- startEnd
            (start, end) = se
            coreFluent <- timedSymExpr2CoreFluent(fluent)
            coreFrom <- staticExprToVarTRANSM(fromValue)
            coreTo <- staticExprToVarTRANSM(toValue)
            _ <- TransM.unit(
              core.TimedTransitionAssertion(start, end, coreFluent, coreFrom, coreTo),
              start < end
            )
          } yield ()
//          val (coreFluent, fluentStatement) = timedSymExpr2CoreFluent(fluent)
//          val (coreFromValue, fromValueStatements) = staticExprToVar(fromValue)
//          val (coreToValue, toValueStatements) = staticExprToVar(toValue)
//          baseStatements ++ fluentStatement ++ fromValueStatements ++ toValueStatements :+ core
//            .TimedTransitionAssertion(start, end, coreFluent, coreFromValue, coreToValue) :+
//            (start < end)
      }
  }

  def trans(model: full.Model, config: Config = Config()): Seq[core.InModuleBlock] = {
    model.store.blocks.flatMap {
      case x: core.InModuleBlock  => Seq(x)
      case x: full.Statement      => trans(x)(Context(model.scope, config)).accumulatedStatement
      case x: full.ActionTemplate => Seq(trans(x)(Context(model.scope, config)))
    }
  }

}
