package dahu.model.validation

import cats._
import cats.implicits._
import dahu.model.input.{Expr, Ident, TypedIdent}
import dahu.model.interpreter.{Interpreter, LambdaInterpreter}
import dahu.model.interpreter.LambdaInterpreter._
import dahu.model.ir.StaticF
import dahu.model.math.{bool, int}
import dahu.model.problem.{API, LazyTree}
import dahu.model.problem.SatisfactionProblem.IR
import dahu.model.types.Tag._
import dahu.model.types._
import dahu.recursion.FAlgebra
import dahu.utils.SFunctor

import scala.collection.mutable
import scala.util.Random

object Validation {

  /** Checks that all possible methods to evaluate `e` give the same result and return it. */
  def checkedEval[A](e: Expr[A],
                     in: TypedIdent[Any] => Option[Value]): LambdaInterpreter.Result[Any] = {
    checkedEval(e, multiLayerFactory.build(e).evaluator(in))
  }
  private def checkedEval[A](e: Expr[A], evaluator: Evaluator): Result[A] = {
    evaluator.eval(e)
  }

  def deepCheckedEval[A](e: Expr[A], layer: Evaluator): LambdaInterpreter.Result[Any] = {
    Expr.dagInstance.topologicalOrderFromRoot(e).reverse.foreach { se =>
      layer.eval(se)
    }
    layer.eval(e)
  }

  /** Checks that all expression yield the same result. */
  def checkedMultiEval[A](es: Iterable[Expr[A]],
                          in: TypedIdent[Any] => Option[Value]): Result[Any] = {
    val evals = es.toList.map(e => checkedEval(e, in))
    assert(allEquals(evals))
    evals.head
  }

  /** Checks that all methods for evaluating `e`, return `res` */
  def assertEvaluatesTo[A](e: Expr[A], in: TypedIdent[Any] => Option[Any])(res: Result[A]): Unit = {
    val v = checkedEval(e, in.asInstanceOf[TypedIdent[Any] => Option[Value]])
    assert(v == res, s"$v != $res")
  }

  /** Checks that all methods for evaluating `e`, return `res` */
  def assertAllEvaluateTo[A](es: Iterable[Expr[A]], in: TypedIdent[Any] => Option[Any])(
      res: Result[A]): Unit = {
    val v = checkedMultiEval(es, in.asInstanceOf[TypedIdent[Any] => Option[Value]])
    assert(v == res, s"$v != $res")
  }

  def allEquals[A](as: List[A]): Boolean = as match {
    case Nil            => true
    case _ :: Nil       => true
    case a :: b :: tail => a == b && allEquals(b :: tail)
  }

  def fuzzedEvalCheck(expr: Expr[Any], numFuzz: Int = 10): Unit = {
    for(seed <- 1 to numFuzz) {
      val input = fuzzer(seed)
      checkedEval(expr, input)
    }
  }
  def deepFuzzedEvalCheck(expr: Expr[Any], numFuzz: Int = 10): Unit = {
    val layer = multiLayerFactory.build(expr)
    for(seed <- 1 to numFuzz) {
      val input = fuzzer(seed)
      deepCheckedEval(expr, layer.evaluator(input))
    }
  }

  def fuzzer(seed: Int): TypedIdent[Any] => Option[Value] = {
    val rand = new Random(seed)
    rand.nextInt(); rand.nextInt() // had some experience in the past were the first generations were not random
    val map = mutable.Map[Ident, Value]()

    tid =>
      {
        map.get(tid.id) match {
          case Some(v) => Some(v)
          case None =>
            val optV = tid.typ match {
              case Tag.ofBoolean => Some(Value(rand.nextBoolean()))
              case t: TagIsoInt[_] =>
                val intervalSize = t.max - t.min + 1
                val v = rand.nextInt(intervalSize) + t.min
                assert(t.min <= v && v <= t.max)
                Some(t.toValue(v))
              case x =>
                dahu.utils.debug.warning(s"Fuzzer does not support this Tag: $x")
                None
            }
            optV.foreach(v => map.update(tid.id, v))
            optV
        }
      }
  }

  trait Evaluator {
    def eval[A](e: Expr[A]): LambdaInterpreter.Result[A]
    def rep[A](e: Expr[A]): Any
  }
  trait Layer {
    def evaluator(in: TypedIdent[Any] => Option[Value]): Evaluator
  }
  trait LayerFactory {
    def build(e: Expr[Any]): Layer
  }

  lazy val layerFactories: List[LayerFactory] =
    List(afterElimDyna, afterExpandLambdas, afterTotalization)

  val afterElimDyna: LayerFactory = new LayerFactory {
    override def build(e: Expr[Any]): Layer = new Layer {
      private val parsed = API.parse(e)
      private val ev = API.eliminitateDynamics(parsed).tree.fixID
      override def evaluator(in: TypedIdent[Any] => Option[Value]): Evaluator = {
        val evaluated = ev.cata(LambdaInterpreter.partialEvalAlgebra2(in))
        new Evaluator {
          override def eval[A](e: Expr[A]): Result[A] = evaluated.get(e).asInstanceOf[Result[A]]
          override def rep[A](e: Expr[A]): Any =
            SFunctor[StaticF].smap(ev.getExt(e))(i => evaluated.getInternal(i))
        }
      }
    }
  }

  val afterExpandLambdas: LayerFactory = new LayerFactory {
    override def build(e: Expr[Any]): Layer = new Layer {
      private val parsed = API.parse(e)
      private val ev = API.expandLambdas(API.eliminitateDynamics(parsed)).tree.fixID
      override def evaluator(in: TypedIdent[Any] => Option[Value]): Evaluator = {
        val evaluated = ev.cata(LambdaInterpreter.partialEvalAlgebra2(in))
        new Evaluator {
          override def eval[A](e: Expr[A]): Result[A] = evaluated.get(e).asInstanceOf[Result[A]]
          override def rep[A](e: Expr[A]): Any =
            SFunctor[StaticF].smap(ev.getExt(e))(i => evaluated.getInternal(i))
        }
      }
    }
  }

  val afterTotalization: LayerFactory = new LayerFactory {
    override def build(e: Expr[Any]): Layer = new Layer {
      private val parsed = API.parse(e)
      private val ev = API.makeTotal(API.expandLambdas(API.eliminitateDynamics(parsed))).tree.fixID
      override def evaluator(in: TypedIdent[Any] => Option[Value]): Evaluator = {
        val evaluated = ev.cata(Interpreter.evalAlgebra(in.andThen(_.get)))
        new Evaluator {
          override def eval[A](e: Expr[A]): Result[A] = evaluated.get(e) match {
            case IR(_, false, _) => LambdaInterpreter.Empty
            case IR(_, _, false) => LambdaInterpreter.ConstraintViolated
            case IR(v, _, _)     => LambdaInterpreter.Res(v.asInstanceOf[A])
          }
          override def rep[A](e: Expr[A]): Any =
            SFunctor[IR].smap(ev.getTreeRoot(e))(i => evaluated.getInternal(i))
        }
      }
    }
  }

  val multiLayerFactory: LayerFactory = new LayerFactory {
    override def build(e: Expr[Any]): Layer = {
      val layers = layerFactories.map(_.build(e))
      new Layer {
        override def evaluator(in: TypedIdent[Any] => Option[Value]): Evaluator = {
          val evaluators = layers.map(_.evaluator(in))

          new Evaluator {
            override def eval[A](e: Expr[A]): Result[A] = {
              val evals: List[Result[A]] = evaluators.map(f => f.eval(e))
              if(!allEquals(evals)) {
                for(ev <- evaluators) {
                  println(ev)
                  println("  " + ev.rep(e))
                  println("  -> " + ev.eval(e))
                }
              }
              assert(allEquals(evals), s"Not all evaluations are equal: $evals")
              evals.head
            }

            override def rep[A](e: Expr[A]): Any = evaluators.map(_.rep(e))
          }
        }
      }
    }
  }
}
