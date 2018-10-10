package dahu.model.validation

import cats._
import dahu.model.input.{Expr, Ident, TypedIdent}
import dahu.model.interpreter.{FEval, Interpreter, LambdaInterpreter}
import dahu.model.interpreter._
import dahu.model.ir.StaticF
import dahu.model.problem.API
import dahu.model.types._
import dahu.utils._

import scala.collection.mutable
import scala.util.Random

object Validation {

  /** Checks that all possible methods to evaluate `e` give the same result and return it. */
  def checkedEval[A](e: Expr[A], in: TypedIdent => Option[Value]): PEval[Any] = {
    checkedEval(e, multiLayerFactory.build(e).evaluator(in))
  }
  private def checkedEval[A](e: Expr[A], evaluator: Evaluator): PEval[A] = {
    evaluator.eval(e)
  }

  def deepCheckedEval[A](e: Expr[A], layer: Evaluator): PEval[Any] = {
    Expr.dagInstance.topologicalOrderFromRoot(e).reverse.foreach { se =>
      layer.eval(se)
    }
    layer.eval(e)
  }

  /** Checks that all expression yield the same result. */
  def checkedMultiEval[A](es: Iterable[Expr[A]], in: TypedIdent => Option[Value]): PEval[Any] = {
    val evals = es.toList.map(e => checkedEval(e, in))
    assert(allEquals(evals))
    evals.head
  }

  /** Checks that all methods for evaluating `e`, return `res` */
  def assertEvaluatesTo[A](e: Expr[A], in: TypedIdent => Option[Any])(res: PEval[A]): Unit = {
    val v = checkedEval(e, in.asInstanceOf[TypedIdent => Option[Value]])
    assert(v == res, s"$v != $res")
  }

  /** Checks that all methods for evaluating `e`, return `res` */
  def assertAllEvaluateTo[A](es: Iterable[Expr[A]], in: TypedIdent => Option[Any])(
      res: PEval[A]): Unit = {
    val v = checkedMultiEval(es, in.asInstanceOf[TypedIdent => Option[Value]])
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

  def fuzzer(seed: Int): TypedIdent => Option[Value] = {
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
    def eval[A](e: Expr[A]): PEval[A]
    def rep[A](e: Expr[A]): Any
  }
  trait Layer {
    def evaluator(in: TypedIdent => Option[Value]): Evaluator
  }
  trait LayerFactory {
    def build(e: Expr[Any]): Layer
  }

  lazy val layerFactories: List[LayerFactory] =
    List(afterElimDyna, afterExpandLambdas)

  val afterElimDyna: LayerFactory = new LayerFactory {
    override def build(e: Expr[Any]): Layer = new Layer {
      private val parsed = API.parse(e)
      private val ev = API.eliminateDynamics(parsed, ???).tree.fixID
      override def evaluator(in: TypedIdent => Option[Value]): Evaluator = {
        val evaluated = ev.cata(LambdaInterpreter.partialEvalAlgebra(in))
        new Evaluator {
          override def eval[A](e: Expr[A]): PEval[A] =
            evaluated.get(e).asInstanceOf[PEval[A]]
          override def rep[A](e: Expr[A]): Any =
            SFunctor[StaticF].smap(ev.getExt(e))(i => evaluated.getInternal(i))
        }
      }
    }
  }

  val afterExpandLambdas: LayerFactory = new LayerFactory {
    override def build(e: Expr[Any]): Layer = new Layer {
      private val parsed = API.parse(e)
      private val ev = API.expandLambdas(API.eliminateDynamics(parsed, ???)).tree.fixID
      override def evaluator(in: TypedIdent => Option[Value]): Evaluator = {
        val evaluated = ev.cata(LambdaInterpreter.partialEvalAlgebra(in))
        new Evaluator {
          override def eval[A](e: Expr[A]): PEval[A] =
            evaluated.get(e).asInstanceOf[PEval[A]]
          override def rep[A](e: Expr[A]): Any =
            SFunctor[StaticF].smap(ev.getExt(e))(i => evaluated.getInternal(i))
        }
      }
    }
  }

  private def allEquivalent[A](l: List[PEval[A]]): Boolean = l match {
    case Nil      => true
    case a :: Nil => true
    case a :: b :: tail =>
      (a, b) match {
        case _ if a == b        => allEquivalent(b :: tail)
        case (_: Pending[_], _) => allEquivalent(b :: tail)
        case (_, _: Pending[_]) => allEquivalent(b :: tail)
        case _                  => false
      }
  }

  val multiLayerFactory: LayerFactory = new LayerFactory {
    override def build(e: Expr[Any]): Layer = {
      val layers = layerFactories.map(_.build(e))
      new Layer {
        override def evaluator(in: TypedIdent => Option[Value]): Evaluator = {
          val evaluators = layers.map(_.evaluator(in))

          new Evaluator {
            override def eval[A](e: Expr[A]): PEval[A] = {
              val evals: List[PEval[A]] = evaluators.map(f => f.eval(e))
              if(!allEquivalent(evals)) {
                println()
                for(ev <- evaluators) {
                  println(ev)
                  println("  " + ev.rep(e))
                  println("  -> " + ev.eval(e))
                }
              }
              assert(allEquivalent(evals), s"Not all evaluations are equal: $evals")
              evals.head
            }

            override def rep[A](e: Expr[A]): Any = evaluators.map(_.rep(e))
          }
        }
      }
    }
  }
}
