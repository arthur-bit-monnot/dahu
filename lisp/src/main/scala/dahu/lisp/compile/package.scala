package dahu.lisp

import dahu.lisp.keywords._
import dahu.model.functions.Fun
import dahu.model.ir._
import dahu.model.types.SequenceTag.SequenceTagImplAny
import dahu.model.types._
import dahu.utils._

package object compile {

  import Env._

  def eval(e: SExpr, env: Env): I = {
    val TRUE = env.getId(dahu.model.math.bool.TrueF)
    val FALSE = env.getId(dahu.model.math.bool.FalseF)
    def typeOf(i: I): Type = env.extractValue(i).typ
    def record(v: V): I = env.getId(v)
    def get(i: I): V = env.extractValue(i)

    e match {
      case x: Sym    => env.getValue(x) //env.getCallSite(x).dynamicInvoker().invoke()
      case x: Int    => env.getId(CstF(Value(x), Tag.ofInt))
      case x: String => env.getId(CstF(Value(x), Tag.ofString))
      case true      => TRUE
      case false     => FALSE
      case DO :: rest =>
        assert(rest.nonEmpty)
        rest.map(eval(_, env)).last
      case IF :: cond :: onTrue :: onFalse :: Nil =>
        val c = eval(cond, env)
        c match {
          case TRUE                            => eval(onTrue, env)
          case FALSE                           => eval(onFalse, env)
          case x if typeOf(x) != Tag.ofBoolean => error(s"Unexpected condition result: ${get(x)}")
          case _ =>
            val t = eval(onTrue, env)
            val f = eval(onFalse, env)
            env.getId(ITEF(c, t, f, typeOf(t)))

        }
      case QUOTE :: (e: List[_]) :: Nil =>
        record(
          SequenceF[I](
            e.map(eval(_, env)),
            SequenceTagImplAny(Tag.unsafe.ofAny)
          ))
//      case ATOM :: (_: List[_]) :: Nil => false
//      case ATOM :: _ :: Nil            => true
//      case LAMBDA :: (args: Seq[Sym]) :: exp :: Nil =>
//        mh.mh((params: Array[AnyRef]) => {
//            eval(exp, env.subEnv(args.toArray, params))
//          })
//          .asCollector(classOf[Array[AnyRef]], args.size)
      case DEFINE :: Sym(label) :: expr :: Nil =>
        val value = eval(expr, env)
        env.setConstantValue(label, value)
        value
//      case Nil => false
      case l: List[_] =>
        l.head match {
          case _: List[_]                   => unsupported(s"nested list $l")
          case LAMBDA | DEFINE | QUOTE | IF => error(s"Malformed expression $l")
          case _                            =>
        }
        val fid :: args = l.map(eval(_, env))
        get(fid) match {
          case CstF(f: Fun[_], _) =>
            record(ComputationF(f, args, f.outType))
          case x =>
            error(s"Expected function (method handle) but got $x")
        }

    }
  }

  def parseEval(str: String, env: Env): I = {
    eval(parse(str), env)
  }

  def format(e: Any): String = e match {
    case l: List[_] => l.map(format).mkString("(", " ", ")")
    case e          => e.toString
  }

}
