package dahu.lisp

import java.lang.invoke.MethodHandle

import dahu.lisp._
import dahu.lisp.keywords._

package object eval {

  def eval(e: SExpr, env: Env): Any = {
    e match {
      case x: Sym    => env.getValue(x) //env.getCallSite(x).dynamicInvoker().invoke()
      case x: Int    => x
      case x: String => x
      case DO :: rest =>
        rest.foreach(eval(_, env))
      case IF :: cond :: onTrue :: onFalse :: Nil =>
        eval(cond, env) match {
          case true  => eval(onTrue, env)
          case false => eval(onFalse, env)
          case x     => error(s"Unexpected condition result: $x")
        }
      case QUOTE :: e :: Nil           => e
      case ATOM :: (_: List[_]) :: Nil => false
      case ATOM :: _ :: Nil            => true
      case LAMBDA :: (args: Seq[Sym]) :: exp :: Nil =>
        mh.mh((params: Array[AnyRef]) => {
            eval(exp, env.subEnv(args.toArray, params))
          })
          .asCollector(classOf[Array[AnyRef]], args.size)
      case DEFINE :: Sym(label) :: expr :: Nil =>
        val mh = eval(expr, env)
        env.setConstantValue(label, mh)
        mh
      case Nil => false
      case l: List[AnyRef] =>
        l.head match {
          case _: List[_]                   => unsupported(s"nested list $l")
          case LAMBDA | DEFINE | QUOTE | IF => error(s"Malformed expression $l")
          case _                            =>
        }
        val X = l.map(eval(_, env))
        X match {
          case (f: MethodHandle) :: (args: List[AnyRef]) =>
            f.invokeWithArguments(args: _*)
          case x =>
            error(s"Expected function (method handle) but got $x")
        }

    }
  }

  def parseEval(str: String, env: Env): Any = {
    eval(parse(str), env)
  }

  def format(e: Any): String = e match {
    case l: List[_] => l.map(format).mkString("(", " ", ")")
    case e          => e.toString
  }

}
