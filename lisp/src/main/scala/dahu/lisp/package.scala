package dahu
import java.lang.invoke.MethodHandle

package object lisp {
  case class Err(msg: String) extends Exception(msg)

  def error(msg: String): Nothing = throw Err(msg)
  def unsupported(msg: String): Nothing = error(s"Unsupported: $msg")

  object keywords {
    final val QUOTE = Sym("quote")
    final val IF = Sym("if")
    final val DEFINE = Sym("define")
    final val ATOM = Sym("atom")
    final val CONS = Sym("cons")
    final val LAMBDA = Sym("lambda")
    final val DO = Sym("do")
  }
  import keywords._

  type SExpr = Any

  final case class Sym(name: String) {
    override def toString: String = name
  }

  def parse(str: String): SExpr = {
    parser.parse(str) match {
      case fastparse.core.Parsed.Success(sexpr, _) => sexpr
      case x: fastparse.core.Parsed.Failure[_, _]  => error(x.toString())
    }
  }

}
