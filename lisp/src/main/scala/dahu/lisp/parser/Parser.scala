package dahu.lisp.parser

import fastparse._
import dahu.lisp.Sym

import scala.util.Try

object Parser {

  import dahu.lisp.keywords._

  import fastparse.noApi._
  import ClojureWhiteSpace.whiteApi._
  type P[X] = Parser[X]

  val allowInIdents = "a-zA-Z"
  def allowed(c: Char): Boolean = c match {
    case ' ' | '\n' | '\t' | '(' | ')' | '[' | ']' => false
    case _                                         => true
  }

  def asInt(str: String): Option[Int] = {
    if(str.endsWith("i"))
      Try(str.dropRight(1).toInt).toOption
    else None
  }
  def asDouble(str: String): Option[Double] =
    Try(str.toDouble).toOption

  val token: P[E] = ("\"" ~/ CharsWhile(_ != '"').! ~ "\""
    | CharsWhile(allowed, min = 1).!.map(
      s => asDouble(s).orElse(asInt(s)).getOrElse(Sym(s))
    ))

  val atom: P[Sym] = token
    .filter {
      case _: Sym => true
      case _      => false
    }
    .map(_.asInstanceOf[Sym])

  val expr: P[E] = P(
    ("(" ~ "defn" ~/ atom ~/ "[" ~/ atom.rep.map(_.toList) ~/ "]" ~ expr ~ ")").map {
      case (name, params, expr) => List(DEFINE, name, List(LAMBDA, params, expr))
    }
      | ("(" ~ "fn" ~/ "[" ~/ atom.rep.map(_.toList) ~/ "]" ~ expr ~ ")").map {
        case (params, expr) => List(LAMBDA, params, expr)
      }
      | "(" ~/ expr.rep.map(_.toList) ~ ")"
      | "'" ~/ expr.map(e => QUOTE :: e :: Nil)

      | token
  )

  val parser: P[E] = Pass ~ expr ~ End
  val parserMany: P[Seq[E]] = Pass ~ expr.rep ~ End

}
