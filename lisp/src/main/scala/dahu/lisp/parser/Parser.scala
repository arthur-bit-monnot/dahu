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

  val token: P[E] = (
    "\"" ~/ CharsWhile(_ != '"').! ~ "\""
      | CharsWhile(allowed, min = 1).!.map(s => {
        Try(s.toInt.asInstanceOf[Integer])
          .orElse(Try(s.toDouble.asInstanceOf[java.lang.Double]))
          .getOrElse(Sym(s))
//        Try(s.toInt) match {
//          case scala.util.Success(value) => value.asInstanceOf[Integer]
//          case _ =>
//            Try(s.toDouble.asInstanceOf[java.lang.Double])
//              .getOrElse(Sym(s))
//        }
      })
  )

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
