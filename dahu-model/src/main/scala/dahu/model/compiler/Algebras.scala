package dahu.model.compiler

import dahu.maps.ArrayMap
import dahu.model.input._
import dahu.model.ir._
import dahu.model.types.Value
import dahu.recursion._
import dahu.recursion.Recursion._

import scala.annotation.tailrec

object Algebras {

  val coalgebra: FCoalgebra[ExprF, Tentative[_]] = {
    case x @ Input(name)                => InputF(name, x.typ)
    case x @ Cst(value)                 => CstF(Value(value), x.typ)
    case x: Computation[_]              => ComputationF(x.f, x.args, x.typ)
    case x @ SubjectTo(value, cond)     => Partial(value, cond, x.typ)
    case x @ Product(value)             => ProductF(x.members, x.typ)
    case x @ Optional(value, present)   => OptionalF(value, present, x.typ)
    case x @ ITE(cond, onTrue, onFalse) => ITEF(cond, onTrue, onFalse, x.typ)
    case Present(partial)               => PresentF(partial)
    case Valid(partial)                 => ValidF(partial)
  }

  val printAlgebra: FAlgebra[ExprF, String] = {
    case InputF(v, _)                   => "$" + v
    case CstF(v, _)                     => v.toString
    case ComputationF(f, args, _)       => f.name + args.mkString("(", ",", ")")
    case Partial(value, cond, _)        => s"$value ?($cond)"
    case ProductF(members, _)           => members.mkString("(", ", ", ")")
    case OptionalF(value, present, _)   => s"($value (presence: $present))"
    case ITEF(cond, onTrue, onFalse, _) => s"ite($cond, $onTrue, $onFalse)"
    case PresentF(partial)              => s"present($partial)"
    case ValidF(partial)                => s"valid($partial)"
  }

  def format(e: Fix[Total]): String = cata(printAlgebraMultiLine)(e)

  val printAlgebraMultiLine: FAlgebra[Total, String] = x => {
    def repeatChar(c: Char, n: Int): String = {
      @tailrec
      def repeatCharAcc(c: Char, n: Int, accum: String): String = {
        if(n == 0) accum
        else repeatCharAcc(c, n - 1, accum + c)
      }
      repeatCharAcc(c, n, "")
    }
    def addIndent(str: String, n: Int) = str.replaceAll("\n", "\n" + repeatChar(' ', n))
    def noNewline(c: ComputationF[String]) = c.fun.name + c.args.mkString("(", ", ", ")")
    x match {
      case InputF(v, _) => "$" + v
      case CstF(v, _)   => v.toString
      case c @ ComputationF(f, args, _) =>
        val argsIndent = f.name.length + 1
        val tmp =
          if(noNewline(c).length < 15)
            noNewline(c)
          else
            f.name + args.mkString("(", ",\n", ")")
        addIndent(tmp, argsIndent)
      case ProductF(members, _)           => members.mkString("(", ", ", ")")
      case ITEF(cond, onTrue, onFalse, _) => s"ite($cond, $onTrue, $onFalse)"
    }
  }

  def pprint(prg: Tentative[_]): String =
    hylo(coalgebra, printAlgebra)(prg)

  def pprint[T](coalgebra: FCoalgebra[ExprF, T], expr: T): String =
    hylo(coalgebra, printAlgebra)(expr)

  def parse[T](e: Tentative[T]): AST[Tentative[_]] =
    parse(e, coalgebra)

  def parse[T](t: T, coalgebra: FCoalgebra[ExprF, T]): AST[T] = {
    import scala.collection.mutable

    // algebra that deduplicates the entries, the tree into a directed acyclic graph
    val store = mutable.LinkedHashMap[ExprF[Int], Int]()
    val astStore = mutable.LinkedHashMap[Int, mutable.ArrayBuffer[T]]()
    val alg: FAlgebra[EnvT[T, ExprF, ?], Int] = {
      case EnvT(x, e) =>
        // gets an id for e:
        // if we already met e, then get the id we stored
        // else assign and record a new id
        val i = store.getOrElseUpdate(e, store.size)
        // record that that i map to the input x
        astStore.getOrElseUpdate(i, mutable.ArrayBuffer()) += x
        i
    }

    // this is mainly used to force traversal and populate the hash maps
    val rootExprID: Int = hylo(coalgebra.toAttributeCoalgebra, alg)(t)

    val reverseAstStore = astStore.flatMap(kp => kp._2.map((_, kp._1))).toMap
    val reverseStore = store.map(_.swap).toMap
    val forward: T => Option[Int] = x => reverseAstStore.get(x)
    val expr: Int => ExprF[Int] = reverseStore(_)

    val tree: ArrayMap[ExprF[Int]] = ArrayMap.build(store.values, expr)
    val casted: ArrayMap.Aux[tree.K, ExprF[tree.K]] = tree.map(_.asInstanceOf[ExprF[tree.K]])
    assert(casted.isInDomain(rootExprID))
    val root = rootExprID.asInstanceOf[tree.K]
    val fromInput = forward.asInstanceOf[T => Option[tree.K]]
    val toInput: tree.K => List[T] = k => astStore(k).toList
    new ASTImpl(casted, root, fromInput, toInput)
  }

}
