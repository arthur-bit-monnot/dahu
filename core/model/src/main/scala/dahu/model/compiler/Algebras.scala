package dahu.model.compiler

import dahu.utils.Vec
import dahu.maps.ArrayMap
import dahu.model.input._
import dahu.model.ir._
import dahu.model.types.Value
import dahu.recursion._
import dahu.recursion.Recursion._

object Algebras {

  val coalgebra: FCoalgebra[ExprF, Expr[_]] = {
    case x @ Input(id)                       => InputF(id, id.typ)
    case x @ Cst(value)                      => CstF(Value(value), x.typ)
    case x: Computation[_]                   => ComputationF(x.f, x.args, x.typ)
    case x @ SubjectTo(value, cond)          => Partial(value, cond, x.typ)
    case x @ UniversalSubjectTo(value, cond) => UniversalPartial(value, cond, x.typ)
    case x @ Product(value)                  => ProductF(x.members, x.typ)
    case x @ Sequence(members)               => SequenceF(members, x.typ)
    case x @ Optional(value, present)        => OptionalF(value, present, x.typ)
    case x @ ITE(cond, onTrue, onFalse)      => ITEF(cond, onTrue, onFalse, x.typ)
    case Present(partial)                    => PresentF(partial)
    case Valid(partial)                      => ValidF(partial)
    case x @ Dynamic(f, monoid, accept)      => DynamicF(f, monoid, accept, x.typ)
    case x @ DynamicProvider(e, p)           => DynamicProviderF(e, p, x.typ)
    case x: Lambda[_, _]                     => LambdaF(x.inputVar, x.parameterizedTree, x.id, x.typ)
    case x @ Apply(lambda, param)            => ApplyF(lambda, param, x.typ)
    case x @ Lambda.Param(id)                => LambdaParamF(id, x.typ)
  }

  val printAlgebra: FAlgebra[ExprF, String] = {
    case InputF(v, _)                   => "$" + v
    case CstF(v, _)                     => v.toString
    case ComputationF(f, args, _)       => f.name + args.mkString("(", ",", ")")
    case Partial(value, cond, _)        => s"$value ?($cond)"
    case SequenceF(members, _)          => members.mkString("[", ", ", "]")
    case ProductF(members, _)           => members.mkString("(", ", ", ")")
    case OptionalF(value, present, _)   => s"($value (presence: $present))"
    case ITEF(cond, onTrue, onFalse, _) => s"ite($cond, $onTrue, $onFalse)"
    case PresentF(partial)              => s"present($partial)"
    case ValidF(partial)                => s"valid($partial)"
    case DynamicF(f, monoid, accept, _) => s"dyn($f // $monoid)"
    case DynamicProviderF(e, p, _)      => s"($e (providing: $p))"
    case LambdaF(in, tree, _, _)        => s"($in ↦ $tree)"
    case ApplyF(lambda, param, _)       => s"$lambda $param"
    case NoopF(e, _)                    => e
    case LambdaParamF(id, _)            => s"?$id"
  }

  def format(e: Fix[Total]): String = cata(printAlgebraTree[Total])(e).mkString(30)

  private class Writer(val maxExprSize: Int) {
    def mkString: String = sb.mkString

    private val sb = new StringBuilder
    private var currentIdentation: Int = 0
    private var currentLine: Int = 0

    private def spaceLeft: Int = maxExprSize - (currentLine - currentIdentation)
    private def append(str: String): Unit = sb.append(str)
    private def write(str: String): Unit = {
      assert(!str.contains("\n"))
      append(str)
      currentLine += str.length
    }
    private def lineBreak(): Unit = {
      if(currentLine != currentIdentation) {
        append("\n")
        append(" " * currentIdentation)
        currentLine = currentIdentation
      }
    }
    def add(t: StringTree): Unit = {
      t match {
        case StringLeaf(s) => write(s)
        case Node(l, r) =>
          add(l)
          add(r)
        case t @ TreeSeq(ns, sep, before, after) if t.length <= spaceLeft =>
          write(before)
          for(i <- ns.indices) {
            add(ns(i))
            if(i == ns.length - 1)
              write(after)
            else
              write(sep)
          }
        case t @ TreeSeq(ns, sep, before, after) =>
          write(before)
          val prevIdent = currentIdentation
          currentIdentation = currentLine
          for(i <- ns.indices) {
            add(ns(i))
            if(i == ns.length - 1)
              write(after)
            else
              write(sep)
            lineBreak()
          }
          currentIdentation = prevIdent
      }
    }
  }
  sealed trait StringTree {
    def length: Int
    def ++(right: StringTree): StringTree = Node(this, right)
    def ++(right: String): StringTree = Node(this, StringLeaf(right))

    def mkString(maxExprSize: Int = 0): String = {
      val writer = new Writer(maxExprSize)
      writer.add(this)
      writer.mkString
    }
  }
  private final case class StringLeaf(v: String) extends StringTree {
    override def length: Int = v.length
  }
  private final case class Node(l: StringTree, r: StringTree) extends StringTree {
    override val length: Int = l.length + r.length
  }
  private final case class TreeSeq(ns: Vec[StringTree],
                                   separator: String = ", ",
                                   before: String = "(",
                                   after: String = ")")
      extends StringTree {
    override def length: Int =
      before.length + after.length + ns.foldLeft(0)((acc, a) => acc + a.length) + math.max(
        ns.length - 1,
        0) * separator.length
  }
  def printAlgebraTree[F[X] <: ExprF[X]]: FAlgebra[F, StringTree] = printAlgebraTreeImpl
  private val printAlgebraTreeImpl: FAlgebra[ExprF, StringTree] = {
    case InputF(v, _)                                                           => StringLeaf("$" + v)
    case CstF(v, _)                                                             => StringLeaf(v.toString)
    case c @ ComputationF(f, Vec(a), _) if f.name == "box" || f.name == "unbox" => a
    case c @ ComputationF(f, args, _) =>
      TreeSeq(args, before = f.name + "(", after = ")")
    case SequenceF(members, _) => TreeSeq(members, before = "[", after = "]")
    case ProductF(members, _)  => TreeSeq(members)
    case ITEF(cond, onTrue, onFalse, _) =>
      TreeSeq(Vec(cond, onTrue, onFalse), before = "ite(", after = ")") //s"ite($cond, $onTrue, $onFalse)"
    case ApplyF(lbd, param, _)          => TreeSeq(Vec(lbd, param))
    case NoopF(e, _)                    => e
    case Partial(value, condition, _)   => TreeSeq(Vec(value, condition), before = "subjectTo(")
    case OptionalF(value, condition, _) => TreeSeq(Vec(value, condition), before = "optional(")
    case PresentF(v)                    => TreeSeq(Vec(v), before = "present(")
    case x: LambdaParamF[_]             => StringLeaf(x.toString)
    case LambdaF(in, tree, _, _) =>
      TreeSeq(Vec(in, tree), before = "Lbd:", separator = " -> ", after = "")
  }

  def pprint(prg: Expr[_]): String =
    hylo(coalgebra, printAlgebra)(prg)

  def pprint[T](coalgebra: FCoalgebra[ExprF, T], expr: T): StringTree =
    hylo(coalgebra, printAlgebraTreeImpl)(expr)

  def parse[T](e: Expr[T]): AST[Expr[_]] =
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
