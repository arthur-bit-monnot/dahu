package dahu.cerbero

object Algebras {

  import matryoshka._

  import Planning._
  import dahu.expr._
  import dahu.recursion._

  implicit class FunOps[O: WTypeTag](f: Fun[O]) {
    def apply(seq: Ast*): ComputationF[Ast] = ComputationF(f, seq, typeOf[O])
  }

  val coalgebra: Coalgebra[ExprF, Ast] = {
    case IntVar(name)  => InputF(name, typeOf[Int])
    case CstInt(value) => CstF(Value(value), typeOf[Int])

    case BooleanVar(name)  => InputF(name, typeOf[Boolean])
    case CstBoolean(value) => CstF(Value(value), typeOf[Boolean])

    case AbsoluteTime(value)       => CstF(Value(value), typeOf[Int])
    case Timepoint(name)           => InputF(name, typeOf[Int])
    case RelativeTime(base, delay) => int.Add(base, delay)

    case Sequence(members) => ProductF(members, typeOf[Sequence[_]])

    case Opt(value, present)        => ProductF(Seq(value, present), typeOf[Opt[_]])
    case Interval(start, dur, end)  => ProductF(Seq(start, dur, end), typeOf[Interval])
    case Token(itv, _, value, _, _) => ProductF(Seq(itv, value), typeOf[Token])
    case Action(itv, tokens, _)     => ProductF(Seq(itv, Sequence(tokens)), typeOf[Action])

    case And(booleans)       => bool.And(booleans: _*)
    case Or(booleans)        => bool.Or(booleans: _*)
    case Before(left, right) => int.LEQ(left, right)
    case LEQ(left, right)    => int.LEQ(left, right)
    case EQ(l, r)            => int.EQ(l, r)
    case Sum(l, r)           => int.Add(l, r)
    case Not(e)              => bool.Not(e)
  }

}
