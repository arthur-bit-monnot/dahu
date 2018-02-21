package dahu.cerberdo

import cats.{~>, Id}
import dahu.expr._
import dahu.recursion.InputF
import shapeless.{Generic, HNil}

object Planning {

  // solution structure
  type Timepoint = Int
  type Duration = Int

  class CSP

  class Timeline[+V] {}
  class Token[V](start: Timepoint, duration: Duration, end: Timepoint, tl: Timeline[V], value: V)
  class Observation[V](time: Int, tl: Timeline[V], value: V)

  class FilledTimeline[V](tl: Timeline[V], tokens: Seq[Token[V]], observations: Set[Observation[V]])

  class Frame(timelines: Set[Timeline[Any]], csp: CSP)

  // planning structure
  object Domain {
    sealed abstract class Item
    object Item {
      object a extends Item
      object b extends Item

      val allInstances = List(a, b)
    }
    sealed abstract class Loc
    object Loc {
      case object la extends Loc
      case object lb extends Loc
      case object processed extends Loc

      val allInstances = List(la, lb, processed)
    }

    val atTimelines: Map[Item, Timeline[Loc]] =
      Item.allInstances.map(i => i -> new Timeline[Loc]).toMap

    case class Token[+V](s: Expr[Int],
                         d: Expr[Int],
                         e: Expr[Int],
                         present: Expr[Boolean],
                         sv: Timeline[V],
                         value: Expr[V],
                         isEffect: Boolean)

    var tpId = 0
    def intVar(name: String): Expr[Int] = Input[Int](name)
    def tp(name: String): Expr[Int] = intVar(name)
    def token[V](name: String,
                 present: Expr[Boolean],
                 sv: Timeline[V],
                 value: Expr[V],
                 isEffect: Boolean): Token[V] =
      Token(tp(s"start($name)"),
            intVar(s"duration($name)"),
            tp(s"end($name)"),
            present,
            sv,
            value,
            isEffect)
    def effectToken[V](name: String, present: Expr[Boolean], sv: Timeline[V], value: Expr[V]) =
      token(name, present, sv, value, isEffect = true)
    def condToken[V](name: String, present: Expr[Boolean], sv: Timeline[V], value: Expr[V]) =
      token(name, present, sv, value, isEffect = false)

    case class Action(name: String,
                      present: Expr[Boolean],
                      tokens: Seq[Token[Any]],
                      constraints: Expr[Boolean])

    // action process(Item i),
    val actions = for(i <- Item.allInstances) {
      val baseName = s"pick_$i"
      val present = Input[Boolean](s"pick_$i")
      val from = Input[Loc](s"pick_from_$i")
      val sv = atTimelines(i)
      val condition = condToken(baseName + "_from", present, sv, from)
      val effect = effectToken(baseName + "_to", present, sv, Cst(Loc.processed))

      import dahu.expr.dsl._
      val constraints = condition.e === effect.s
      Action(baseName, present, List(condition, effect), constraints)
    }

    val initState: List[Token[Loc]] = List(
      effectToken(s"init_a", Cst(true), atTimelines(Item.a), Cst(Loc.la)),
      effectToken(s"init_b", Cst(true), atTimelines(Item.b), Cst(Loc.lb))
    )

    val goalState: List[Token[Loc]] = List(
      condToken(s"processed_a", Cst(true), atTimelines(Item.a), Cst(Loc.processed))
    )
  }

  object Structs extends App {

    case class IntervalF[F[_]](start: F[Int], duration: F[Int], end: F[Int], present: F[Boolean])
    type Interval = IntervalF[Id]
    type IntervalExpr = Product[IntervalF, IntervalF[Expr]]

//    type IntervalExpr = Product[IntervalF]
    case class TokenF[F[_], V](itv: F[Interval],
                               timeline: F[Timeline[V]],
                               value: F[V],
                               isEffect: F[Boolean])
    type Token[V] = TokenF[Id, V]

    class TokenExpr[V](override val itv: IntervalExpr,
                       override val timeline: Cst[Timeline[V]],
                       override val value: Expr[V],
                       override val isEffect: Cst[Boolean])
        extends TokenF[Expr, V](itv, timeline, value, isEffect)

    def interval: IntervalExpr =
      Product[IntervalF, IntervalF[Expr]](IntervalF[Expr](Cst(1), Cst(2), Cst(3), Cst(true)))

//    val x = Product(IntervalF[Expr](Cst(1), Cst(1), Cst(2), Cst(true)))
    import scala.reflect.runtime.universe._
    val x = weakTypeTag[IntervalF[Id]]
    println(x.toString())

//    case class Arr[V](members: Seq[Expr[V]]) extends Struct with Expr[Seq[V]] { // Expr[Seq[V]]
//      override def nested: Seq[Expr[Any]] = members
//    }
//    abstract class TokF[F[_]](start: F[Int], duration: F[Int], end: F[Int], present: F[Boolean])
//    type Tok  = TokF[Id]
//    type TokE = TokF[Expr]
//    case class TokExpr(start: Expr[Int],
//                       duration: Expr[Int],
//                       end: Expr[Int],
//                       present: Expr[Boolean])
//        extends Struct
//        with Expr[Tok] {
//      def nested = Seq(start, duration, end, present)
//    }
//    case class ActionF[F[+ _]](name: String, start: F[Int], end: F[Int], tokens: Arr[Tok])
//
//    type Action = ActionF[Id]
  }

}
