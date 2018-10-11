package dahu.planning.model.common

object operators {

  type TypingResult = Either[String, Type]

  sealed abstract class Associativity
  object Associativity {
    case object Left extends Associativity
    case object Right extends Associativity
    case object Non extends Associativity
  }

  private implicit class TypeOps(private val tpe: Type) extends AnyVal {
    def isNum: Boolean = tpe.isSubtypeOf(Type.Reals)
    def isInt: Boolean = tpe.isSubtypeOf(Type.Integers)
    def isFloat: Boolean = isNum && !isInt
  }

  sealed trait Operator {
    def op: String
    def precedence: Int
  }
  sealed abstract class UnaryOperator(val op: String, val precedence: Int) extends Operator {
    def tpe(lhs: Type): TypingResult
  }

  sealed abstract class BinaryOperator(val op: String,
                                       val precedence: Int,
                                       val associativity: Associativity)
      extends Operator {
    def tpe(lhs: Type, rhs: Type)(implicit predef: Predef): TypingResult
  }

  sealed abstract class BooleanBinaryOperator(op: String,
                                              precedence: Int,
                                              associativity: Associativity)
      extends BinaryOperator(op, precedence, associativity) {
    override def tpe(lhs: Type, rhs: Type)(implicit predef: Predef): TypingResult =
      (lhs, rhs) match {
        case (predef.Boolean, predef.Boolean) => Right(predef.Boolean)
        case _                                => Left("On of the subexpression does not have the boolean type.")
      }
  }

  sealed abstract class NumericBinaryOperator(op: String, precedence: Int)
      extends BinaryOperator(op, precedence, Associativity.Left) {
    override def tpe(lhs: Type, rhs: Type)(implicit predef: Predef): TypingResult = {
      lhs.lowestCommonAncestor(rhs) match {
        case None if !lhs.isNum       => Left(s"Left hand side is not a numeric type but: $lhs")
        case None if !rhs.isNum       => Left(s"Right hand side is not a numeric type but: $rhs")
        case Some(tpe) if tpe.isInt   => Right(Type.Integers)
        case Some(tpe) if tpe.isFloat => Right(Type.Reals)
        case x                        => sys.error(s"Unhandled case: $x")

      }
    }
  }

  sealed abstract class NumericComparison(op: String, precedence: Int)
      extends BinaryOperator(op, precedence, Associativity.Non) {
    override def tpe(lhs: Type, rhs: Type)(implicit predef: Predef): TypingResult = {
      lhs.lowestCommonAncestor(rhs) match {
        case None if !lhs.isNum     => Left(s"Left hand side is not a numeric type but: $lhs")
        case None if !rhs.isNum     => Left(s"Right hand side is not a numeric type but: $rhs")
        case Some(tpe) if tpe.isNum => Right(predef.Boolean)
        case x                      => sys.error(s"Unhandled case: $x")
      }
    }
  }

  sealed abstract class EqualityOperator(op: String, precedence: Int)
      extends BinaryOperator(op, precedence, Associativity.Non) {
    override def tpe(lhs: Type, rhs: Type)(implicit predef: Predef): TypingResult = {
      if(!lhs.overlaps(rhs))
        Left(s"Comparing unrelated types: $lhs $rhs")
      else
        Right(predef.Boolean)
    }
  }

  case object Implies extends BooleanBinaryOperator("implies", 1, Associativity.Right)
  case object Xor extends BooleanBinaryOperator("xor", 3, Associativity.Left)
  case object Or extends BooleanBinaryOperator("or", 4, Associativity.Left)
  case object And extends BooleanBinaryOperator("and", 5, Associativity.Left)

  case object Eq extends EqualityOperator("==", 7)
  case object Neq extends EqualityOperator("!=", 7)
  case object LT extends NumericComparison("<", 7)
  case object GT extends NumericComparison(">", 7)
  case object LEQ extends NumericComparison("<=", 7)
  case object GEQ extends NumericComparison(">=", 7)

  case object Add extends NumericBinaryOperator("+", 13)
  case object Sub extends NumericBinaryOperator("-", 13)
  case object Mul extends NumericBinaryOperator("*", 14)
  case object Div extends NumericBinaryOperator("/", 14)

  case object Minus extends UnaryOperator("-", 15) {
    override def tpe(lhs: Type): TypingResult =
      if(lhs.isInt || lhs.isFloat) Right(lhs)
      else Left(s"Non numeric type $lhs")
  }
  case object Not extends UnaryOperator(op = "!", precedence = 16) {
    override def tpe(lhs: Type): TypingResult = {
      if(lhs.isBoolean) Right(lhs)
      else Left(s"Non boolean type: $lhs")
    }
  }

  val all: Set[Operator] =
    Set(Implies, Xor, Or, And, Eq, Neq, LT, GT, LEQ, GEQ, Add, Sub, Mul, Div, Minus)
  val layeredOps = computeLayeredOps(all)

  private def computeLayeredOps(ops: Set[Operator]): Seq[OperatorGroup] = {
    all
      .groupBy(_.precedence)
      .values
      .map(opSet => {
        opSet.head match {
          case bin: BinaryOperator =>
            BinOperatorGroup(opSet.map(_.asInstanceOf[BinaryOperator]),
                             bin.precedence,
                             bin.associativity)
          case uni: UnaryOperator =>
            UniOperatorGroup(opSet.map(_.asInstanceOf[UnaryOperator]), uni.precedence)
        }
      })
      .toSeq
      .sortBy(_.precedence)
      .reverse
  }

  sealed trait OperatorGroup {
    def precedence: Int
  }
  case class BinOperatorGroup(ops: Set[BinaryOperator],
                              precedence: Int,
                              associativity: Associativity)
      extends OperatorGroup {
    require(ops.forall(op => op.precedence == precedence && op.associativity == associativity))
  }
  case class UniOperatorGroup(ops: Set[UnaryOperator], precedence: Int) extends OperatorGroup {
    require(ops.forall(op => op.precedence == precedence))
  }
}
