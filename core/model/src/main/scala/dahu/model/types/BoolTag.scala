package dahu.model.types
import dahu.model.input.Expr

object Bool {
  val False: Bool = 0.asInstanceOf[Bool]
  val True: Bool = 1.asInstanceOf[Bool]

  def asBool(b: Boolean): Bool = if(b) True else False
  def fromBool(b: Bool): Boolean = if(b == True) true else false

  implicit class BoolOps(val lhs: Bool) extends AnyVal {
    def &&(rhs: Bool): Bool = if(lhs + rhs == 2) True else False
    def ||(rhs: Bool): Bool = if(lhs + rhs >= 1) True else False
    def ^(rhs: Bool): Bool = if(lhs + rhs == 1) True else False
    def unary_! : Bool = if(lhs == True) False else True
  }
}

object BoolTag extends Tag[Bool] with RawInt {

  override def min: Int = 0
  override def max: Int = 1
}
