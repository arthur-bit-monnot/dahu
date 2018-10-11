package dahu.utils

object errors {

  sealed trait ErrorType
  object ErrorType {
    case object Unsupported extends ErrorType
    case object Unspecified extends ErrorType
  }
  import ErrorType._

  final case class Error(clazz: ErrorType, msg: String) extends Throwable(msg) {
    override def toString: String = s"$clazz: $msg"
  }

  private def err(clazz: ErrorType, msg: String): Throwable = Error(clazz, msg)
  def err(msg: String): Throwable = Error(Unspecified, msg)

  def unexpected: Nothing = throw new AssertionError("Unexpected")
  def unexpected(msg: String): Nothing = throw new AssertionError(msg)
  def unexpected(msg: String, cause: Throwable): Nothing = throw new AssertionError(msg, cause)

  def unsupported: Nothing = throw err(Unsupported, "")
  def unsupported(msg: String): Nothing = throw err(Unsupported, msg)
}
