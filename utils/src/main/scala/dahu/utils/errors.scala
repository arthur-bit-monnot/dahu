package dahu.utils
import scala.util.Try

object errors {

  sealed trait ErrorType
  object ErrorType {
    case object Unsupported extends ErrorType
    case object Unspecified extends ErrorType
    case object Unexpected extends ErrorType
  }
  import ErrorType._

  final case class Error(clazz: ErrorType, msg: String, cause: Throwable = null)
      extends Throwable(msg, cause) {
    override def toString: String = s"$clazz: $msg"
  }

  private def err(clazz: ErrorType, msg: String): Throwable = Error(clazz, msg)
  def err(msg: String): Throwable = err(Unspecified, msg)

  def unexpected: Nothing = unexpected("Unexpected")
  def unexpected(msg: String, cause: Throwable = null): Nothing =
    throw Error(Unexpected, msg, cause)

  def unsupported: Nothing = throw err(Unsupported, "")
  def unsupported(msg: String): Nothing = throw err(Unsupported, msg)
}
