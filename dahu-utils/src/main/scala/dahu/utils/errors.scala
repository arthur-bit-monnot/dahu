package dahu.utils

object errors {

  final case class Error(msg: String) extends Throwable(msg)

  def err(msg: => String): Throwable = Error(msg)

  def unexpected: Nothing = throw new AssertionError("Unexpected")
  def unexpected(msg: String): Nothing = throw new AssertionError(msg)
}
