package dahu.utils

object Errors {

  final case class Error(msg: String) extends Throwable(msg)

  def err(msg: => String): Throwable = Error(msg)

  def unexpected: Nothing = throw new RuntimeException("Unexpected")
  def unexpected(msg: String): Nothing = throw new RuntimeException(msg)
}
