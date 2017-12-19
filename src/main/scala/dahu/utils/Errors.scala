package dahu.utils

object Errors {

  final case class Error(msg: String) extends Throwable(msg)

  def err(msg: => String): Throwable = Error(msg)

}
