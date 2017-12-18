package dahu.utils

object Errors {

  def err(msg: => String): Throwable = new Throwable(msg)

}
