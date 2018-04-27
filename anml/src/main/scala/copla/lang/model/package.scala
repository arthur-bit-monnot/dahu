package copla.lang

package object model {

  val reservedPrefix = "__"
  private[this] var nextID = 0
  def defaultId(): String = reservedPrefix + { nextID += 1; nextID - 1 }

}
