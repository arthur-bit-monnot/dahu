package dahu.utils

import scala.annotation.elidable
import scala.annotation.elidable._

/** Provides a flexible assertion support, related to a debug level.
  *
  * DEBUG_LEVEL == 1, should be suitable for a release version of the software, with inexpensive sanity checks.
  * DEBUG_LEVEL 2 and 3, encompass more expensive sanity checks that should only be made in debug mode.
  * DEBUG_LEVEL 4 is reserved to very expensive computation that should never be used except beside development.
  *
  * Default debug level is 3 when java assertions are enabled (VM option "-ea") and 1 otherwise.
  *
  * All assert methods have lazy evaluation of the all parameters, so invocation should be almost for free when above
  * the current DEBUG_LEVEL.
  *
  * Assertion can be remove at compile time with "-Xelide-below" parameter to scalac.
  */
object debug {

  var DEBUG_LEVEL = 3
  var LOG_LEVEL = 3

  @elidable(ASSERTION)
  @inline
  final def assert1(assertion: => Boolean, message: => Any = {}) {
    if(DEBUG_LEVEL >= 1 && !assertion)
      throw new java.lang.AssertionError("assertion failed: " + message)
  }

  @elidable(FINE)
  @inline
  final def assert2(assertion: => Boolean, message: => Any = {}) {
    if(DEBUG_LEVEL >= 2 && !assertion)
      throw new java.lang.AssertionError("assertion failed: " + message)
  }

  @elidable(FINER)
  @inline
  final def assert3(assertion: => Boolean, message: => Any = {}) {
    if(DEBUG_LEVEL >= 3 && !assertion)
      throw new java.lang.AssertionError("assertion failed: " + message)
  }

  @elidable(FINEST)
  @inline
  final def assert4(assertion: => Boolean, message: => Any = {}) {
    if(DEBUG_LEVEL >= 4 && !assertion)
      throw new java.lang.AssertionError("assertion failed: " + message)
  }

  private var lastLog = System.currentTimeMillis()
  private var prevNeedsTime = false

  def out(msg: String, withDuration: Boolean = false): Unit = {
    val time = System.currentTimeMillis()
    if(prevNeedsTime)
      println(s"\t[${time-lastLog}]")
    if(withDuration) {
      print(msg)
      prevNeedsTime = true
      lastLog = time
    } else {
      println(msg)
      prevNeedsTime = false
    }

  }

  @inline final def warning(msg: => String): Unit = if(LOG_LEVEL >= 1) println("warning: " + msg)
  @inline final def info(msg: => String): Unit =
    if(LOG_LEVEL >= 3) {
      out("info: " + msg, withDuration = true)
    }

  /** Use to annotate blocks that should not be in hot paths, typically here to maintain genericity.
    *
    * Intended use is to gather runtime information to identify blocks that should be further optimized.
    */
  final def slow[T](value: T): T = value

  final def approximation[T](value: T): T = value

  implicit final class IgnoreResultOps[T](private val value: T) extends AnyVal {

    /** Does nothing, only to ignore compiler warning regarding unused values. */
    def ignoreResult: Unit = ()
  }
}
