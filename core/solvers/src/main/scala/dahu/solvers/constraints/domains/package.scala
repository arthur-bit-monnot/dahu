package dahu.constraints

package object domains {

  /** Upper and lower bounds on domain values that are resistant to overflow. */
  val MIN_INT: Int = Integer.MIN_VALUE / 2 + 1
  val MAX_INT: Int = Integer.MAX_VALUE / 2 - 1

  val EmptyDomain = new IntervalDomain(1, 0)

  def SingletonDomain(value: Int) = new IntervalDomain(value, value)

  val True: IntDomain = SingletonDomain(1)
  val False: IntDomain = SingletonDomain(0)
  val TrueOrFalse: IntDomain = new IntervalDomain(0, 1)

  val FullDomain = new IntervalDomain(MIN_INT, MAX_INT)

}
