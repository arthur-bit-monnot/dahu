package dahu.constraints

package object domains {

  val EmptyDomain = new IntervalDomain(1, 0)

  def SingletonDomain(value: Int) = new IntervalDomain(value, value)

}
