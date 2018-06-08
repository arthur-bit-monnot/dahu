package dahu.planning.planner

case class Operator[F[_]](name: F[String],
                          args: F[Seq[Literal]],
                          start: F[Int],
                          end: F[Int],
                          present: F[Boolean]) {
  override def toString = s"[$start, $end] $name($args)"
}
