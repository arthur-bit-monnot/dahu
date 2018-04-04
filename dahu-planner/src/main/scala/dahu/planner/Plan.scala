package dahu.planner

import copla.lang.model.full.Instance

case class Operator[F[_]](name: F[String],
                          args: F[List[Instance]],
                          start: F[Int],
                          end: F[Int],
                          present: F[Boolean])
