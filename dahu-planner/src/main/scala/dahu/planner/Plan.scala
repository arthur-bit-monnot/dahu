package dahu.planner

import copla.lang.model.full.Instance

case class Operator[F[_]](name: F[String],
                          args: F[Seq[Instance]],
                          start: F[Int],
                          end: F[Int],
                          present: F[Boolean])
