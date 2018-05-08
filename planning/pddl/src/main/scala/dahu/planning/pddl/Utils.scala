package dahu.planning.pddl

import dahu.planning.model.common.{Predef, Term}
import dahu.planning.model.full.CommonTerm

import scala.language.implicitConversions

object Utils {

  private var counter = 0
  def next(): Int = { counter += 1; counter }
  implicit def term2FullModel(v: Term): CommonTerm = CommonTerm(v)

}
