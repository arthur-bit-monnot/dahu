package dahu.model.interpreter

import dahu.utils._
import utest._

object PEvalTests extends TestSuite {

  def vec(es: PEval[Any]*): Vec[PEval[Any]] = Vec.fromSeq(es)
  def seq(es: PEval[Any]*): PEval[Vec[Any]] = Vec.fromSeq(es).sequence

  override def tests = Tests {
    "sequencing" - {
      seq(PEmpty, PEmpty)                           ==> PEmpty
      seq(PConstraintViolated, PConstraintViolated) ==> PConstraintViolated
      seq(PEmpty, PConstraintViolated)              ==> PEmpty
      seq(PConstraintViolated, PEmpty)              ==> PEmpty
      seq(PEmpty, Unknown(Set(null)))               ==> PEmpty
      seq(Unknown(Set(null)), PEmpty)               ==> PEmpty
      seq(PConstraintViolated, Unknown(Set(null)))  ==> PConstraintViolated
      seq(Unknown(Set(null)), PConstraintViolated)  ==> PConstraintViolated
      seq(FEval(1), PEmpty)                         ==> PEmpty
      seq(FEval(1), PConstraintViolated)            ==> PConstraintViolated
      seq(FEval(1), FEval(2))                       ==> FEval(Vec[Any](1, 2))
    }
  }
}
