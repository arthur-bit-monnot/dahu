package dahu.planning.model.transforms

import dahu.planning.anml.parser.{AnmlPredef, InputAnmlModels}
import dahu.planning.model.common.Predef
import dahu.planning.model.core._
import dahu.planning.anml.parser._
import org.scalatest.FunSuite

class FullToCoreTest extends FunSuite {

  implicit val predef: Predef = AnmlPredef

  def asCore(anml: String): Either[String, CoreModel] = {
    Parser.parse(anml) match {
      case ParseSuccess(mod) =>
        Right(FullToCore.trans(mod))
      case _ =>
        Left("Could not parse anml string")
    }
  }

  for(anml <- InputAnmlModels.valid) {
    test("translation to core: " + anml) {
      Parser.parse(anml) match {
        case ParseSuccess(module) =>
//          println(FullToCore.trans(module).mkString("\n"))
        case err =>
          fail(s"Could not parse anml string: $err")
      }
    }
  }

  for(anml <- InputAnmlModels.valid) {
    test("Invariant on core: " + anml) {
      Parser.parse(anml) match {
        case ParseSuccess(fullMod) =>
          val m = FullToCore.trans(fullMod)
          checkInvariantsInCore(m)
        case err =>
          fail(s"Could not parse anml string: $err")
      }
    }
  }

  def checkInvariantsInCore(m: CoreModel): Unit = {
    val declarations = m.collect { case x: Declaration[_] => x }
    Predef.assert(declarations.distinct.size == declarations.size,
                  "\nDuplicate declarations in: \n" + declarations.mkString("\n"))
  }
}
