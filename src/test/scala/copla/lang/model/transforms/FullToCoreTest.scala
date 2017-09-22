package copla.lang.model.transforms

import copla.lang.parsing.anml.{InputAnmlModels, ParseFailure, ParseSuccess, Parser}
import org.scalatest.FunSuite

class FullToCoreTest extends FunSuite {

  for (anml <- InputAnmlModels.valid) {
    test("translation to core: " + anml) {
      Parser.parse(anml) match {
        case ParseSuccess(module) =>
          println(FullToCore.trans(module).mkString("\n"))
        case x: ParseFailure =>
          // ignore, this should be caught by the ANML parsing tests
      }
    }
  }
}
