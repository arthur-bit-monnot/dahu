package dahu.planning.anml.parser

import org.scalatest.FunSuite

class AnmlParsingTest extends FunSuite {

  for(anml <- InputAnmlModels.valid) {
    test("valid: " + anml) {
      Parser.parse(anml) match {
        case ParseSuccess(module) =>
//          println("PARSED:\n" + anml + "\n")
//          println("AS:\n" + module + "\n\n")
        case x: ParserFailure =>
          fail(s"Could not parse anml string: $anml\n\n${x.format}")
        case ParserCrash(err, _) =>
          err.printStackTrace()
          fail(s"Exception raised while parsing")
        case x: FileAccessError => ???
      }
    }
  }

  for(anml <- InputAnmlModels.invalid) {
    test("invalid: " + anml) {
      Parser.parse(anml) match {
        case ParseSuccess(module) =>
          fail(s"Following anml string should be invalid:\n$anml\n\nParsed to:\n $module")
        case x: ParserFailure =>
//          println(s"Detected invalid ANML:\n${x.format}")
        case x => fail(s"Unexpected output: $x")
      }
    }
  }

  val tmp = "constant integer i; i == 2 implies (-i + 16) * 2 == -4 implies true;"

  test("debug: temporary") {

    /** Dummy text to facilitate testing. */
//    println(tmp)
    Parser.parse(tmp) match {
      case ParseSuccess(module) =>
//        println("PARSED:\n")
//        println("AS:\n" + module + "\n\n")
      case x: ParserFailure =>
        fail(s"Could not parse anml string:\n${x.format}")
      case ParserCrash(err, _) =>
        err.printStackTrace()
        fail(s"Exception raised while parsing")
      case _ => ???
    }
  }
}
