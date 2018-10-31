package dahu.lisp
import java.io.File
import java.nio.file.{Files, Path}

import fastparse.core.Parsed

import scala.io.Source

package object parser {

  /** An S-Expression */
  type E = AnyRef

  def parse(str: String) = Parser.parser.parse(str)

  def parseMany(str: String) = Parser.parserMany.parse(str)

  def parseFile(file: File) = {
    val source = Source.fromFile(file).getLines().fold("")((a, b) => a + "\n" + b)
    parseMany(source)
  }
}
