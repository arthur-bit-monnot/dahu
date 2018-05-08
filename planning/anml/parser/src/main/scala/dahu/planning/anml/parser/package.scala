package dahu.planning.anml

import java.io.File

import dahu.planning.model.core.CoreModel
import dahu.planning.model.transforms.FullToCore
import dahu.planning.model.{core, full}
import dahu.planning.parsing.anml

package object parser {

  implicit val predef = AnmlPredef

  def parse(anml: String): ParseResult[core.CoreModel] =
    parseToFull(anml).map(FullToCore.trans(_))

  def parse(anml: File): ParseResult[CoreModel] =
    parseToFull(anml).map(FullToCore.trans(_))

  def parseToFull(anmlString: String): ParseResult[full.Model] = {
    Parser.parse(anmlString)
  }

  def parseToFull(anmlFile: File): ParseResult[full.Model] = {
    Parser.parse(anmlFile)
  }

}
