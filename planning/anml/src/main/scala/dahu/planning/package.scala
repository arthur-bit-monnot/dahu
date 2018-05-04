package dahu

import java.io.File

import dahu.planning.model.common.Predef
import dahu.planning.model.core.CoreModel
import dahu.planning.model.transforms.FullToCore
import dahu.planning.model.{core, full}
import dahu.planning.parsing.anml
import dahu.planning.parsing.anml.AnmlPredef

package object planning {

  implicit val predef = AnmlPredef

  sealed trait Result[+T] {
    def map[B](f: T => B): Result[B]
  }
  sealed trait Failure extends Result[Nothing] {
    override def map[B](f: Nothing => B): Result[B] = this
  }

  case class ParseError(failure: anml.GenFailure) extends Failure

  case class Crash(msg: String, throwable: Option[Throwable]) extends Failure

  case class Success[T](result: T) extends Result[T] {
    override def map[B](f: (T) => B): Success[B] = Success(f(result))
  }

  def parse(anml: String): Result[core.CoreModel] =
    parseToFull(anml).map(FullToCore.trans(_))

  def parse(anml: File): Result[CoreModel] =
    parseToFull(anml).map(FullToCore.trans(_))

  def parseToFull(anmlString: String): Result[full.Model] = {
    anml.Parser.parse(anmlString) match {
      case anml.ParseSuccess(model) =>
        Success(model)
      case x: anml.GenFailure =>
        ParseError(x)
    }
  }

  def parseToFull(anmlFile: File): Result[full.Model] = {
    anml.Parser.parse(anmlFile) match {
      case anml.ParseSuccess(model) =>
        Success(model)
      case x: anml.GenFailure =>
        ParseError(x)
    }
  }

}
