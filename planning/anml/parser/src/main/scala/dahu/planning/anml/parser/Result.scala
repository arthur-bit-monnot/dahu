package dahu.planning.anml.parser

import java.io.File

import ParserApi.baseApi._

sealed trait ParseResult[+T] {
  def map[B](f: T => B): ParseResult[B]
  def flatMap[B](f: T => ParseResult[B]): ParseResult[B]
}

case class ParseSuccess[T](result: T) extends ParseResult[T] {
  override def map[B](f: (T) => B): ParseSuccess[B] = ParseSuccess(f(result))
  override def flatMap[B](f: T => ParseResult[B]): ParseResult[B] = f(result)
}

sealed trait ParseFailure extends ParseResult[Nothing] {
  override def map[B](f: Nothing => B): ParseResult[B] = this
  def format: String
  override def flatMap[B](f: Nothing => ParseResult[B]): ParseResult[B] = this
}

final case class FileAccessError(file: File, throwable: Throwable) extends ParseFailure {
  override def format: String =
    s"Error while trying to read: $file:\n" + throwable.getLocalizedMessage
}

final case class ParserFailure(faultyLine: String,
                               lineIndex: Int,
                               columnIndex: Int,
                               lastParser: Parser[Any],
                               file: Option[File])
    extends ParseFailure {

  override def format: String = {
    s"Could not parse anml string at (${lineIndex + 1}:${columnIndex + 1}) ${file.map("[" + _.toString + "]").getOrElse("")}:\n" +
      faultyLine + "\n" +
      " " * columnIndex + "^\n" +
      s"Expected: $lastParser"
  }
}

final case class ParserCrash(throwable: Throwable, file: Option[File])
    extends ParseResult[Nothing] {
  def format: String =
    s"Error while processing ANML input${file.map(" [" + _ + "]").getOrElse("")}:\n" + throwable.getLocalizedMessage

  override def map[B](f: Nothing => B): ParseResult[B] = this
  override def flatMap[B](f: Nothing => ParseResult[B]): ParseResult[B] = this
}
