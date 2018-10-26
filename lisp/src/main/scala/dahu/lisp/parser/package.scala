package dahu.lisp

package object parser {

  /** An S-Expression */
  type E = AnyRef

  def parse(str: String): E = Parser.parser.parse(str)
}
