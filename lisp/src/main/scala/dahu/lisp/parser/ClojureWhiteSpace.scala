package dahu.lisp.parser

object ClojureWhiteSpace {

  val baseApi = fastparse.noApi
  val whiteApi = fastparse.WhitespaceApi.Wrapper {
    import fastparse.all._
    val lineComment = ";" ~ CharsWhile(c => c != '\n', min = 0) ~ ("\n" | PassWith("") ~ &(End))
    val white = CharIn(Seq(' ', '\r', '\n', '\t')) | lineComment
      .map(_ => {})
      .opaque("white-space")
    NoTrace(white.rep.opaque("white-spaces"))
  }

}
