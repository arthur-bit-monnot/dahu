package dahu.model.problem.partial

sealed trait NodeTag

case object Constant extends NodeTag

case object Input extends NodeTag

/** The node is considered as an input in a a partial AST. */
case object BoundaryInput extends NodeTag

case object Expression extends NodeTag

/** Denotes a node that is not present is a particular AST. */
case object Absent extends NodeTag
