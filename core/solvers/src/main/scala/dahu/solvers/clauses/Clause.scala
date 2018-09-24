package dahu.solvers.clauses

final case class Clause[Var](direct: Set[Var], negated: Set[Var])
