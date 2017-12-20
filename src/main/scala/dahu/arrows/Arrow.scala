package dahu.arrows

trait Arrow[-Domain, +CoDomain] {

  def apply(v: Domain): CoDomain

  def andThen[Res](next: Arrow[CoDomain, Res]): Arrow[Domain, Res] =
    ComposedArrow(this, next)
}

object Arrow {
  def lift[A, B](f: A => B): Arrow[A, B] = FunctionArrow(f)
}

final case class FunctionArrow[In, Res](f: In => Res) extends Arrow[In, Res] {
  override def apply(v1: In): Res = f(v1)
}

final case class ComposedArrow[A, B, C](ab: Arrow[A, B], bc: Arrow[B, C]) extends Arrow[A, C] {
  override def apply(v1: A): C = bc(ab(v1))
}
