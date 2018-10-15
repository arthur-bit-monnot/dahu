package dahu.model.products

trait GenConstructor[P[_[_]], F[_]] {

  def construct(values: Seq[F[Any]]): P[F]
  def deconstruct(prod: P[F]): Seq[F[Any]]
}

object GenConstructor {

  def apply[P[_[_]], F[_]](implicit instance: GenConstructor[P, F]): GenConstructor[P, F] = instance

  import shapeless._

  trait HListExtract[H <: HList, F[_]] {
    def terms(h: H): List[F[Any]]
    def fromTerms(l: Seq[F[Any]]): H
  }

  implicit def genPE[P[_[_]], F[_], H <: HList](
      implicit gen: Generic.Aux[P[F], H],
      hListExtract: HListExtract[H, F]): GenConstructor[P, F] = {
    new GenConstructor[P, F] {
      override def construct(values: Seq[F[Any]]): P[F] = {
        val hlist: H = hListExtract.fromTerms(values)
        gen.from(hlist)
      }
      override def deconstruct(prod: P[F]): Seq[F[Any]] = {
        val hlist: H = gen.to(prod)
        hListExtract.terms(hlist)
      }
    }
  }
  implicit def peOfHNil[F[_]]: HListExtract[HNil, F] = new HListExtract[HNil, F] {
    override def terms(h: HNil): List[F[Any]] = Nil
    override def fromTerms(l: Seq[F[Any]]): HNil = {
      require(l.isEmpty)
      HNil
    }
  }
  implicit def peOfHlist[H, T <: HList, F[_]](
      implicit t: HListExtract[T, F]): HListExtract[F[H] :: T, F] =
    new HListExtract[F[H] :: T, F] {
      override def terms(l: F[H] :: T): List[F[Any]] =
        l.head.asInstanceOf[F[Any]] :: t.terms(l.tail)
      override def fromTerms(l: Seq[F[Any]]): F[H] :: T =
        l.head.asInstanceOf[F[H]] :: t.fromTerms(l.tail)
    }

}
