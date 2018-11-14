package dahu.graphs

import cats._
import cats.implicits._
import dahu.graphs.autotrans.AutoTransformation
import dahu.utils._

final class ExtensibleASG[K,
                          F[_]: SFunctor: TreeNode: ClassTagK,
                          Opt[_]: Functor,
                          I <: IDTop,
                          OI <: Int](
    ogetKey: K => Opt[OI],
    ocoalg: OI => F[OI],
) extends OpenASG[K, F, Opt, I] {

  override def fixID: ExtensibleASG[K, F, Opt, SubSubInt[IDTop, Marker], OI] =
    castIDTo[SubSubInt[IDTop, Marker]]
  override def castIDTo[NewInternalID <: IDTop]: ExtensibleASG[K, F, Opt, NewInternalID, OI] =
    this.asInstanceOf[ExtensibleASG[K, F, Opt, NewInternalID, OI]]

  private val x = new AutoTransformation[F](autotrans.Transformation.none)
    .asInstanceOf[AutoTransformation.Aux[F, I]]
  private val oi2i = debox.Map[OI, I]()

  private def recordOld(oi: OI): I = {
    if(!oi2i.contains(oi)) {
      val fi = ocoalg(oi).smap(recordOld)
      val i = x.deepRecord(fi)
      oi2i.update(oi, i)
    }
    oi2i(oi)
  }

  override def getTreeRoot(k: K): Opt[I] = {
    val optoi = ogetKey(k)
    optoi.map(recordOld)
  }
  override def internalCoalgebra(i: I): F[I] = x.extract(i)

  def record(fi: F[I]): I = x.deepRecord(fi)

  def record[J <: Int](j: J, coalg: J => F[J]): I = {
    record(
      coalg(j).smap(j2 => record(j2, coalg))
    )
  }
}
