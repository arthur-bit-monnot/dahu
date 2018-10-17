package dahu.refinement
import cats.Id
import dahu.model.types.Bool
import dahu.utils.Vec

object Test {

  case class TimeStampedF[F[_], S[_[_]]](time: F[Float], s: F[S[F]])
  type TimeStamped[S[_[_]]] = TimeStampedF[Id, S]

  case class DStateF[F[_]](moving: F[Bool])
  type DState = DStateF[Id]
  case class CStateF[F[_]](x: F[Float], y: F[Float])
  type CState = CStateF[Id]
  type TCState = TimeStamped[CStateF]

  case class BandF[F[_]](ds: F[DState], cs: F[Vec[TCState]])
  type Band = BandF[Id]

}
