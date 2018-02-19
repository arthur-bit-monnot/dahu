package dahu.recursion

object Types {

  abstract class IndexLabelImpl {
    type T
    def apply(s: Int): T = fromInt(s)

    def fromInt(s: Int): T
    def toInt(lbl: T): Int
    def fromIntF[F[_]](fs: F[Int]): F[T]
    def toIntF[F[_]](fs: F[T]): F[Int]
  }

  abstract class SubIndexLabelImpl[UB] extends IndexLabelImpl {
    def wrap(s: UB): T
    def unwrap(lbl: T): UB
    def subst[F[_]](fs: F[UB]): F[T]
    def unsubst[F[_]](fs: F[T]): F[UB]
  }

  val ExprId: IndexLabelImpl = new IndexLabelImpl {
    type T = Int
    override def fromInt(s: Int): T               = s
    override def toInt(lbl: T): Int               = lbl
    override def fromIntF[F[_]](fs: F[Int]): F[T] = fs
    override def toIntF[F[_]](fs: F[T]): F[Int]   = fs
  }
  type ExprId = ExprId.T

  /** Implicit conversion to Int, mainly to facilitate usage as index. */
  implicit def exprIdAsInt(i: ExprId): Int      = ExprId.toInt(i)
  implicit val orderingExprId: Ordering[ExprId] = ExprId.fromIntF(Ordering[Int])
  implicit class ExprIdOps(val i: ExprId) extends AnyVal {
    def value: Int = ExprId.toInt(i)
  }

  private val intClassTag: ClassTag[Int] = implicitly[ClassTag[Int]]

  implicit val classTagExpr: ClassTag[ExprId] = ExprId.fromIntF(implicitly[ClassTag[Int]])


}

/**
  * Abstract Syntax Directed Acyclic Graph.
  * Provides:
  *
  * Two opaque type alias of Int:
  *  - EId: represents the set of identifiers for all expressions
  *  - VarId: a subset of EId that represents decision variables
  *
  * A partial function from T => EId
  * A coalgebra EId => ExprF[EId]
  */
trait ASDAG[T] {
  import Types._

  /** Opaque type representing the IDs of the expression in this table. */
  type VariableId = ExprId

  type Expr     = ExprF[ExprId]
  type Variable = InputF[ExprId]

  def root: ExprId
  def coalgebra: ExprId ==> Expr // equivalent to Coalgebra[ExprF, EId]

  def compiledForm: T ==> Option[ExprId]

  def variableCoalgebra: VariableId ==> Variable =
    id => coalgebra(id).asInstanceOf[Variable] // TODO: unsafe as VarID is just an alias for ExprId

  def ids: OpaqueIntSubset[ExprId]
}
