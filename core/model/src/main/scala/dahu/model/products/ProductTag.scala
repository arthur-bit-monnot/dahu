package dahu.model.products
import cats.Id
import dahu.model.input.Expr
import dahu.model.types.Tag.Type
import dahu.model.types.{Tag, TagAny, Value}
import dahu.utils.{ClassTag, Vec}

trait ProductTag[P[_[_]]] extends Tag[P[cats.Id]] {

  val fields: Vec[Field]

  def fromValues(fields: Vec[Any]): P[cats.Id] //= construct[F](fields.toSeq)
  def buildFromValues(fields: Vec[Value]): P[cats.Id] =
    fromValues(fields.asInstanceOf[Vec[Any]])

  def getFields(prod: P[Expr]): Vec[Expr[Any]]

  def getFieldsIdentity(prod: P[Id]): Vec[Any]
  def getField[A](prod: P[Id], position: Int): A =
    getFieldsIdentity(prod)(position).asInstanceOf[A]

  def getAccessor[A: Tag](name: String): FieldAccess[P, A] = {
    require(fields.map(_.position).toSeq == (0 until fields.size))
    require(fields.map(_.name).distinct.size == fields.size, "Two fields have the same name")

    fields.toSeq.collectFirst {
      case Field(fieldName, tag, position) if name == fieldName =>
        require(tag == Tag[A])
        new FieldAccess[P, A]()(this, Tag[A]) {
          override def name: String = fieldName
          override def fieldPosition: Int = position
        }

    } match {
      case Some(accessor) => accessor
      case None           => dahu.utils.errors.unexpected(s"Problem in extracting field: $name")
    }
  }

}

object ProductTag {
  def build[P[_[_]]](_fields: (String, TagAny)*)(
      implicit ct: ClassTag[P[Id]],
      gcid: GenConstructor[P, cats.Id],
      gcexpr: GenConstructor[P, Expr]
  ): ProductTag[P] = new ProductTag[P] {
    override val fields: Vec[Field] = Vec.fromSeq(_fields.zipWithIndex.map {
      case ((name, tag), index) => Field(name, tag, index)
    })

    override def clazz: ClassTag[P[Id]] = ct
    override def typ: Type = ???
    override def fromValues(fields: Vec[Any]): P[cats.Id] = gcid.construct(fields.toSeq)
    override def getFields(prod: P[Expr]): Vec[Expr[Any]] = Vec.fromSeq(gcexpr.deconstruct(prod))
    override def getFieldsIdentity(prod: P[cats.Id]): Vec[Any] = Vec.fromSeq(gcid.deconstruct(prod))
  }
}
