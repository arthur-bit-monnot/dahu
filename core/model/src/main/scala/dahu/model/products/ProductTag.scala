package dahu.model.products
import cats.Id
import dahu.model.functions.Fun1
import dahu.model.input.Expr
import dahu.model.types.Tag.Type
import dahu.model.types.{LambdaTag, LambdaTagAny, Tag, TagAny, Value}
import dahu.utils.{ClassTag, Vec}

trait ProductTagAny extends TagAny { self =>
  def name: String
  def fields: Vec[Field]
  def getField(i: Int, product: Any): Any
  def fromValues(fields: Vec[Any]): Any
  def buildFromValues(fields: Vec[Value]): Any

  def fieldPosition(name: String): Option[Int] =
    fields.toSeq.find(_.name == name).map(_.position)

  def getAccessorAny(name: String): Option[FieldAccessAny] = {
    require(fields.map(_.position).toSeq == (0 until fields.size))
    require(fields.map(_.name).distinct.size == fields.size, "Two fields have the same name")

    fields.toSeq.find(_.name == name) match {
      case Some(Field(fieldName, tag, position)) =>
        val accessor = new FieldAccessAny() {
          override def prodTag: ProductTagAny = self
          override def fieldTag: TagAny = tag
          override def fieldPosition: Int = position
          override def name: String = fieldName
          override def arity: Option[Int] = Some(1)
          override def compute(args: Vec[Value]): Any = {
            require(args.length == 1)
            self.getField(position, args(0))
          }
          override def outType: TagAny = fieldTag
          override def funType: LambdaTagAny = LambdaTag.of(prodTag, fieldTag)
        }
        Some(accessor)
      case None => dahu.utils.errors.unexpected(s"Problem in extracting field: $name")
    }
  }
}

trait ProductTag[P[_[_]]] extends Tag[P[cats.Id]] with ProductTagAny {

  val fields: Vec[Field]

  def fromValues(fields: Vec[Any]): P[cats.Id]
  def buildFromValues(fields: Vec[Value]): P[cats.Id] =
    fromValues(fields.asInstanceOf[Vec[Any]])

  def getFields(prod: P[Expr]): Vec[Expr[Any]]

  def getFieldsIdentity(prod: P[Id]): Vec[Any]
  override def getField(i: Int, product: Any): Any = getField(product.asInstanceOf[P[Id]], i)
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
  def build[P[_[_]]](_name: String, _fields: (String, TagAny)*)(
      implicit ct: ClassTag[P[Id]],
      gcid: GenConstructor[P, cats.Id],
      gcexpr: GenConstructor[P, Expr],
      tt: scala.reflect.runtime.universe.WeakTypeTag[P[cats.Id]]
  ): ProductTag[P] = new ProductTag[P] {
    override def name: String = _name
    override val fields: Vec[Field] = Vec.fromSeq(_fields.zipWithIndex.map {
      case ((name, tag), index) => Field(name, tag, index)
    })
    override def clazz: ClassTag[P[Id]] = ct
    override val typ: Type = tt.tpe
    override def fromValues(fields: Vec[Any]): P[cats.Id] = gcid.construct(fields.toSeq)
    override def getFields(prod: P[Expr]): Vec[Expr[Any]] = Vec.fromSeq(gcexpr.deconstruct(prod))
    override def getFieldsIdentity(prod: P[cats.Id]): Vec[Any] = Vec.fromSeq(gcid.deconstruct(prod))
    override def toString: String = name
  }
}
