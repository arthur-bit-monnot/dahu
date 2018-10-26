package dahu.lisp

import java.lang.invoke._
import java.lang.reflect.Modifier

import scala.reflect.ClassTag

package object mh {

  def error(str: String): Nothing = throw new RuntimeException(str)

  def mhRef(tpe: Class[_], name: String): MethodHandle =
    MethodHandles.publicLookup.unreflect(
      tpe.getMethods.filter(_.getName == name) match {
        case Array(a) => a
        case Array()  => error(s"No method named $name in $tpe")
        case x =>
          x.foreach(m => {
            println("  Method: " + m)
            println("Synthetic: " + m.isSynthetic)
            println("Default: " + m.isDefault)
            println("Types: " + m.getGenericParameterTypes.mkString(" -- "))
            println("Modifiers: " + Modifier.isPublic(m.getModifiers))
          })
          // more than one method, we should look at the types to see if some are incompatible
          error(s"More than one method named $name in $tpe: \n${x.mkString("\n")}")
      }
    )

  def typeOfFun1[A: ClassTag, R: ClassTag]: MethodType =
    MethodType.methodType(clazz[R], clazz[A])

  def typeOfFun2[A: ClassTag, B: ClassTag, R: ClassTag]: MethodType =
    MethodType.methodType(clazz[R], clazz[A], clazz[B])

  def typeOfFun3[A: ClassTag, B: ClassTag, C: ClassTag, R: ClassTag]: MethodType =
    MethodType.methodType(clazz[R], clazz[A], clazz[B], clazz[C])

  def mh[A: ClassTag, B: ClassTag](fun: A => B): MethodHandle =
    mhRef(classOf[A => B], "apply")
      .bindTo(fun)
      .asType(typeOfFun1[A, B])
  def mhVarArgs[A, B](fun: Array[A] => B): MethodHandle =
    mhRef(classOf[Array[A] => B], "apply")
      .bindTo(fun)
      .asType(MethodType.methodType(classOf[AnyRef], classOf[Array[AnyRef]]))
      .asVarargsCollector(classOf[Array[AnyRef]])
  def mh[A: ClassTag, B: ClassTag, C: ClassTag](fun: (A, B) => C): MethodHandle = {
    mhRef(classOf[(A, B) => C], "apply")
      .bindTo(fun)
      .asType(typeOfFun2[A, B, C])
  }
  def mh[A: ClassTag, B: ClassTag, C: ClassTag, D: ClassTag](fun: (A, B, C) => D): MethodHandle =
    mhRef(classOf[(A, B, C) => D], "apply")
      .bindTo(fun)
      .asType(typeOfFun3[A, B, C, D])

  def clazz[A: ClassTag]: Class[A] = implicitly[ClassTag[A]].runtimeClass.asInstanceOf[Class[A]]

}
