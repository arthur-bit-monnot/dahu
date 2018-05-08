package dahu.planning.pddl

import fr.uga.pddl4j.parser._
import dahu.planning.model.common._
import dahu.planning.model.{common, full}
import dahu.planning.model.full._
import dahu.utils.errors._
import dahu.planning.pddl.Utils._
import dahu.planning.pddl.Resolver._

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.language.implicitConversions

abstract class Factory {

  def context: Ctx

  implicit val resolver: Resolver = new Resolver {
    def scope: Scope = context.scope

    override def id(name: String): Id = Id(common.RootScope, name)
    override def typeOf(name: String): Type =
      context.findType(name).getOrElse(unexpected(s"unknown type: $name"))

    override def variable(name: String): StaticExpr = context.findDeclaration(name) match {
      case Some(v: VarDeclaration[_]) => CommonTerm(v.variable)
      case _                          => unexpected(s"unknown variable: $name")
    }

    override def nextId(): String = dahu.planning.model.reservedPrefix + next().toString
  }

  def getTranslator(name: String): FunctionCompat

  def id(name: String): Id = Id(resolver.scope, name)

  protected def asFluent(name: String, args: Seq[String]): Fluent =
    Fluent(getTranslator(name).model, args.map(resolver.variable))

  def hasType(name: String): Boolean = context.findType(name).nonEmpty

}

class ModelFactory(predef: PddlPredef.type) extends Factory {
  private var model = predef.baseModel

  override def context: Model = model

  private val translators = mutable.HashMap[String, FunctionCompat]()
  override def getTranslator(name: String): FunctionCompat = translators(name)

  def rec(block: full.InModuleBlock): Unit = model = (model + block).get

  private def recordFunction(pddlPred: NamedTypedList): Unit = {
    val t = FunctionCompat(pddlPred)
    translators += ((t.name, t))
    rec(FunctionDeclaration(t.model))
  }

  private def recordType(tpe: ast.Tpe): Unit = {
    val ast.Tpe(name, parent) = tpe
    assert(!hasType(name), s"type already recorded: $name")
    assert(parent.forall(hasType), s"parent not recorded: $parent")
    val pt = parent match {
      case None => None
      case Some(other) =>
        model.findType(other) match {
          case Some(ot: Type.ObjType) => Some(ot)
          case None                   => unexpected("parent not recorded")
          case Some(_)                => ???
        }
    }
    (name, pt) match {
      case ("object", None)      => rec(TypeDeclaration(Type.ObjSubType(id("object"), Type.ObjectTop)))
      case (other, Some(father)) => rec(TypeDeclaration(Type.ObjSubType(id(other), father)))
      case _                     => unexpected
    }
  }

  private def recordInstance(name: String, tpe: String): Unit = {
    rec(InstanceDeclaration(Instance(id(name), typeOf(tpe))))
  }

  private def recordInitialState(e: Exp): Unit = {
    val assertion = e match {
      case ast.AssertionOnFunction(funcName) =>
        getTranslator(funcName).effect(e)
    }
    rec(TemporallyQualifiedAssertion(Equals(Interval(predef.Start, predef.Start)), assertion))
  }

  private def recordGoal(e: Exp): Unit = e match {
    case ast.And(goals) =>
      goals.foreach(recordGoal)
    case ast.AssertionOnFunction(name) =>
      val assertion = getTranslator(name).condition(e)
      rec(
        TemporallyQualifiedAssertion(
          Equals(Interval(predef.End, predef.End)),
          assertion
        ))
  }

  def loadDomain(dom: Domain): Unit = {
    val types = dom.getTypes.asScala.map {
      case ast.ReadTpe(tpe: ast.Tpe) => tpe
    }
    val queue = mutable.Queue(types: _*)
    while(queue.nonEmpty) {
      queue.dequeue() match {
        case x @ ast.Tpe(name, parentOpt) if !hasType(name) =>
          parentOpt match {
            case None                             => recordType(x)
            case Some(parent) if !hasType(parent) => queue.enqueue(x)
            case _                                => recordType(x)
          }
        case _ =>
      }
    }
    dom.getPredicates.asScala.foreach(recordFunction)
    dom.getFunctions.asScala.foreach(recordFunction)

    // make sure we fail if part of the domain is not supported
    Option(dom.getConstraints).foreach(_ => ???)
    dom.getConstants.asScala.foreach {
      case ast.TypedSymbol(name, tpe) => recordInstance(name, tpe)
    }

    dom.getDerivesPredicates.asScala.foreach(_ => ???)
  }

  def loadProblem(pb: Problem): Unit = {
    pb.getObjects.asScala.foreach {
      case ast.TypedSymbol(name, tpe) => recordInstance(name, tpe)
    }
    pb.getInit.asScala.foreach(recordInitialState)
    recordGoal(pb.getGoal)
  }

  def result: Model = context
}
