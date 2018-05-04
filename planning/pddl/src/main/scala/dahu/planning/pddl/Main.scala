package dahu.planning.pddl

import fr.uga.pddl4j.parser._
import dahu.planning.model.common._
import dahu.planning.model.{common, full}
import dahu.planning.model.full._
import dahu.utils.errors._

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.language.implicitConversions

object Main extends App {

  //  val domFile = "/home/arthur/work/fape/planning/domains/blocks_ipc2/pddl/blocks_ipc2.dom.pddl"
  //  val pbFile = "/home/arthur/work/fape/planning/domains/blocks_ipc2/pddl/blocks_ipc2.p04-0.pb.pddl"
  val domFile =
    "/home/arthur/work/ext/rcll/temporal_1_robot/rcll_domain_production_durations_nors.pddl"
  val pbFile = "/home/arthur/work/ext/rcll/temporal_1_robot/problem-001-r1-o1-durations.pddl"
  val parser = new Parser()
  parser.parse(domFile, pbFile)

  val dom = parser.getDomain
  val pb = parser.getProblem

  private var counter = 0
  def next(): Int = { counter += 1; counter }

  println(dom)
//  println(pb)

  object PddlPredef extends Predef {
    import common.Type._
    private val scope = RootScope + "_predef_"
    override val Time: IRealType = IntSubType(scope / "time", Integers)

    override val Boolean: BooleanType = BooleanType(scope / "boolean")

    override val True: Instance = Instance(scope / "true", Boolean)
    override val False: Instance = Instance(scope / "false", Boolean)

    override val Start = LocalVar(scope / "start", Time)
    override val End = LocalVar(scope / "end", Time)

    override def baseModel: full.Model =
      (Model() ++ Seq(
        TypeDeclaration(ObjectTop),
        TypeDeclaration(Boolean),
        TypeDeclaration(Reals),
        TypeDeclaration(Integers),
        TypeDeclaration(Time),
        InstanceDeclaration(True),
        InstanceDeclaration(False),
        LocalVarDeclaration(Start),
        LocalVarDeclaration(End),
      )).getOrElse(sys.error("Could not instantiate base model"))
  }
  implicit val predef = PddlPredef
  implicit def term2FullModel(v: Term): CommonTerm = CommonTerm(v)

  object Factory {
    var model = PddlPredef.baseModel

    implicit val ctx = new Ctx {
      override def id(name: String): Id = Id(common.RootScope, name)
      override def typeOf(name: String): Type =
        model.findType(name).getOrElse(unexpected(s"unknown type: $name"))

      override def variable(name: String): StaticExpr = model.findDeclaration(name) match {
        case Some(v: VarDeclaration[_]) => CommonTerm(v.variable)
        case _                          => unexpected(s"unknown variable: $name")
      }
    }

    val translators = mutable.HashMap[String, PredicateTranslator]()
    def getTranslator(name: String): PredicateTranslator = translators(name)
    def recordFunction(pddlPred: NamedTypedList): Unit = {
      val t = new DefaultPredicate(pddlPred)
      translators += ((t.name, t))
      rec(FunctionDeclaration(t.model))
    }

    def rec(block: full.InModuleBlock): Unit = model = (model + block).get

    def id(name: String): Id = Id(common.RootScope, name)

    def recordType(tpe: Tpe): Unit = {
      val Tpe(name, parent) = tpe
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
      }
    }

    def recordInstance(name: String, tpe: String): Unit = {
      rec(InstanceDeclaration(Instance(id(name), typeOf(tpe))))
    }

    def recordInitialState(e: Exp): Unit = {
      val ReadFluent(name, args) = e
      val assertion = TemporallyQualifiedAssertion(
        Equals(Interval(predef.Start, predef.Start)),
        TimedAssignmentAssertion(
          Fluent(getTranslator(name).model, args.map(ctx.variable)),
          predef.True,
          None,
          dahu.planning.model.reservedPrefix + next()
        )
      )
      rec(assertion)
    }

    def recordGoal(e: Exp): Unit = e match {
      case ReadAnd(goals) =>
        goals.foreach {
          case ReadFluent(name, args) =>
            val assertion = TemporallyQualifiedAssertion(
              Equals(Interval(predef.End, predef.End)),
              TimedEqualAssertion(
                Fluent(getTranslator(name).model, args.map(ctx.variable)),
                predef.True,
                None,
                dahu.planning.model.reservedPrefix + next()
              )
            )
            rec(assertion)
        }
    }

    def hasType(name: String): Boolean = model.findType(name).nonEmpty
  }
  object ReadFluent {
    def unapply(exp: Exp): Option[(String, List[String])] = {
      if(exp.getConnective == Connective.ATOM) {
        exp.getAtom.asScala.toList.map(_.getImage) match {
          case head :: tail => Some((head, tail))
          case _            => None
        }
      } else {
        None
      }
    }
  }
  case class Tpe(name: String, parent: Option[String])
  case class ReadTypedSymbol(name: String, tpe: String)
  object ReadTypedSymbol {
    def unapply(e: TypedSymbol): Option[(String, String)] = {
      if(e.getKind == Symbol.Kind.VARIABLE || e.getKind == Symbol.Kind.CONSTANT) {
        val name =
          if(e.getImage.startsWith("?"))
            e.getImage.drop(1)
          else
            e.getImage
        e.getTypes.asScala.toList match {
          case tpe :: Nil => Some((name, tpe.getImage))
          case _          => None
        }
      } else {
        None
      }

    }
  }
  object Read {
    def unapply(e: TypedSymbol): Option[Tpe] =
      if(e.getKind.name() == "TYPE") {
        e.getTypes.asScala.map(_.getImage).toList match {
          case Nil           => Some(Tpe(e.getImage, None))
          case parent :: Nil => Some(Tpe(e.getImage, Some(parent)))
          case _             => None
        }
      } else
        None
  }
  object ReadAnd {
    def unapply(e: Exp): Option[List[Exp]] = {
      if(e.getConnective == Connective.AND) {
        Some(e.getChildren.asScala.toList)
      } else {
        None
      }
    }
  }
  val types = dom.getTypes.asScala.map {
    case Read(tpe: Tpe) => tpe
  }
  val queue = mutable.Queue(types: _*)
  while(queue.nonEmpty) {
    queue.dequeue() match {
      case x @ Tpe(name, parentOpt) if !Factory.hasType(name) =>
        parentOpt match {
          case None                                     => Factory.recordType(x)
          case Some(parent) if !Factory.hasType(parent) => queue.enqueue(x)
          case _                                        => Factory.recordType(x)
        }
      case _ =>
    }
  }
  dom.getPredicates.asScala.foreach(Factory.recordFunction)
  dom.getFunctions.asScala.foreach(Factory.recordFunction)

  // make sure we fail if part of the domain is not supported
  Option(dom.getConstraints).foreach(_ => ???)
  dom.getConstants.asScala.foreach {
    case ReadTypedSymbol(name, tpe) => Factory.recordInstance(name, tpe)
  }

  dom.getDerivesPredicates.asScala.foreach(_ => ???)

  pb.getObjects.asScala.foreach {
    case ReadTypedSymbol(name, tpe) => Factory.recordInstance(name, tpe)
  }
  pb.getInit.asScala.foreach(Factory.recordInitialState)
  Factory.recordGoal(pb.getGoal)

  println(Factory.model)

  trait Ctx {
    def typeOf(name: String): Type
    def id(name: String): Id
    def variable(name: String): StaticExpr
  }
  def typeOf(name: String)(implicit ctx: Ctx): Type = ctx.typeOf(name)
  def id(name: String)(implicit ctx: Ctx): Id = ctx.id(name)

  abstract class PredicateTranslator() {
    def name: String
    def model: FluentTemplate
  }
  class DefaultPredicate(pddf: NamedTypedList)(implicit ctx: Ctx) extends PredicateTranslator {
    override val name: String = pddf.getName.getImage
    override val model =
      FluentTemplate(id(name), predef.Boolean, pddf.getArguments.asScala.map {
        case ReadTypedSymbol(name, tpe) => common.Arg(id(name), typeOf(tpe))
      })
  }

}
