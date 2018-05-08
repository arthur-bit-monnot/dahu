package dahu.planning.model.transforms

import dahu.planning.anml.parser
import dahu.planning.anml.parser.{AnmlPredef, ParseSuccess}
import dahu.planning.model.common.Predef
import org.scalatest.FunSuite

class ActionInstantiationTest extends FunSuite {

  implicit val predef: Predef = AnmlPredef

  import dahu.planning.model.core._

  test("action instantiation") {
    parser.parse("""
      |fluent boolean sv;
      |action Template(boolean x, boolean y) {
      |  duration == 10;
      |  [all] sv == true;
      |  [all] contains id: sv == x :-> y;
      |};
    """.stripMargin) match {
      case ParseSuccess(model) =>
        model.collectFirst { case x: ActionTemplate => x } match {
          case Some(act) =>
            val name = "myInstance"
            val instance = ActionInstantiation.instance(act, name)
            assert(instance.name == name)

            // check that the name template has disappeared
//            val stringWithAPattern = pattern { case x: String if x.contains("Template") => Seq(x) }
//            assert(landscaper.extract(stringWithAPattern, instance.content).isEmpty) // TODO

            assert(instance.args.size == act.args.size)
            assert(instance.args.map(_.typ) == act.args.map(_.typ))
            assert(instance.template == act)

            println(instance.content.mkString("\n"))
          case None =>
            fail()
        }
      case _ =>
        fail()
    }
  }

}
