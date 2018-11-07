package dahu.refinement.interop
import dahu.graphs.transformations.Transformation
import dahu.model.input.TypedIdent
import dahu.model.ir._
import dahu.model.math._
import dahu.model.types._
import dahu.model.products.{Field, ProductTagAny}
import dahu.utils._

object transformations {

  private val epsilon = 1e-8

  val scalarize = new Transformation[ExprF, ExprF] {
    override def transformation[I <: Int](retrieve: I => ExprF[I],
                                          record: ExprF[I] => I): ExprF[I] => ExprF[I] = {
      case InputF(id, tpe: ProductTagAny) =>
        val fields = tpe.fields.map {
          case Field(name, tpe, pos) =>
            val fieldId = TypedIdent(id.id.subIdent(name), tpe)
            record(InputF(fieldId, tpe))
        }
        ProductF(fields, tpe)

      case x => x
    }
  }

  val asErrors = new Transformation[ExprF, ExprF] {
    override def transformation[I <: Int](retrieve: I => ExprF[I],
                                          record: ExprF[I] => I): ExprF[I] => ExprF[I] = {
//      case ComputationF(bool.And, args, _) =>
//        ComputationF(double.Add, args)
      case ComputationF(double.LEQ, Vec(a, b), _) =>
        val bMinusA = record(
          ComputationF(double.Add, Vec(b, record(ComputationF(double.Negate, Vec(a))))))
        ComputationF(double.Min, record(CstF(Value(0.0), Tag.ofDouble)), bMinusA)
      case ComputationF(double.LT, Vec(a, b), _) =>
        val bMinusA = record(
          ComputationF(double.Add,
                       b,
                       record(ComputationF(double.Negate, Vec(a))),
                       record(CstF(Value(-epsilon), Tag.ofDouble)))
        )
        ComputationF(double.Min, record(CstF(Value(0.0), Tag.ofDouble)), bMinusA)
      case ComputationF(double.EQ, Vec(a, b), _) =>
        ComputationF(double.Add, a, record(ComputationF(double.Negate, b)))
//      case ComputationF(bool.Or, _, _) => ???
//      case ComputationF(bool.Not, _, _) => ???
      case x => x
    }
  }

  val withHeadTail = new Transformation[ExprF, ExprF] {
    override def transformation[I <: Int](retrieve: I => ExprF[I],
                                          record: ExprF[I] => I): ExprF[I] => ExprF[I] = {
      case InputF(id, tpe: SequenceTagAny) =>
        val headIdent = TypedIdent(id.id.subIdent("first"), tpe.memberTag)
        val lastIdent = TypedIdent(id.id.subIdent("last"), tpe.memberTag)
        val middleIdent = TypedIdent(id.id.subIdent("middle"), tpe)

        // three list
        val head = record(SequenceF(Vec(record(InputF(headIdent))), tpe))
        val middle = record(InputF(middleIdent))
        val tail = record(SequenceF(Vec(record(InputF(lastIdent))), tpe))

        import Tag.unsafe.ofAny
        ComputationF(sequence.Concat[Any], head, /*middle,*/ tail)

//        SequenceF(Vec(head, tail), tpe)

      case fi @ ComputationF(_: sequence.First[_], Vec(seq), _) =>
        retrieve(seq) match {
          case SequenceF(members, _) =>
            require(members.size > 0)
            retrieve(members.firstUnsafe)
          case ComputationF(_: sequence.Concat[_], args, _) if args.nonEmpty =>
            retrieve(args.firstUnsafe) match {
              case SequenceF(members, _) if members.nonEmpty =>
                retrieve(members.firstUnsafe)
              case _ =>
                fi
            }
          case _ =>
            val a = retrieve(seq)
            println(a)
            fi

        }

      case fi @ ComputationF(_: sequence.Last[_], Vec(seq), _) =>
        retrieve(seq) match {
          case SequenceF(members, _) if members.nonEmpty =>
            retrieve(members.lastUnsafe)
          case ComputationF(_: sequence.Concat[_], args, _) if args.nonEmpty =>
            retrieve(args.lastUnsafe) match {
              case SequenceF(members, _) if members.nonEmpty =>
                retrieve(members.lastUnsafe)
              case _ =>
                fi
            }
          case _ =>
            fi
        }

      case x => x
    }
  }

  val optimizer =
    dahu.model.transformations.makeOptimizer[ExprF](dahu.model.transformations.Pass.allStaticPasses)
}
