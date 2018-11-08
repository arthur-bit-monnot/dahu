package dahu.refinement.interop
import dahu.graphs.transformations.Transformation
import dahu.model.functions.{Fun2, FunN}
import dahu.model.input.TypedIdent
import dahu.model.ir._
import dahu.model.math._
import dahu.model.types._
import dahu.model.products.{Field, ProductTagAny}
import dahu.utils._

object transformations {

  private val epsilon = 1e-8

  object MinError extends FunN[Double, Double] {
    override def name: String = "err.or"
    override def of(args: scala.Seq[Double]): Double = args.map(math.abs).min
  }

  object CombinedErrors extends Monoid[Double] {
    override def tpe: Tag[Double] = Tag.ofDouble
    override def combine(lhs: Double, rhs: Double): Double =
      math.abs(lhs) + math.abs(rhs)
    override val identity: Double = 0.0
    override def name: String = "err.and"
  }

  object ErrEqual extends Fun2[Double, Double, Double] {
    override def of(in1: Double, in2: Double): Double =
      in1 - in2
    override def name: String = "err.="
  }

  object ErrBelow extends Fun2[Double, Double, Double] {
    override def of(in1: Double, in2: Double): Double =
      if(in1 <= in2) 0.0
      else in1 - in2
    override def name: String = "err.<="
  }

  object ErrStrictlyBelow extends Fun2[Double, Double, Double] {
    override def of(in1: Double, in2: Double): Double =
      if((in1 + epsilon) <= in2) 0.0
      else (in1 + epsilon) - in2
    override def name: String = "err.<"
  }

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
      case ComputationF(double.LEQ, args, _) =>
        ComputationF(ErrBelow, args)
      case ComputationF(double.LT, args, _) =>
        ComputationF(ErrStrictlyBelow, args)
      case ComputationF(double.EQ, args, _) =>
        ComputationF(ErrEqual, args)
      case ComputationF(bool.Or, args, _) =>
        ComputationF(MinError, args)
      case ComputationF(bool.And, conjuncts, _) =>
        ComputationF(CombinedErrors, conjuncts)
//      case x if x.typ != Tag.ofDouble => ???

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
//        val head = record(SequenceF(Vec(record(InputF(headIdent))), tpe))
//        val middle = record(InputF(middleIdent))
//        val tail = record(SequenceF(Vec(record(InputF(lastIdent))), tpe))

        import Tag.unsafe.ofAny
//        ComputationF(sequence.Concat[Any], head, /*middle,*/ tail) // TODO

//        val head = record(InputF(headIdent))
//        val a1 = record(InputF(TypedIdent(id.id.subIdent("1"), tpe.memberTag)))
//        val a2 = record(InputF(TypedIdent(id.id.subIdent("2"), tpe.memberTag)))
//        val tail = record(InputF(lastIdent))
        val intermediates = for(i <- 1 until 20)
          yield record(InputF(TypedIdent(id.id.subIdent(i.toString), tpe.memberTag)))

//        SequenceF(Vec(head, tail), tpe)
        SequenceF(intermediates, tpe)

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
