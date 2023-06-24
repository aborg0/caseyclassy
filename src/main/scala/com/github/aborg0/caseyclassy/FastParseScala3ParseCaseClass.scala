package com.github.aborg0.caseyclassy

import fastparse._
import fastparse.NoWhitespace._
import java.time.{LocalDate, LocalTime}

import scala.deriving.Mirror


private[caseyclassy] trait FPScala3Implementations {
  given FastParseParse[Boolean] with {
    override protected[caseyclassy] def parser[$: P]: P[Boolean] = P("true" | "false").!.map(_.toBoolean)
  }

  private[this] def integral[$:P]: P[String] = P("-".? ~ CharIn("0-9").rep(1)).!

  private[this] def numeric[$:P]: P[String] = P("NaN" | "-".? ~ "Infinity" |
    ("-".? ~ CharIn("0-9").rep(1) ~
      ("." ~/ CharIn("0-9").rep(1)).? ~
      (CharIn("eE") ~/ CharIn("+\\-").? ~/ CharIn("0-9").rep(1)).?)).!

  given FastParseParse[Long] with {
    override protected[caseyclassy] def parser[$: P]: P[Long] = integral.map(_.toLong)
  }

  given FastParseParse[Int] with {
    override protected[caseyclassy] def parser[$: P]: P[Int] = integral.map(_.toInt)
  }

  given FastParseParse[Short] with {
    override protected[caseyclassy] def parser[$: P]: P[Short] = integral.map(_.toShort)
  }

  given FastParseParse[Byte] with {
    override protected[caseyclassy] def parser[$: P]: P[Byte] = integral.map(_.toByte)
  }

  given FastParseParse[Double] with {
    override protected[caseyclassy] def parser[$: P]: P[Double] = numeric.map(_.toDouble)
  }

  given FastParseParse[Float] with {
    override protected[caseyclassy] def parser[$: P]: P[Float] = numeric.map(_.toFloat)
  }

//  implicit def parseHNil: FastParseParse[HNil] = () => Pass.map(_ => HNil)
//
//  implicit def parseCNil: FastParseParse[CNil] = () => Fail.log("CNil")
//
//  implicit def parseProduct[Head, Tail <: HList](implicit headParse: Lazy[FastParseParse[Head]],
//                                                 tailParse: Lazy[FastParseParse[Tail]]): FastParseParse[Head :: Tail] =
//    () => (headParse.value.parser() ~ ",".? ~ tailParse.value.parser()).map { case (h, t) => h :: t }
//
//  implicit def parseCoproduct[Head, Tail <: Coproduct](implicit headParse: Lazy[FastParseParse[Head]],
//                                                       tailParse: Lazy[FastParseParse[Tail]]): FastParseParse[Head :+: Tail] =
//    () => headParse.value.parser().map(Inl(_)) | tailParse.value.parser().map(Inr(_))
//
//  implicit def generic[A: TypeTag, R](implicit gen: Generic.Aux[A, R], argParse: FastParseParse[R]): FastParseParse[A] = () => {
//    val typeKind = implicitly[TypeTag[A]]
//    if (typeKind.tpe.typeSymbol.isAbstract /* Approximation of sealed trait */ ) argParse.parser().map(gen.from) else {
//      val name = typeKind.tpe.typeSymbol.name.toString
//      if (name.startsWith("Tuple"))
//        P("(" ~/ argParse.parser() ~ ")").map(gen.from)
//      else
//        P(name ~ (("(" ~/ argParse.parser() ~ ")") | argParse.parser())).map(gen.from)
//    }
//  }
}

case object FastParseParse extends EasyDerive[FastParseParse] with Scala3ParseCaseClass with FPScala3Implementations {

  override def deriveCaseClass[T](productType: CaseClassType[T]): FastParseParse[T] = new FastParseParse[T] {
    override protected[caseyclassy] def parser[$: P]: P[T] = {
      val typeName = productType.label
      if (productType.elements.isEmpty) {
        P(typeName).map(_ => productType.fromElements(List.empty))
      } else {
        P((if (typeName.startsWith("Tuple")) Pass else P(typeName)) ~ "(" ~
          //        val argsParser = ctx.parameters.map(param => param.typeclass.parser() ~ ",".?)
          //ctx.rawConstruct(argsParser)
          //        ctx.parameters.map(p => p.typeclass.parser()).map(v => v: Parser[Any])
          //          .reduce((p1, p2) => p1.! ~",".? ~ p2.!)
          (productType.elements.head.typeclass.parser ~
            productType.elements.drop(1).foldLeft(Pass.map(_ => List.empty[Any]))(
              (parser, param) => (parser ~ "," ~ param.typeclass.parser).map{case (s, t) => s :+ t}))
            .map{case(first, rest) => productType.fromElements(first +: rest)}
          ~ ")"
//          sequence(ctx.parameters.map(_.typeclass.parser().map(v=> v: Any)), ",").map(seq => ctx.rawConstruct(seq)) ~ ")"
        )
      }
      //        ctx.parameters.foldLeft(Pass: Parser[Any])((parser, param) => parser ~ ",".? ~ param.typeclass.parser())
        //       ~ ")").map(params => ctx.rawConstruct(params.asInstanceOf[Seq[Any]]))
      //    def show(value: T): String = ctx.parameters.map { p =>
      //      s"${p.label}=${p.typeclass.show(p.dereference(value))}"
      //    }.mkString("{", ",", "}")
    }

  }

  override def deriveSealed[T](sumType: SealedType[T]): FastParseParse[T] = new FastParseParse[T] {
    override def parser[$:P]: P[T] = {
      val parsers: Seq[() => P[T]] = sumType.elements.sortBy(-_.label.length).map(sub => () => sub.typeclass.parser.asInstanceOf[P[T]])
      val func: () => P[T] =
        parsers.reduce((p1, p2) => () => p1() | p2())
      func()
    }
  }

//  given gen[T]: FastParseParse[T]

  override def to[A](input: String)(using parse: FastParseParse[A]): A = {
    parse.parse(input)
  }

//  def apply[A](using p: FastParseParse[A]): FastParseParse[A] = p

  //region Custom overrides of special types
  given FastParseParse[String] with {
    override protected[caseyclassy] def parser[$: P]: P[String] =
      P(CharsWhile(c => c != ',' && c != ')').!) | P("").!
  }

  given FastParseParse[LocalTime] with {
      override protected[caseyclassy] def parser[$: P]: P[LocalTime] = P(
        CharIn("0-9").rep(2, "", 2) ~ ":" ~
          CharIn("0-9").rep(2, "", 2) ~
          (":" ~/ CharIn("0-9").rep(2, "", 2) ~
            ("." ~/ CharIn("0-9").rep(1)).?).?).!.map(LocalTime.parse(_))
    }

  given FastParseParse[LocalDate] with {
    override protected[caseyclassy] def parser[$: P]: P[LocalDate] =
      P(CharIn("0-9").rep(4, "", 4) ~ "-" ~
        CharIn("0-9").rep(1, "", 2) ~ "-" ~
        CharIn("0-9").rep(1, "", 2)).!.map(LocalDate.parse(_))
  }

  given [A](using parseA: FastParseParse[A]): FastParseParse[Seq[A]] with {
    override protected[caseyclassy] def parser[$: P]: P[Seq[A]] =
      P(("WrappedArray" | "List" | "Vector" | "ArraySeq") ~ "(" ~/ parseA.parser.rep(sep = ", ") ~ ")")
  }

  //endregion
}

trait Scala3ParseCaseClass {
  def to[A /*<: AnyRef*/](input: String)(using parse: FastParseParse[A]): A
}

trait FastParseParse[A] extends Parse[A] {
  protected[caseyclassy] def parser[$:P]: P[A]

  override def parse(input: String): A = fastparse.parse(input, this.parser(_)).fold((p, i, e) =>
    throw new IllegalArgumentException(s"Expected: $p at position: $i"), (a, i) => a)
}