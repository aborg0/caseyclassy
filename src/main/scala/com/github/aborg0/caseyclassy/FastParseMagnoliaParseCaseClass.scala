package com.github.aborg0.caseyclassy

import fastparse._
import fastparse.NoWhitespace._
import java.time.{LocalDate, LocalTime}

import language.experimental.macros
import magnolia._

private[caseyclassy] trait FPMagnoliaImplementations {
  implicit def booleanParse: FastParseParse[Boolean] = new FastParseParse[Boolean] {
    override protected[caseyclassy] def parser[_: P]: P[Boolean] = P("true" | "false").!.map(_.toBoolean)
  }

  private[this] def integral[_:P]: P[String] = P("-".? ~ CharIn("0-9").rep(1)).!

  private[this] def numeric[_:P]: P[String] = P("NaN" | "-".? ~ "Infinity" |
    ("-".? ~ CharIn("0-9").rep(1) ~
      ("." ~/ CharIn("0-9").rep(1)).? ~
      (CharIn("eE") ~/ CharIn("+\\-").? ~/ CharIn("0-9").rep(1)).?)).!

  implicit def longParse: FastParseParse[Long] =
    new FastParseParse[Long] {
      override protected[caseyclassy] def parser[_: P]: P[Long] = integral.map(_.toLong)
    }

  implicit def intParse: FastParseParse[Int] = new FastParseParse[Int] {
    override protected[caseyclassy] def parser[_: P]: P[Int] = integral.map(_.toInt)
  }

  implicit def shortParse: FastParseParse[Short] = new FastParseParse[Short] {
    override protected[caseyclassy] def parser[_: P]: P[Short] = integral.map(_.toShort)
  }

  implicit def byteParse: FastParseParse[Byte] = new FastParseParse[Byte] {
    override protected[caseyclassy] def parser[_: P]: P[Byte] = integral.map(_.toByte)
  }

  implicit def doubleParse: FastParseParse[Double] = new FastParseParse[Double] {
    override protected[caseyclassy] def parser[_: P]: P[Double] = numeric.map(_.toDouble)
  }

  implicit def floatParse: FastParseParse[Float] = new FastParseParse[Float] {
    override protected[caseyclassy] def parser[_: P]: P[Float] = numeric.map(_.toFloat)
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

case object FastParseMagnoliaParseCaseClass extends MagnoliaParseCaseClass with FPMagnoliaImplementations {
  type Typeclass[T] = FastParseParse[T]

  def combine[T](ctx: CaseClass[FastParseParse, T]): FastParseParse[T] = new Typeclass[T] {
    override protected[caseyclassy] def parser[_: P]: P[T] = {
      val typeKind = ctx.typeName
      val typeName = typeKind.short
      if (ctx.isObject) {
        P(typeName).map(_ => ctx.rawConstruct(Seq.empty))
      } else if (ctx.parameters.isEmpty) {
        P(typeName ~ "(" ~ ")").map(_ => ctx.rawConstruct(Seq.empty))
      } else {
        P((if (typeName.startsWith("Tuple")) Pass else P(typeName)) ~ "(" ~
          //        val argsParser = ctx.parameters.map(param => param.typeclass.parser() ~ ",".?)
          //ctx.rawConstruct(argsParser)
          //        ctx.parameters.map(p => p.typeclass.parser()).map(v => v: Parser[Any])
          //          .reduce((p1, p2) => p1.! ~",".? ~ p2.!)
          (ctx.parameters.head.typeclass.parser ~
            ctx.parameters.drop(1).foldLeft(Pass().map(_ => Seq.empty[Any]))(
              (parser, param) => (parser ~ "," ~ param.typeclass.parser).map{case (s, t) => s :+ t}))
            .map{case(first, rest) => ctx.rawConstruct(first +: rest)}
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

  def dispatch[T](ctx: SealedTrait[FastParseParse, T]): FastParseParse[T] = new Typeclass[T] {
    override def parser[_:P]: P[T] = {
      P(ctx.subtypes.sortBy(-_.typeName.short.length).map(sub => () => sub.typeclass.parser).reduce((p1, p2) => () => p1() | p2()
      )())
    }
  }

  implicit def gen[T]: FastParseParse[T] = macro Magnolia.gen[T]

  override def to[A](input: String)(implicit parse: Parse[A]): A = {
    parse.parse(input)
  }

  def apply[A](implicit p: FastParseParse[A]): FastParseParse[A] = p

  //region Custom overrides of special types
  implicit def stringParse: FastParseParse[String] = new Typeclass[String] {
    override protected[caseyclassy] def parser[_: P]: P[String] =
      P(CharsWhile(c => c != ',' && c != ')').!) | P("").!
  }

  implicit def timeParse: FastParseParse[LocalTime] =
    new Typeclass[LocalTime] {
      override protected[caseyclassy] def parser[_: P]: P[LocalTime] = P(
        CharIn("0-9").rep(2, "", 2) ~ ":" ~
          CharIn("0-9").rep(2, "", 2) ~
          (":" ~/ CharIn("0-9").rep(2, "", 2) ~
            ("." ~/ CharIn("0-9").rep(1)).?).?).!.map(LocalTime.parse(_))
    }

  implicit def dateParse: FastParseParse[LocalDate] = new Typeclass[LocalDate] {
    override protected[caseyclassy] def parser[_: P]: P[LocalDate] =
      P(CharIn("0-9").rep(4, "", 4) ~ "-" ~
        CharIn("0-9").rep(1, "", 2) ~ "-" ~
        CharIn("0-9").rep(1, "", 2)).!.map(LocalDate.parse(_))
  }

  implicit def seqConverter[A](implicit parseA: FastParseParse[A]): FastParseParse[Seq[A]] = new Typeclass[Seq[A]] {
    override protected[caseyclassy] def parser[_: P]: P[Seq[A]] =
      P(("WrappedArray" | "List" | "Vector" | "ArraySeq") ~ "(" ~/ parseA.parser.rep(sep = ", ") ~ ")")
  }

  //endregion
}

trait MagnoliaParseCaseClass {
  def to[A /*<: AnyRef*/](input: String)(implicit parse: Parse[A]): A
}

trait FastParseParse[A] extends Parse[A] {
  protected[caseyclassy] def parser[_:P]: P[A]

  override def parse(input: String): A = fastparse.parse(input, this.parser(_)).fold((p, i, e) =>
    throw new IllegalArgumentException(s"Expected: $p at position: $i"), (a, i) => a)
}