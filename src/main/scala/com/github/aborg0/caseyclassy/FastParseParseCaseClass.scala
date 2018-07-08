package com.github.aborg0.caseyclassy

import fastparse.all._
import java.time.{LocalDate, LocalDateTime, LocalTime}

import shapeless._

import scala.reflect.runtime.universe._

private[caseyclassy] trait FPGenericImplementations {
  implicit def booleanParse: FastParseParse[Boolean] = () => P("true" | "false").!.map(_.toBoolean)

  private[this] def integral: Parser[String] = P("-".? ~ CharIn('0' to '9').rep(1)).!

  private[this] def numeric: Parser[String] = P("NaN" | "-".? ~ "Infinity" |
    ("-".? ~ CharIn('0' to '9').rep(1) ~
      ("." ~/ CharIn('0' to '9').rep(1)).? ~
      (CharIn("eE") ~/ CharIn("+-").? ~/ CharIn('0' to '9').rep(1)).?)).!

  implicit def longParse: FastParseParse[Long] = () => integral.map(_.toLong)

  implicit def intParse: FastParseParse[Int] = () => integral.map(_.toInt)

  implicit def shortParse: FastParseParse[Short] = () => integral.map(_.toShort)

  implicit def byteParse: FastParseParse[Byte] = () => integral.map(_.toByte)

  implicit def doubleParse: FastParseParse[Double] = () => numeric.!.map(_.toDouble)

  implicit def floatParse: FastParseParse[Float] = () => numeric.!.map(_.toFloat)

  implicit def parseHNil: FastParseParse[HNil] = () => Pass.map(_ => HNil)

  implicit def parseCNil: FastParseParse[CNil] = () => Fail.log("CNil")

  implicit def parseProduct[Head, Tail <: HList](implicit headParse: Lazy[FastParseParse[Head]],
                                                 tailParse: Lazy[FastParseParse[Tail]]): FastParseParse[Head :: Tail] =
    () => (headParse.value.parser() ~ ",".? ~ tailParse.value.parser()).map { case (h, t) => h :: t }

  implicit def parseCoproduct[Head, Tail <: Coproduct](implicit headParse: Lazy[FastParseParse[Head]],
                                                       tailParse: Lazy[FastParseParse[Tail]]): FastParseParse[Head :+: Tail] =
    () => headParse.value.parser().map(Inl(_)) | tailParse.value.parser().map(Inr(_))

  implicit def generic[A: TypeTag, R](implicit gen: Generic.Aux[A, R], argParse: FastParseParse[R]): FastParseParse[A] = () => {
    val typeKind = implicitly[TypeTag[A]]
    if (typeKind.tpe.typeSymbol.isAbstract /* Approximation of sealed trait */ ) argParse.parser().map(gen.from) else {
      val name = typeKind.tpe.typeSymbol.name.toString
      if (name.startsWith("Tuple"))
        P("(" ~/ argParse.parser() ~ ")").map(gen.from)
      else
        P(name.? ~ (("(" ~/ argParse.parser() ~ ")") | argParse.parser())).map(gen.from)
    }
  }
}

case object FastParseParseCaseClass extends ParseCaseClass with FPGenericImplementations {

  override def to[A](input: String)(implicit parse: Lazy[Parse[A]]): A = {
    parse.value.parse(input)
  }

  def apply[A](implicit p: Lazy[FastParseParse[A]]): FastParseParse[A] = p.value

  //region Custom overrides of special types
  implicit def stringParse: FastParseParse[String] = () =>
    P(CharsWhile(c => c != ',' && c != ')').!) | P("").!

  implicit def timeParse: FastParseParse[LocalTime] = () => P(
    CharIn('0' to '9').rep(2, "", 2) ~ ":" ~
      CharIn('0' to '9').rep(2, "", 2) ~
      (":" ~/ CharIn('0' to '9').rep(2, "", 2) ~
        ("." ~/ CharIn('0' to '9').rep(1)).?).?).!.map(LocalTime.parse(_))

  implicit def dateParse: FastParseParse[LocalDate] = () =>
    P(CharIn('0' to '9').rep(4, "", 4) ~ "-" ~
      CharIn('0' to '9').rep(1, "", 2) ~ "-" ~
      CharIn('0' to '9').rep(1, "", 2)).!.map(LocalDate.parse(_))

  implicit def localDateTimeParse: FastParseParse[LocalDateTime] = () =>
    (dateParse.parser() ~ "T" ~ timeParse.parser()) map { case (d, t) => LocalDateTime.of(d, t) }

  implicit def seqConverter[A](implicit parseA: FastParseParse[A]): FastParseParse[Seq[A]] = () =>
    P(("WrappedArray" | "List" | "Vector") ~ "(" ~/ parseA.parser().rep(sep = ", ") ~ ")") |
      P("[" ~/ parseA.parser().rep(sep=", ") ~ "]")

  //endregion
}

trait FastParseParse[A] extends Parse[A] {
  protected[caseyclassy] def parser(): Parser[A]

  override def parse(input: String): A = parser().parse(input).fold((p, i, e) =>
    throw new IllegalArgumentException(s"Expected: $p at position: $i"), (a, i) => a)
}