package com.github.aborg0.caseyclassy

import java.time.{LocalDate, LocalDateTime, LocalTime}

import fastparse.NoWhitespace._
import fastparse._
import shapeless._

import scala.language.implicitConversions
import scala.reflect.runtime.universe._

private[caseyclassy] trait FPGenericImplementations {
  implicit def booleanParse: FastParseParse[Boolean] =
    new FastParseParse[Boolean] {
      override protected[caseyclassy] def parser[_: P]: P[Boolean] =
        P("true" | "false").!.map(_.toBoolean)
    }

  private[this] def integral[_: P]: P[String] =
    P("-".? ~ CharIn("0-9").rep(1)).!

  private[this] def numeric[_: P]: P[String] =
    P(
      "NaN" | "-".? ~ "Infinity" |
        ("-".? ~ CharIn("0-9").rep(1) ~
          ("." ~/ CharIn("0-9").rep(1)).? ~
          (CharIn("eE") ~/ CharIn("\\-", "+").? ~/ CharIn("0-9").rep(1)).?)).!

  implicit def longParse: FastParseParse[Long] =
    new FastParseParse[Long] {
      override protected[caseyclassy] def parser[_: P]: P[Long] =
        integral.map(_.toLong)
    }

  implicit def intParse: FastParseParse[Int] = new FastParseParse[Int] {
    override protected[caseyclassy] def parser[_: P]: P[Int] =
      integral.map(_.toInt)
  }

  implicit def shortParse: FastParseParse[Short] =
    new FastParseParse[Short] {
      override protected[caseyclassy] def parser[_: P]: P[Short] =
        integral.map(_.toShort)
    }

  implicit def byteParse: FastParseParse[Byte] =
    new FastParseParse[Byte] {
      override protected[caseyclassy] def parser[_: P]: P[Byte] =
        integral.map(_.toByte)
    }

  implicit def doubleParse: FastParseParse[Double] =
    new FastParseParse[Double] {
      override protected[caseyclassy] def parser[_: P]: P[Double] =
        numeric.!.map(_.toDouble)
    }

  implicit def floatParse: FastParseParse[Float] =
    new FastParseParse[Float] {
      override protected[caseyclassy] def parser[_: P]: P[Float] =
        numeric.!.map(_.toFloat)
    }

  implicit def parseHNil: FastParseParse[HNil] =
    new FastParseParse[HNil] {
      override protected[caseyclassy] def parser[_: P]: P[HNil] =
        Pass.map(_ => HNil)
    }

  implicit def parseCNil: FastParseParse[CNil] =
    new FastParseParse[CNil] {
      override protected[caseyclassy] def parser[_: P]: P[CNil] =
        Fail.log("CNil")
    }

  implicit def parseProduct[Head, Tail <: HList](
      implicit headParse: Lazy[FastParseParse[Head]],
      tailParse: Lazy[FastParseParse[Tail]]): FastParseParse[Head :: Tail] =
    new FastParseParse[Head :: Tail] {
      override protected[caseyclassy] def parser[_: P]: P[Head :: Tail] =
        (headParse.value.parser ~ ",".? ~ tailParse.value.parser).map {
          case (h, t) => h :: t
        }
    }

  implicit def parseCoproduct[Head, Tail <: Coproduct](
      implicit headParse: Lazy[FastParseParse[Head]],
      tailParse: Lazy[FastParseParse[Tail]]): FastParseParse[Head :+: Tail] =
    new FastParseParse[Head :+: Tail] {
      override protected[caseyclassy] def parser[_: P]: P[Head :+: Tail] =
        headParse.value.parser.map(Inl(_)) | tailParse.value.parser.map(Inr(_))
    }

  implicit def generic[A: TypeTag, R](
      implicit gen: Generic.Aux[A, R],
      argParse: FastParseParse[R]): FastParseParse[A] = new FastParseParse[A] {
    override protected[caseyclassy] def parser[_: P]: P[A] = {
      val typeKind = implicitly[TypeTag[A]]
      if (typeKind.tpe.typeSymbol.isAbstract /* Approximation of sealed trait */ )
        argParse.parser.map(gen.from)
      else {
        val name = typeKind.tpe.typeSymbol.name.toString
        if (name.startsWith("Tuple"))
          P("(" ~/ argParse.parser ~ ")").map(gen.from)
        else
          P((name.? ~ ("(" ~ argParse.parser ~ ")")) | (name ~ argParse.parser))
            .map(gen.from)
      }
    }
  }
}

case object FastParseParseCaseClass
    extends ParseCaseClass
    with FPGenericImplementations {
//import ParseCaseClass._
  override def to[A](input: String)(implicit parse: Lazy[Parse[A]]): A = {
    parse.value.parse(input)
  }

  def apply[A](implicit p: Lazy[FastParseParse[A]]): FastParseParse[A] = p.value

  //region Custom overrides of special types
  implicit def stringParse: FastParseParse[String] =
    new FastParseParse[String] {
      override protected[caseyclassy] def parser[_: P]: P[String] =
        P(CharsWhile(c => c != ',' && c != ')').!) | P("").!
    }

  implicit def timeParse: FastParseParse[LocalTime] =
    new FastParseParse[LocalTime] {
      override protected[caseyclassy] def parser[_: P]: P[LocalTime] =
        P(
          CharIn("0-9").rep(2, "", 2) ~ ":" ~
            CharIn("0-9").rep(2, "", 2) ~
            (":" ~/ CharIn("0-9").rep(2, "", 2) ~
              ("." ~/ CharIn("0-9").rep(1)).?).?).!.map(LocalTime.parse(_))
    }

  implicit def dateParse: FastParseParse[LocalDate] =
    new FastParseParse[LocalDate] {
      override protected[caseyclassy] def parser[_: P]: P[LocalDate] =
        P(
          CharIn("0-9").rep(4, "", 4) ~ "-" ~
            CharIn("0-9").rep(1, "", 2) ~ "-" ~
            CharIn("0-9").rep(1, "", 2)).!.map(LocalDate.parse(_))
    }

  implicit def localDateTimeParse: FastParseParse[LocalDateTime] =
    new FastParseParse[LocalDateTime] {
      override protected[caseyclassy] def parser[_: P]: P[LocalDateTime] =
        (dateParse.parser ~ "T" ~ timeParse.parser) map {
          case (d, t) => LocalDateTime.of(d, t)
        }
    }

  implicit def seqConverter[A](
      implicit parseA: FastParseParse[A]): FastParseParse[Seq[A]] =
    new FastParseParse[Seq[A]] {
      override protected[caseyclassy] def parser[_: P]: P[Seq[A]] =
        P(
          ("WrappedArray" | "List" | "Vector" | "ArraySeq") ~ "(" ~/ parseA.parser.rep(
            sep = ", ") ~ ")") |
          P("[" ~/ parseA.parser.rep(sep = ", ") ~ "]")
    }

  //endregion
}

trait FastParseParse[A] extends Parse[A] {
  protected[caseyclassy] def parser[_: P]: P[A]

  override def parse(input: String): A =
    fastparse
      .parse(input, parser(_))
      .fold(
        (p, i, e) =>
          throw new IllegalArgumentException(s"Expected: $p at position: $i"),
        (a, i) => a)
}
