package com.github.aborg0.caseyclassy

import java.time.{LocalDate, LocalTime}

import scala.reflect.runtime.universe._

import shapeless._

case object RegexParseCaseClass extends ParseCaseClass {

  override def to[A <: AnyRef](input: String)(implicit parse: Lazy[Parse[A]]): A = {
    parse.value.parse(input)
  }

  def apply[A](implicit p: Lazy[Parse[A]]): Parse[A] = p.value

  implicit def longParse: Parse[Long] = _.toLong

  implicit def intParse: Parse[Int] = _.toInt

  implicit def shortParse: Parse[Short] = _.toShort

  implicit def byteParse: Parse[Byte] = _.toByte

  implicit def doubleParse: Parse[Double] = _.toDouble

  implicit def floatParse: Parse[Float] = _.toFloat

  implicit def booleanParse: Parse[Boolean] = _.toBoolean

  implicit def stringParse: Parse[String] = input => input

  implicit def timeParse: Parse[LocalTime] = LocalTime.parse(_)

  implicit def dateParse: Parse[LocalDate] = LocalDate.parse(_)

  implicit def seqConverter[A](implicit parse: Parse[A]): Parse[Seq[A]] = input => {
    ???
  }

  implicit def parseHNil: Parse[HNil] = input => if (input.isEmpty) HNil else throw new IllegalArgumentException(s"Non-empty: |$input|")

  implicit def parseH[Head, Tail <: HList](implicit headParse: Lazy[Parse[Head]], tailParse: Lazy[Parse[Tail]]): Parse[Head :: Tail] = input => {
    val head = headParse.value.parse(input)
    head :: tailParse.value.parse(input.substring(head.toString.length))
  }

  implicit def generic[A: TypeTag, R](implicit gen: Generic.Aux[A, R], parse: Parse[R]): Parse[A] = input => {
    val typeKind = the[TypeTag[A]]
    import scala.reflect.runtime.universe._
    if (typeKind.tpe <:< typeTag[AnyVal].tpe || typeKind.tpe <:< typeTag[String].tpe || typeKind.tpe <:< typeTag[LocalDate].tpe || typeKind.tpe <:< typeTag[LocalTime].tpe) {
      gen.from(parse.parse(input))
    } else if (input.startsWith(typeKind.tpe.typeSymbol.name.toString)) {
      val name = typeKind.tpe.typeSymbol.name.toString
      val rest = if (input.length > name.length + 1) input.substring(name.length + 1, input.length - 1) else ""
      gen.from(parse.parse(rest))
    } else {
      throw new IllegalArgumentException(s"|$input| does not start with ${typeKind.tpe.typeSymbol.name.toString}")
    }
  }
}
