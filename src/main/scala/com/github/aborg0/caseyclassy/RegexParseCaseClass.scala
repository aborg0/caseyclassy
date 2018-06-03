package com.github.aborg0.caseyclassy

import java.time.{LocalDate, LocalTime}

import shapeless._

import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe._
import scala.util.Try
import scala.util.matching.Regex

private[caseyclassy] trait GenericImplementations {
  implicit def longParse: RegexParse[Long] = _.toLong

  implicit def intParse: RegexParse[Int] = new CommonRegexParse[Int] {
    override protected[caseyclassy] val pattern: Regex = "([\\-]?\\d+)".r

    override def toResult(found: String): Int = found.toInt
  }

  implicit def shortParse: RegexParse[Short] = new CommonRegexParse[Short] {
    override protected[caseyclassy] val pattern: Regex = "([\\-]?\\d+)".r

    override def toResult(found: String): Short = found.toShort
  }

  implicit def byteParse: CommonRegexParse[Byte] = _.toByte

  implicit def doubleParse: RegexParse[Double] = _.toDouble

  implicit def floatParse: CommonRegexParse[Float] = _.toFloat

  implicit def booleanParse: CommonRegexParse[Boolean] = new CommonRegexParse[Boolean] {
    protected[caseyclassy] override val pattern: Regex = "(true|false)".r

    override def toResult(found: String): Boolean = found.toBoolean
  }

  implicit def parseHNil: RegexParse[HNil] = new CommonRegexParse[HNil] {
    protected[caseyclassy] override val pattern: Regex = "()".r

    override def toResult(found: String): HNil = if (found.isEmpty) HNil else throw new IllegalArgumentException(s"Non-empty: |$found|")
  }

  implicit def parseCNil: RegexParse[CNil] = new CommonRegexParseWithoutToResult[CNil] {
    override def parse(input: String): CNil = throw new IllegalStateException(input)

    override protected[caseyclassy] val pattern: Regex = "".r
  }

  implicit def parseProduct[Head, Tail <: HList](implicit headParse: Lazy[RegexParse[Head]], tailParse: Lazy[RegexParse[Tail]]): RegexParse[Head :: Tail] = new CommonRegexParseWithoutToResult[Head :: Tail] {
    protected[caseyclassy] override val pattern: Regex = {
      val tailPattern = tailParse.value.pattern.pattern.pattern
      if (tailPattern.length <= 2 /*"((?=\\)))".length*/
      /*"()"*/ )
        toNonCapturing(headParse.value.pattern.pattern.pattern).r
      else
        s"${toNonCapturing(headParse.value.pattern.pattern.pattern)},${toNonCapturing(tailPattern)}".r
    }

    override def parse(input: String): Head :: Tail = {
      val head = headParse.value.parse(input)
      val headLength = head.toString.length
      head :: tailParse.value.parse(if (input.length > headLength) input.substring(headLength + 1) else "")
    }
  }

  implicit def parseCoproduct[Head, Tail <: Coproduct](implicit headParse: Lazy[RegexParse[Head]], tailParse: Lazy[RegexParse[Tail]]): RegexParse[Head :+: Tail] = new CommonRegexParseWithoutToResult[Head :+: Tail] {
    protected[caseyclassy] override val pattern: Regex = {
      val tailPattern = tailParse.value.pattern.pattern.pattern
      if (tailPattern.isEmpty)
        toNonCapturing(headParse.value.pattern.pattern.pattern).r
      else
        s"((?:${toNonCapturing(headParse.value.pattern.pattern.pattern)})|(?:${toNonCapturing(tailPattern)}))".r
    }

    override def parse(input: String): Head :+: Tail = {
      val leftValue = Try(Inl(headParse.value.parse(input)))
      leftValue.getOrElse {
        Inr(tailParse.value.parse(input))
      }
    }
  }

  implicit def generic[A: TypeTag, R](implicit gen: Generic.Aux[A, R], argParse: RegexParse[R]): RegexParse[A] = new CommonRegexParseWithoutToResult[A] {
    protected[caseyclassy] override val pattern: Regex = {
      val typeKind = implicitly[TypeTag[A]]
      /*if (typeKind.tpe <:< typeTag[AnyVal].tpe || typeKind.tpe <:< typeTag[String].tpe || typeKind.tpe <:< typeTag[LocalDate].tpe || typeKind.tpe <:< typeTag[LocalTime].tpe) {
        s"(${toNonCapturing(argParse.pattern.pattern.pattern)})".r
      } else */ if (typeKind.tpe.typeSymbol.isAbstract /*approximation of sealed base*/ ) {
        toNonCapturing(argParse.pattern.pattern.pattern).r
      } else {
        val name = typeKind.tpe.typeSymbol.name.toString
        if (name.startsWith("Tuple")) {
          s"(?:\\()?(${toNonCapturing(argParse.pattern.pattern.pattern)})(?:\\))?".r
        } else
          s"(?:$name)(?:\\()?(${toNonCapturing(argParse.pattern.pattern.pattern)})(?:\\))?".r
      }
    }

    override def parse(input: String): A = {
      val typeKind = implicitly[TypeTag[A]]
      /*if (typeKind.tpe <:< typeTag[AnyVal].tpe || typeKind.tpe <:< typeTag[String].tpe || typeKind.tpe <:< typeTag[LocalDate].tpe || typeKind.tpe <:< typeTag[LocalTime].tpe) {
        gen.from(argParse.parse(input))
      } else*/ if (pattern.findPrefixOf(input).isDefined /*input.startsWith(typeKind.tpe.typeSymbol.name.toString)*/ ) {
        val name = typeKind.tpe.typeSymbol.name.toString
        if (input.startsWith(name)) {
          val rest = if (input.length > name.length + 1) input.substring(name.length + 1, input.length - 1) else ""
          //          if (argParse.pattern.pattern.pattern.length < 3 && rest.isEmpty) {
          //            gen.from(HNil.asInstanceOf[R])
          //          } else
          gen.from(argParse.parse(rest))
        } else if (name.startsWith("Tuple") && input.length >= 2) {
          gen.from(argParse.parse(input.substring(1, input.length - 1)))
        } else {
          gen.from(argParse.parse(input))
        }
      } else if (typeKind.tpe.typeSymbol.isAbstract /*approximation of sealed base*/ ) {
        // fall back for base sealed structure
        gen.from(argParse.parse(input))
      } else
        throw new IllegalArgumentException(s"|$input| does not start with ${typeKind.tpe.typeSymbol.name.toString}")
    }
  }
}

case object RegexParseCaseClass extends ParseCaseClass with GenericImplementations {

  override def to[A/* <: AnyRef*/](input: String)(implicit parse: Lazy[Parse[A]]): A = {
    parse.value.parse(input)
  }

  def apply[A](implicit p: Lazy[RegexParse[A]]): RegexParse[A] = p.value

  //region Custom overrides of special types
  implicit def stringParse: CommonRegexParse[String] = new CommonRegexParse[String] {

    override protected[caseyclassy] val pattern: Regex = "([^,)]*)".r

    override def toResult(found: String): String = found
  }

  implicit def timeParse: CommonRegexParse[LocalTime] = LocalTime.parse(_)

  implicit def dateParse: CommonRegexParse[LocalDate] = new CommonRegexParse[LocalDate] {
    override def toResult(found: String): LocalDate = LocalDate.parse(found)

    override protected[caseyclassy] val pattern: Regex = "(\\d{4}\\-\\d{2}\\-\\d{2})".r
  }

  implicit def seqConverter[A](implicit parseA: RegexParse[A]): CommonRegexParse[Seq[A]] = new CommonRegexParseWithoutToResult[Seq[A]] {
    override protected[caseyclassy] val pattern: Regex = s"(?:WrappedArray|List|Vector)\\(((?:(?:${toNonCapturing(parseA.pattern.pattern.pattern)})(?:, )?)*)\\)".r

    override def parse(input: String): Seq[A] = pattern.findPrefixMatchOf(input).fold(throw new IllegalArgumentException(s"$input does not start with a Seq"))(m => {
      val all = m.group(1)
      val buffer = new ArrayBuffer[A]()
      var rest = all
      while (rest != "") {
        val newA = parseA.parse(rest)
        rest = rest.substring(newA.toString.length)
        if (rest.startsWith(", ")) {
          rest = rest.substring(", ".length)
        }
        buffer.append(newA)
      }
      buffer.toVector
    })
  }

  //endregion

}

trait RegexParse[A] extends Parse[A] {
  protected[caseyclassy] def pattern: Regex = "([^,)]+)".r
}

trait CommonRegexParse[A] extends RegexParse[A] {

  def toResult(found: String): A

  override def parse(input: String): A = pattern.findPrefixMatchOf(input).map(m => toResult(m.group(1))).getOrElse(
    throw new IllegalArgumentException(s"|$input| does not start with proper value"))

  protected final def toNonCapturing(pattern: String): String = pattern.replaceAll("(?<!\\\\)\\((?!\\?)", "(?:")
}

trait CommonRegexParseWithoutToResult[A] extends CommonRegexParse[A] {
  final def toResult(found: String): A =
    throw new IllegalStateException("Should not be called, consider CommonRegexParse base instead")
}
