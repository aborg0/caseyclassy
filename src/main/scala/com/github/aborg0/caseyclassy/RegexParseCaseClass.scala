package com.github.aborg0.caseyclassy

import java.time.{LocalDate, LocalTime}

import scala.reflect.runtime.universe._
import scala.util.Try
import shapeless._

import scala.util.matching.Regex

private[caseyclassy] trait GenericImplementations {
  implicit def longParse: RegexParse[Long] = _.toLong

  implicit def intParse: RegexParse[Int] = _.toInt

  implicit def shortParse: RegexParse[Short] = _.toShort

  implicit def byteParse: RegexParse[Byte] = _.toByte

  implicit def doubleParse: RegexParse[Double] = _.toDouble

  implicit def floatParse: RegexParse[Float] = _.toFloat

  implicit def booleanParse: CommonRegexParse[Boolean] = new CommonRegexParse[Boolean] {
    protected[caseyclassy] override val pattern: Regex = "(true|false)".r

    override def toResult(found: String): Boolean = found.toBoolean

    //override def parse(input: String): Boolean = input.toBoolean
  }


  implicit def parseHNil: RegexParse[HNil] = new CommonRegexParse[HNil] {
    protected[caseyclassy] override val pattern: Regex = "()".r

    override def toResult(found: String): HNil = if (found.isEmpty) HNil else throw new IllegalArgumentException(s"Non-empty: |$found|")
  }

  implicit def parseCNil: RegexParse[CNil] = new CommonRegexParse[CNil] {
    override def toResult(found: String): CNil = throw new IllegalStateException(found)

    override def parse(input: String): CNil = throw new IllegalStateException(input)

    override protected[caseyclassy] val pattern: Regex = "".r
  }

  implicit def parseProduct[Head, Tail <: HList](implicit headParse: Lazy[RegexParse[Head]], tailParse: Lazy[RegexParse[Tail]]): RegexParse[Head :: Tail] = new CommonRegexParse[::[Head, Tail]] {
    protected[caseyclassy] override val pattern: Regex = {
      val tailPattern = tailParse.value.pattern.pattern.pattern
      if (tailPattern.length <= 2/*"((?=\\)))".length*/ /*"()"*/)
        toNonCapturing(headParse.value.pattern.pattern.pattern).r
      else
        s"${toNonCapturing(headParse.value.pattern.pattern.pattern)},${toNonCapturing(tailPattern)}".r
    }

    override def toResult(found: String): Head :: Tail = {
      val head = headParse.value.parse(found)
      head :: tailParse.value.parse(found.substring(head.toString.length))
    }

    override def parse(input: String): Head :: Tail = {
      val head = headParse.value.parse(input)
      val headLength = head.toString.length
      head :: tailParse.value.parse(if (input.length > headLength)input.substring(headLength + 1) else "")
    }

  }

  implicit def parseCoproduct[Head, Tail <: Coproduct](implicit headParse: Lazy[RegexParse[Head]], tailParse: Lazy[RegexParse[Tail]]): RegexParse[Head :+: Tail] = new CommonRegexParse[:+:[Head, Tail]] {
    protected[caseyclassy] override val pattern: Regex = {
      val tailPattern = tailParse.value.pattern.pattern.pattern
      if (tailPattern.isEmpty)
        toNonCapturing(headParse.value.pattern.pattern.pattern).r
      else
        s"(${toNonCapturing(headParse.value.pattern.pattern.pattern)}|${toNonCapturing(tailPattern)})".r
    }
    override def toResult(found: String): Head :+: Tail = {
      Try(Inl(headParse.value.parse(found))).getOrElse {
        Inr(tailParse.value.parse(found))
      }
    }
    override def parse(input: String): Head :+: Tail = {
      val leftValue = Try(Inl(headParse.value.parse(input)))
      leftValue.getOrElse {
        Inr(tailParse.value.parse(input))
      }
    }

  }


  implicit def generic[A: TypeTag, R](implicit gen: Generic.Aux[A, R], argParse: RegexParse[R]): RegexParse[A] = new CommonRegexParse[A] {
    protected[caseyclassy] override val pattern: Regex = {
      val typeKind = implicitly[TypeTag[A]]
      import scala.reflect.runtime.universe._
      if (typeKind.tpe <:< typeTag[AnyVal].tpe || typeKind.tpe <:< typeTag[String].tpe || typeKind.tpe <:< typeTag[LocalDate].tpe || typeKind.tpe <:< typeTag[LocalTime].tpe) {
        s"(${toNonCapturing(argParse.pattern.pattern.pattern)})".r
      } else if (typeKind.tpe.typeSymbol.isAbstract /*approximation of sealed base*/) {
        toNonCapturing(argParse.pattern.pattern.pattern).r
      } else {
        val name = typeKind.tpe.typeSymbol.name.toString
        s"(?:$name)(?:\\()?(${toNonCapturing(argParse.pattern.pattern.pattern)})(?:\\))?".r
      }
    }

    override def toResult(found: String): A = ???

    override def parse(input: String): A = {
      val typeKind = implicitly[TypeTag[A]]
      import scala.reflect.runtime.universe._
      if (typeKind.tpe <:< typeTag[AnyVal].tpe || typeKind.tpe <:< typeTag[String].tpe || typeKind.tpe <:< typeTag[LocalDate].tpe || typeKind.tpe <:< typeTag[LocalTime].tpe) {
        gen.from(argParse.parse(input))
      } else if (pattern.findPrefixOf(input).isDefined/*input.startsWith(typeKind.tpe.typeSymbol.name.toString)*/) {
        val name = typeKind.tpe.typeSymbol.name.toString
        if (input.startsWith(name)) {
          val rest = if (input.length > name.length + 1) input.substring(name.length + 1, input.length - 1) else ""
//          if (argParse.pattern.pattern.pattern.length < 3 && rest.isEmpty) {
//            gen.from(HNil.asInstanceOf[R])
//          } else
            gen.from(argParse.parse(rest))
        } else {
          gen.from(argParse.parse(input))
        }
      } else if (typeKind.tpe.typeSymbol.isAbstract/*approximation of sealed base*/){
        // fall back for base sealed structure
        gen.from(argParse.parse(input))
      } else
          throw new IllegalArgumentException(s"|$input| does not start with ${typeKind.tpe.typeSymbol.name.toString}")
    }
  }
}

case object RegexParseCaseClass extends ParseCaseClass with GenericImplementations {

  override def to[A <: AnyRef](input: String)(implicit parse: Lazy[Parse[A]]): A = {
    parse.value.parse(input)
  }

  def apply[A](implicit p: Lazy[RegexParse[A]]): RegexParse[A] = p.value

  //region Custom overrides of special types
  implicit def stringParse: CommonRegexParse[String] = input => input

  implicit def timeParse: CommonRegexParse[LocalTime] = LocalTime.parse(_)

  implicit def dateParse: CommonRegexParse[LocalDate] = new CommonRegexParse[LocalDate] {
    override def toResult(found: String): LocalDate = LocalDate.parse(found)

    override protected[caseyclassy] val pattern: Regex = "(\\d{4}\\-\\d{2}\\-\\d{2})".r
  }


  implicit def seqConverter[A](implicit parse: RegexParse[A]): CommonRegexParse[Seq[A]] = input => {
    ???
  }

  implicit def tuple1Parse[A](implicit argParse: RegexParse[A]): CommonRegexParse[Tuple1[A]] = new CommonRegexParse[Tuple1[A]] {
    protected[caseyclassy] override val pattern: Regex = s"\\((${toNonCapturing(argParse.pattern.pattern.pattern)})\\)".r

    override def toResult(found: String): Tuple1[A] = Tuple1(argParse.parse(found))

    //    override def parse(input: String): Tuple1[A] = Tuple1(argParse.parse(input.substring(1, input.length - 1)))
  }

  //endregion

}

trait RegexParse[A] extends Parse[A] {
  protected[caseyclassy] def pattern: Regex = "([^,)]+?)".r
}

trait CommonRegexParse[A] extends RegexParse[A] {

  def toResult(found: String): A

  override def parse(input: String): A = pattern.findPrefixMatchOf(input).map(m => toResult(m.group(1))).getOrElse(
    throw new IllegalArgumentException(s"|$input| does not start with proper value"))

  protected def toNonCapturing(pattern: String): String = pattern.replaceAll("\\((?!\\?)", "(?:")
}
