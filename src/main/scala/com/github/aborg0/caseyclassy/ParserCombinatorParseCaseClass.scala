package com.github.aborg0.caseyclassy

import java.time.{LocalDate, LocalTime}

import shapeless._

import scala.reflect.runtime.universe._
import scala.util.parsing.combinator._


private[caseyclassy] trait PCGenericImplementations extends RegexParsers {
  implicit def longParse: ParserCombinatorParse[Long] = new ParserCombinatorParse[Long] {
    override protected[caseyclassy] def parser: Parser[Long] = "\\-?\\d+".r ^^ {
      _.toLong
    }
  }

  implicit def intParse: ParserCombinatorParse[Int] = new ParserCombinatorParse[Int] {
    override protected[caseyclassy] def parser: Parser[Int] = "\\-?\\d+".r ^^ {
      _.toInt
    }
  }

  implicit def shortParse: ParserCombinatorParse[Short] = new ParserCombinatorParse[Short] {
    override protected[caseyclassy] def parser: Parser[Short] = "\\-?\\d+".r ^^ {
      _.toShort
    }
  }

  implicit def byteParse: ParserCombinatorParse[Byte] = new ParserCombinatorParse[Byte] {
    override protected[caseyclassy] def parser: Parser[Byte] = "\\-?\\d+".r ^^ {
      _.toByte
    }
  }

  implicit def booleanParse: ParserCombinatorParse[Boolean] = new ParserCombinatorParse[Boolean] {
    override protected[caseyclassy] def parser: Parser[Boolean] = "true|false".r ^^ {
      _.toBoolean
    }
  }


  implicit def doubleParse: ParserCombinatorParse[Double] = new ParserCombinatorParse[Double] {
    private[this] def numeric: Parser[String] = "NaN".r | "\\-?Infinity".r | "-?\\d+(\\.\\d+)?([eE][+-]?\\d+)?".r

    override protected[caseyclassy] def parser: Parser[Double] = numeric ^^ {
      _.toDouble
    }
  }

  implicit def floatParse: ParserCombinatorParse[Float] = new ParserCombinatorParse[Float] {
    private[this] def numeric: Parser[String] = "NaN".r | "\\-?Infinity".r | "-?\\d+(\\.\\d+)?".r

    override protected[caseyclassy] def parser: Parser[Float] = numeric ^^ {
      _.toFloat
    }
  }


  implicit def parseHNil: ParserCombinatorParse[HNil] = new ParserCombinatorParse[HNil] {
    override protected[caseyclassy] def parser: Parser[HNil] = success(HNil)
  }

  implicit def parseCNil: ParserCombinatorParse[CNil] = new ParserCombinatorParse[CNil] {
    override protected[caseyclassy] def parser: Parser[CNil] = failure("CNil")
  }

  implicit def parseProduct[Head, Tail <: HList](implicit headParse: Lazy[ParserCombinatorParse[Head]], tailParse: Lazy[ParserCombinatorParse[Tail]]): ParserCombinatorParse[Head :: Tail] =
    new ParserCombinatorParse[Head :: Tail] {
      override protected[caseyclassy] def parser: Parser[Head :: Tail] =
        (headParse.value.parser.asInstanceOf[Parser[Head]] ~ ",".r.? ~ tailParse.value.parser.asInstanceOf[Parser[Tail]]) ^^ {case h ~ _ ~ t => h.asInstanceOf[Head] :: t.asInstanceOf[Tail]}
    }

  implicit def parseCoproduct[Head: TypeTag, Tail <: Coproduct](implicit headParse: Lazy[ParserCombinatorParse[Head]], tailParse: Lazy[ParserCombinatorParse[Tail]]): ParserCombinatorParse[Head :+: Tail] = new ParserCombinatorParse[Head :+: Tail] {
    override protected[caseyclassy] def parser: Parser[Head :+: Tail] = headParse.value.parser.asInstanceOf[Parser[Head]] ^^ {Inl(_)} | tailParse.value.parser.asInstanceOf[Parser[Tail]] ^^ {Inr(_)}
  }

  implicit def generic[A: TypeTag, R](implicit gen: Generic.Aux[A, R], argParse: ParserCombinatorParse[R]): ParserCombinatorParse[A] = new ParserCombinatorParse[A] {
    override protected[caseyclassy] def parser: Parser[A] = {
      val typeKind = implicitly[TypeTag[A]]
      if (typeKind.tpe.typeSymbol.isAbstract /* Approximation of sealed trait */ ) argParse.parser.asInstanceOf[Parser[R]] ^^ gen.from else {
        val name = typeKind.tpe.typeSymbol.name.toString
        if (name.startsWith("Tuple"))
          ("\\(".r ~> argParse.parser.asInstanceOf[Parser[R]] <~ "\\)".r) ^^ {gen.from}
        else
          (s"$name\\(".r ~> argParse.parser.asInstanceOf[Parser[R]] <~ "\\)".r) ^^ gen.from |
            (s"$name".r ~> argParse.parser.asInstanceOf[Parser[R]]) ^^ gen.from
      }

    }
  }
}

case object ParserCombinatorParseCaseClass extends ParseCaseClass with PCGenericImplementations {

  override def to[A](input: String)(implicit parse: Lazy[Parse[A]]): A = {
    parse.value.parse(input)
  }

  def apply[A](implicit p: Lazy[ParserCombinatorParse[A]]): ParserCombinatorParse[A] = p.value

  //region Custom overrides of special types

  implicit def stringParse: ParserCombinatorParse[String] = new ParserCombinatorParse[String] {
    override protected[caseyclassy] def parser(): Parser[String] =
      """(?:[^,\)])*""".r ^^ {
        _.toString
      }
  }

  implicit def timeParse: ParserCombinatorParse[LocalTime] = new ParserCombinatorParse[LocalTime] {
    override protected[caseyclassy] def parser: Parser[LocalTime] = "\\d{2}\\:\\d{2}(?:\\:\\d{2})?".r ^^ {
      LocalTime.parse(_)
    }
  }

  implicit def dateParse: ParserCombinatorParse[LocalDate] = new ParserCombinatorParse[LocalDate] {
    override protected[caseyclassy] def parser: Parser[LocalDate] = "\\d{4}-\\d{2}-\\d{2}".r ^^ {
      LocalDate.parse(_)
    }
  }

  implicit def seqConverter[A](implicit parseA: ParserCombinatorParse[A]): ParserCombinatorParse[Seq[A]] = new ParserCombinatorParse[Seq[A]] {
    override protected[caseyclassy] def parser: Parser[Seq[A]] = "(?:WrappedArray|List|Vector)\\(".r ~> repsep[A](parseA.parser.asInstanceOf[Parser[A]], ",": Parser[Any]) <~ "\\)".r
  }
  //endregion
}

trait ParserCombinatorParse[A] extends Parse[A] with RegexParsers {
  protected[caseyclassy] def parser: Parser[A]

  override def parse(input: String): A = super[RegexParsers].parse[A](parser, input).get //.fold((p, i, e) => throw new IllegalArgumentException(s"Expected: $p at position: $i"), (a, i) => a)
}
