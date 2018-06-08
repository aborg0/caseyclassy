package com.github.aborg0.caseyclassy

import java.time.{LocalDate, LocalTime}

import com.github.aborg0.caseyclassy.example.TwoArgsBoolInt
import org.scalatest.FlatSpec
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor1, TableFor2}

class PCTwoArgsTests extends FlatSpec with TableDrivenPropertyChecks {
  val implementations: TableFor1[ParseCaseClass] = Table("implementation", ParserCombinatorParseCaseClass)

  import ParserCombinatorParseCaseClass._
  behavior of "FastParseParseCaseClass for two args cases"
  it should "parse Tuple2[Int, LocalDate]" in {
    val intDateInputs: TableFor2[ParseCaseClass, (Int, LocalDate)] = Table(
      ("implementation", "(Int, LocalDate)"),
      Seq(1,
        0,
        -11111111,
      ).flatMap(i => Seq(LocalDate.of(1970, 1, 1), LocalDate.of(2000, 12, 31)).map((i, _))).flatMap(tup =>
        implementations.map(impl => impl -> tup)): _*
    )
    forAll(intDateInputs) { (impl: ParseCaseClass, input: (Int, LocalDate)) =>
      assert(impl.to[(Int, LocalDate)](input.toString) === input)
    }
  }
  it should "parse Tuple2[Byte, LocalTime]" in {
    val byteTimeInputs: TableFor2[ParseCaseClass, (Byte, LocalTime)] = Table(
      ("implementation", "(Byte, LocalTime)"),
      Seq(1.toByte,
        0.toByte,
        Byte.MinValue,
      ).flatMap(i => Seq(LocalTime.of(0, 0, 0), LocalTime.of(23, 59, 59)).map((i, _))).flatMap(tup =>
        implementations.map(impl => impl -> tup)): _*
    )
    forAll(byteTimeInputs) { (impl: ParseCaseClass, input: (Byte, LocalTime)) =>
      assert(impl.to[(Byte, LocalTime)](input.toString) === input)
    }
  }
  it should "parse Tuple2[Float, Long]" in {
    val floatLongInputs: TableFor2[ParseCaseClass, (Float, Long)] = Table(
      ("implementation", "(Float, Long)"),
      Seq(1l,
        0l,
        Long.MinValue,
        Long.MaxValue,
      ).flatMap(i => Seq(Float.NegativeInfinity, -3.14159f).map((_, i))).flatMap(tup =>
        implementations.map(impl => impl -> tup)): _*
    )
    forAll(floatLongInputs) { (impl: ParseCaseClass, input: (Float, Long)) =>
      assert(impl.to[(Float, Long)](input.toString) === input)
    }
  }

  it should "parse TwoArgsBoolInt" in {
    val simpleDoubleInputs: TableFor2[ParseCaseClass, TwoArgsBoolInt] = Table(
      ("implementation", "TwoArgsBoolInt"),
      Seq(1,
        0,
        -11111111,
      ).flatMap(i => Seq(true, false).map((i, _))).flatMap(tup =>
        implementations.map(impl => impl -> TwoArgsBoolInt(tup._2, tup._1))): _*
    )
    forAll(simpleDoubleInputs) { (impl: ParseCaseClass, input: TwoArgsBoolInt) =>
      assert(impl.to[TwoArgsBoolInt](input.toString) === input)
    }

  }

}
