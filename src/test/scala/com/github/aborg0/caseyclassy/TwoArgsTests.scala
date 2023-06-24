package com.github.aborg0.caseyclassy

import java.time.{LocalDate, LocalTime}

import com.github.aborg0.caseyclassy.example.TwoArgsBoolInt
import com.github.aborg0.caseyclassy.FastParseParse.given
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor1, TableFor2}

class TwoArgsTests extends AnyFlatSpec with TableDrivenPropertyChecks {
  val implementations: TableFor1[Scala3ParseCaseClass] = Table("implementation", FastParseParse)

  behavior of "ParseCaseClass for two args cases"

  import FastParseParse._
  it should "parse Tuple2[Int, LocalDate]" in {
    val intDateInputs: TableFor2[Scala3ParseCaseClass, (Int, LocalDate)] = Table(
      ("implementation", "(Int, LocalDate)"),
      Seq(1,
        0,
        -11111111,
      ).flatMap(i => Seq(LocalDate.of(1970, 1, 1), LocalDate.of(2000, 12, 31)).map((i, _))).flatMap(tup =>
        implementations.map(impl => impl -> tup)): _*
    )
    forAll(intDateInputs) { (impl: Scala3ParseCaseClass, input: (Int, LocalDate)) =>
      assert(impl.to[(Int, LocalDate)](input.toString) === input)
    }
  }
  it should "parse Tuple2[Byte, LocalTime]" in {
    val byteTimeInputs: TableFor2[Scala3ParseCaseClass, (Byte, LocalTime)] = Table(
      ("implementation", "(Byte, LocalTime)"),
      Seq(1.toByte,
        0.toByte,
        Byte.MinValue,
      ).flatMap(i => Seq(LocalTime.of(0, 0, 0), LocalTime.of(23, 59, 59)).map((i, _))).flatMap(tup =>
        implementations.map(impl => impl -> tup)): _*
    )
    forAll(byteTimeInputs) { (impl: Scala3ParseCaseClass, input: (Byte, LocalTime)) =>
      assert(impl.to[(Byte, LocalTime)](input.toString) === input)
    }
  }
  it should "parse Tuple2[Float, Long]" in {
    val floatLongInputs: TableFor2[Scala3ParseCaseClass, (Float, Long)] = Table(
      ("implementation", "(Float, Long)"),
      Seq(1l,
        0l,
        Long.MinValue,
        Long.MaxValue,
      ).flatMap(i => Seq(Float.NegativeInfinity, -3.14159f).map((_, i))).flatMap(tup =>
        implementations.map(impl => impl -> tup)): _*
    )
    forAll(floatLongInputs) { (impl: Scala3ParseCaseClass, input: (Float, Long)) =>
      assert(impl.to[(Float, Long)](input.toString) === input)
    }
  }

  it should "parse TwoArgsBoolInt" in {
    val simpleDoubleInputs: TableFor2[Scala3ParseCaseClass, TwoArgsBoolInt] = Table(
      ("implementation", "TwoArgsBoolInt"),
      Seq(1,
        0,
        -11111111,
      ).flatMap(i => Seq(true, false).map((i, _))).flatMap(tup =>
        implementations.map(impl => impl -> TwoArgsBoolInt(tup._2, tup._1))): _*
    )
    forAll(simpleDoubleInputs) { (impl: Scala3ParseCaseClass, input: TwoArgsBoolInt) =>
      assert(impl.to[TwoArgsBoolInt](input.toString) === input)
    }

  }

}
