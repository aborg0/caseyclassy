package com.github.aborg0.caseyclassy

import java.time.LocalDate

import com.github.aborg0.caseyclassy.example.{SimpleBoolean, SimpleDouble, SimpleInt, SimpleObject}
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.FlatSpec
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor1, TableFor2}

class SimpleTests extends FlatSpec with TableDrivenPropertyChecks {
  val implementations: TableFor1[ParseCaseClass] = Table("implementation", RegexParseCaseClass)

  behavior of "ParseCaseClass for simple cases"

  import RegexParseCaseClass._

  it should "parse SimpleDouble" in {
    val simpleDoubleInputs: TableFor2[ParseCaseClass, SimpleDouble] = Table(
      ("implementation", "SimpleDouble"),
      Seq(1d,
        0d,
        .25,
        -2.5,
        -0d,
        Double.MaxValue,
        Double.NaN,
        Double.NegativeInfinity,
        Double.PositiveInfinity).flatMap(d =>
        implementations.map(impl => impl -> SimpleDouble(d))): _*
    )
    forAll(simpleDoubleInputs) { (impl: ParseCaseClass, input: SimpleDouble) =>
      assert(impl.to[SimpleDouble](input.toString) === input)
    }
  }
  it should "parse SimpleInt" in {
    val simpleIntInputs: TableFor2[ParseCaseClass, SimpleInt] = Table(
      ("implementation", "SimpleInt"),
      Seq(1, 0, 2, -5, -10, Int.MaxValue, Int.MinValue).flatMap(i =>
        implementations.map(impl => impl -> SimpleInt(i))): _*
    )
    forAll(simpleIntInputs) { (impl: ParseCaseClass, input: SimpleInt) =>
      assert(impl.to[SimpleInt](input.toString) === input)
    }
  }
  it should "parse SimpleBoolean" in {
    val simpleIntInputs: TableFor2[ParseCaseClass, SimpleBoolean] = Table(
      ("implementation", "SimpleBoolean"),
      Seq(false, true).flatMap(b =>
        implementations.map(impl => impl -> SimpleBoolean(b))): _*
    )
    forAll(simpleIntInputs) { (impl: ParseCaseClass, input: SimpleBoolean) =>
      assert(impl.to[SimpleBoolean](input.toString) === input)
    }
  }
  it should "parse SimpleObject" in {
    forAll(implementations) { impl => assert(impl.to[SimpleObject.type](SimpleObject.toString) === SimpleObject) }
  }
  it should "parse options" in {
    val options = Table(("implementation", "option"), (for {opt <- Seq(None, Some(4))
                                                            impl <- implementations} yield impl -> opt): _*)
    forAll(options) { (impl, input) => assert(impl.to[Option[Int]](input.toString) === input) }
  }
  it should "parse eithers" in {
    val options = Table(("implementation", "either"), (for {either <- Seq(Left(LocalDate.of(2018, 4, 2)), Right(3.14159f))
                                                            impl <- implementations} yield impl -> either): _*)
    forAll(options) { (impl, input) => assert(impl.to[Either[LocalDate, Float]](input.toString) === input) }
  }
  it should "parse Tuple1s" in {
    val options = Table(("implementation", "tuple1"), (for {tup1 <- Seq(Tuple1(Some(2)), Tuple1(None))
                                                            impl <- implementations} yield impl -> tup1): _*)
    forAll(options) { (impl, input) => assert(impl.to[Tuple1[Option[Int]]](input.toString) === input) }
  }

  "RegexParseCaseClass" should "support reuse" in {
    val simpleBooleanParser = RegexParseCaseClass[SimpleBoolean]
    assert(simpleBooleanParser.parse("SimpleBoolean(false)") === SimpleBoolean(false))
    assert(simpleBooleanParser.parse("SimpleBoolean(true)") === SimpleBoolean(true))
  }
}
