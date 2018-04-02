package com.github.aborg0.caseyclassy

import com.github.aborg0.caseyclassy.example.{SimpleBoolean, SimpleDouble, SimpleInt, SimpleObject}
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.FlatSpec
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}

class SimpleTests extends FlatSpec with TableDrivenPropertyChecks {
  val implementations = Table("implementation", RegexParseCaseClass)

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
}
