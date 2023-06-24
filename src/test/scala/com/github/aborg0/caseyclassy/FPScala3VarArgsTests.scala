package com.github.aborg0.caseyclassy

import com.github.aborg0.caseyclassy.example.{OnlyVarArgs, StringPlusVarArgs}
import com.github.aborg0.caseyclassy.FastParseParse.given
import fastparse._
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor1, TableFor2}

class FPScala3VarArgsTests extends AnyWordSpec with TableDrivenPropertyChecks {
  val implementations: TableFor1[FastParseParse.type] = Table("implementation", FastParseParse)

  import FastParseParse._

  "ParseCaseClass for variable arity arguments successfully parse" when {
    "OnlyVarArgs" should {
      "has empty args" in {
        forAll(
          Table(("implementation", "OnlyVarArgs[Int]"), implementations.map(impl => impl -> OnlyVarArgs[Int]()): _*)) {
          (impl, input: OnlyVarArgs[Int]) =>
            assert(impl.to[OnlyVarArgs[Int]](input.toString) === input)
        }
      }
      "has one argument" in {
        val simpleVarArgsInputs = Table(
          ("implementation", "OnlyVarArgs[Int]"),
          Seq(1,
            0,
            -1,
            Int.MaxValue,
            Int.MinValue).flatMap(i =>
            implementations.map(impl => impl -> OnlyVarArgs[Int](i))): _*
        )
        forAll(simpleVarArgsInputs) { (impl, input: OnlyVarArgs[Int]) =>
          assert(impl.to[OnlyVarArgs[Int]](input.toString) === input)
        }
      }
      "has two arguments" in {
        val varArgsInputs = Table(
          ("implementation", "OnlyVarArgs[Int]"),
          Seq(1,
            0,
            -1,
            Int.MaxValue,
            Int.MinValue).flatMap(i =>
            implementations.map(impl => impl -> OnlyVarArgs[Int](i, -i))): _*
        )
        forAll(varArgsInputs) { (impl, input: OnlyVarArgs[Int]) =>
          assert(impl.to[OnlyVarArgs[Int]](input.toString) === input)
        }
      }
      "has three arguments" in {
        val varArgsInputs = Table(
          ("implementation", "OnlyVarArgs[Int]"),
          Seq(1,
            0,
            -1,
            Int.MaxValue,
            Int.MinValue).flatMap(i =>
            implementations.map(impl => impl -> OnlyVarArgs[Int](3, i, -i))): _*
        )
        forAll(varArgsInputs) { (impl, input: OnlyVarArgs[Int]) =>
          assert(impl.to[OnlyVarArgs[Int]](input.toString) === input)
        }
      }
    }
    "StringPlusVarArgs" should {
      "with String + no arguments" in {
        val simpleStringPlusVarArgs = Table(
          ("implementation", "StringPlusVarArgs[Short]"),
          Seq("", "Hello", "3").flatMap(s => implementations.map(impl => impl -> StringPlusVarArgs[Short](s))): _*
        )
        forAll(simpleStringPlusVarArgs) { (impl, input: StringPlusVarArgs[Short]) =>
          assert(impl.to[StringPlusVarArgs[Short]](input.toString) === input)
        }
      }
      "with String + one argument" in {
        val stringPlusVarArgs = Table(
          ("implementation", "StringPlusVarArgs[Short]"),
          Seq("", "Hello", "3").flatMap(s => Seq(1.toShort,
            0.toShort,
            (-1).toShort,
            Short.MaxValue,
            Short.MinValue).flatMap(i =>
            implementations.map(impl => impl -> StringPlusVarArgs(s, i)))): _*
        )
        forAll(stringPlusVarArgs) { (impl, input: StringPlusVarArgs[Short]) =>
          assert(impl.to[StringPlusVarArgs[Short]](input.toString) === input)
        }
      }
      "with String + two arguments" in {
        val stringPlusVarArgs = Table(
          ("implementation", "StringPlusVarArgs[Short]"),
          Seq("", "Hello", "3").flatMap(s => Seq(1.toShort,
            0.toShort,
            (-1).toShort,
            Short.MaxValue,
            Short.MinValue).flatMap(i =>
            implementations.map(impl => impl -> StringPlusVarArgs(s, i, 2.toShort)))): _*
        )
        forAll(stringPlusVarArgs) { (impl, input: StringPlusVarArgs[Short]) =>
          assert(impl.to[StringPlusVarArgs[Short]](input.toString) === input)
        }
      }
    }
  }
}
