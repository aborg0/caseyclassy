package com.github.aborg0.caseyclassy

import com.github.aborg0.caseyclassy.example.TwoArgsBoolInt
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.FlatSpec
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}

class TwoArgsTests  extends FlatSpec with TableDrivenPropertyChecks {
    val implementations = Table("implementation", RegexParseCaseClass)

    behavior of "ParseCaseClass for two args cases"

    import RegexParseCaseClass._

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
