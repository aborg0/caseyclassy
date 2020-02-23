package com.github.aborg0.expressions

import fastparse._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.wordspec.AnyWordSpec

class ExpressionTest extends AnyWordSpec with TableDrivenPropertyChecks {
  val examples = Table[String, Int](("input", "result"),
    ("2+3*4", 14),
    ("2^3^2", 512),
    ("2^3^(1+1)", 512),
    ("2^3^1+1", 9),
    ("2^3*2+1", 17),
  )
  "Expression" should {
    examples.forEvery {
      case (input, expected) =>
        s"evaluate with expr according to the example: $input=$expected" in {
          assert(parse(input, Expression.expr(_)).get.value === expected)
        }
    }
  }
}
