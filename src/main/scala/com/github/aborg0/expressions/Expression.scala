package com.github.aborg0.expressions

import fastparse.NoWhitespace._
import fastparse._
object Expression extends App {
  def number[_: P]: P[Int] = P(CharIn("0-9").rep(1).!.map(_.toInt))
  def parens[_: P]: P[Int] = P("(" ~/ addSub ~ ")")
  def atom[_: P]: P[Int] = P(number | parens)
  def factor[_: P]: P[Int] = P(atom ~ ("^" ~ factor).?).map {
    case (base, Some(exponent)) => math.pow(base, exponent).toInt
    case (v, None)              => v
  }

  def divMul[_: P]: P[Int] = P(factor ~ (CharIn("*/").! ~/ factor).rep).map {
    case (l, opAndRests) =>
      opAndRests.foldLeft(l) {
        case (acc, ("*", right)) => acc * right
        case (acc, ("/", right)) => acc / right
      }
  }
  def addSub[_: P]: P[Int] =
    P(divMul ~ (CharIn("+\\-").! ~/ divMul).rep).map {
      case (l, opAndRests) =>
        opAndRests.foldLeft(l) {
          case (acc, ("+", right)) => acc + right
          case (acc, ("-", right)) => acc - right
        }
    }
  def expr[_: P]: P[Int] = P(addSub ~ End)

  println(parse("2+3*4", expr(_)))
  println(parse("2^3^2", expr(_)))
  println(parse("2^3^(1+1)", expr(_)))
  println(parse("2^3^1+1", expr(_)))
  println(parse("2^3*2+1", expr(_)))
}
