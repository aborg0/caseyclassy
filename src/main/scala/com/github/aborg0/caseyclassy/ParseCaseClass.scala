package com.github.aborg0.caseyclassy

import shapeless.Lazy

trait ParseCaseClass {
  def to[A /*<: AnyRef*/](input: String)(implicit parse: Lazy[Parse[A]]): A
}

trait Parse[A] {
  def parse(input: String): A
}
