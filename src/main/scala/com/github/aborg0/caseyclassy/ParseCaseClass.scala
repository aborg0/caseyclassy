package com.github.aborg0.caseyclassy

trait ParseCaseClass {
  def to[A <: AnyRef](input: String): A
}
