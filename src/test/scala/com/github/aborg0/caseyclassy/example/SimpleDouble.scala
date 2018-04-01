package com.github.aborg0.caseyclassy.example

final case class SimpleDouble(d: Double) {
  override def equals(obj: scala.Any): Boolean = obj match {
    case SimpleDouble(o) => (d == o) || (d.isNaN && o.isNaN)
    case _ => false
  }
}