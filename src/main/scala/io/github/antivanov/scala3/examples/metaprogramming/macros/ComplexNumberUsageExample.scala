package io.github.antivanov.scala3.examples.metaprogramming.macros

@main def macrosUsageMain: Unit =
  import ComplexNumberSyntaxMacros.*
  val z1 = complexNumber {
    1 + i * 2
  }
  val z2 = complexNumber {
    3 + i * 4
  }
  val z3 = z1 * z2
  println(z3.toString)