package io.github.antivanov.scala3.examples.metaprogramming.macros

@main def macrosUsageMain: Unit =
  import ComplexNumberSyntaxMacros.*
  val z = complexNumber {
    1 + i * 2
  }
  val z1 = ComplexNumberRectangularForm(1, 2)
  val z2 = ComplexNumberRectangularForm(3, 4)
  val z3 = z1 * z2
  println(z3.toString)