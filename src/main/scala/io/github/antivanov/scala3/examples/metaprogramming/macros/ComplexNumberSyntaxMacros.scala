package io.github.antivanov.scala3.examples.metaprogramming.macros

import scala.quoted._
import scala.quoted.{Expr, Quotes, Type, Varargs}

object ComplexNumberSyntaxMacros:

  given ToExpr[ComplexNumberRectangularForm] with
    def apply(z: ComplexNumberRectangularForm)(using Quotes) =
      val x = Expr(z.x)
      val y = Expr(z.y)
      '{ ComplexNumberRectangularForm($x, $y) }

  val i: Double = Double.NegativeInfinity

  inline def complexNumber(inline z: Double): ComplexNumber =
    ${ asComplexNumber('z) }

  private def asComplexNumber(z: Expr[Double])(using Quotes): Expr[ComplexNumber] =
    import quotes.reflect.report
    z match
      case '{ ($x: Double) + i * ($y: Double) } =>
        val number = ComplexNumberRectangularForm(x.valueOrError, y.valueOrError)
        Expr(number)
      case _ =>
        report.error(s"Invalid complex number  ${z.show}");
        '{???}

end ComplexNumberSyntaxMacros
