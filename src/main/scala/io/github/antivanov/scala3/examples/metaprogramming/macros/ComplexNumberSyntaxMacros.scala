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

  /*
   * Just an illustration/idea of the macros, a better implementation would also correctly parse
   * 1 - i; 2 * i + 3; i * 2 + 3; i; -i; etc.
   * For this it would be necessary to parse the expression and convert only i to the ComplexNumber, plus define
   * the methods for operations between the ordinary numbers and complex numbers
   */
  private def asComplexNumber(z: Expr[Double])(using Quotes): Expr[ComplexNumber] =
    import quotes.reflect.report
    z match
      case '{ ($x: Int) + i * ($y: Int) } =>
        toComplexNumber[Int](x, y)
      case '{ ($x: Float) + i * ($y: Float) } =>
        toComplexNumber[Float](x, y)
      case '{ ($x: Double) + i * ($y: Double) } =>
        toComplexNumber[Double](x, y)
      case '{ ($x: Int) - i * ($y: Int) } =>
        toComplexNumber[Int](x, '{-${y}})
      case '{ ($x: Float) - i * ($y: Float) } =>
        toComplexNumber[Float](x, '{-${y}})
      case '{ ($x: Double) - i * ($y: Double) } =>
        toComplexNumber[Double](x, '{-${y}})
      case _ =>
        report.error(s"Invalid complex number definition: ${z.show}");
        '{???}

  private def toComplexNumber[T](x: Expr[T], y: Expr[T])(using n: Numeric[T])(using Quotes, FromExpr[T]): Expr[ComplexNumber] =
    val number = ComplexNumberRectangularForm(n.toDouble(x.valueOrAbort), n.toDouble(y.valueOrAbort))
    Expr(number)

end ComplexNumberSyntaxMacros
