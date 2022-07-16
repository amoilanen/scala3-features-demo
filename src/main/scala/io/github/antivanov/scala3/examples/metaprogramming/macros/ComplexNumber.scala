package io.github.antivanov.scala3.examples.metaprogramming.macros

import scala.annotation.targetName

trait ComplexNumber:
  def toRectangular: ComplexNumber
  def toPolar: ComplexNumber

  override def toString: String =
    val ComplexNumberRectangularForm(x1, y1) = this.toRectangular
    s"$x1 + $y1 * i"

  @targetName("add")
  def +(z: ComplexNumber): ComplexNumber =
    val ComplexNumberRectangularForm(x1, y1) = this.toRectangular
    val ComplexNumberRectangularForm(x2, y2) = z.toRectangular
    ComplexNumberRectangularForm(x1 + x2, y1 + y2)

  @targetName("subtract")
  def -(z: ComplexNumber): ComplexNumber =
    val ComplexNumberRectangularForm(x1, y1) = this.toRectangular
    val ComplexNumberRectangularForm(x2, y2) = z.toRectangular
    ComplexNumberRectangularForm(x1 - x2, y1 - y2)

  @targetName("mul")
  def *(z: ComplexNumber): ComplexNumber =
    val ComplexNumberPolarForm(r1, phi1) = this.toPolar
    val ComplexNumberPolarForm(r2, phi2) = z.toPolar
    ComplexNumberPolarForm(r1 * r2, phi1 + phi2)

  @targetName("div")
  def /(z: ComplexNumber): ComplexNumber =
    val ComplexNumberPolarForm(r1, phi1) = this.toPolar
    val ComplexNumberPolarForm(r2, phi2) = z.toPolar
    ComplexNumberPolarForm(r1 / r2, phi1 - phi2)

case class ComplexNumberPolarForm(r: Double, phi: Double) extends ComplexNumber:
  override def toRectangular: ComplexNumber =
    ComplexNumberRectangularForm(r * Math.cos(phi), r * Math.sin(phi))

  override def toPolar: ComplexNumber = this

case class ComplexNumberRectangularForm(x: Double, y: Double) extends ComplexNumber:
  override def toRectangular: ComplexNumber = this

  override def toPolar: ComplexNumber =
    val r = Math.sqrt(x * x + y * y)
    val atan = Math.atan(y / x)
    val phi = if (x >= 0)
      atan
    else
      Math.PI + atan
    ComplexNumberPolarForm(r, phi)

