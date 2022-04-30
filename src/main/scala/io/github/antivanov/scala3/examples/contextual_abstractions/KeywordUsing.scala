package io.github.antivanov.scala3.examples.contextual_abstractions

import scala.math._

class Polynomial(coeffs: List[Double]):
  val coefficients = coeffs.dropWhile(_ == 0)
  val degree = coefficients.size - 1
  override def toString: String =
    s"Polynomial[${coefficients.toString}]"

case class LinearPolynomial(a: Double, b: Double) extends Polynomial(List(a, b))

case class QuadraticPolynomial(a: Double, b: Double, c: Double) extends Polynomial(List(a, b, c))

trait RootFindingAlgorithm[T <: Polynomial]:
  def findRoots(polynomial: T): List[Double]

given linearAlgorithm: RootFindingAlgorithm[LinearPolynomial] with
  def findRoots(polynomial: LinearPolynomial): List[Double] =
    val LinearPolynomial(a, b) = polynomial
    List(-b / a)

given quadraticAlgorithm: RootFindingAlgorithm[QuadraticPolynomial] with
  def findRoots(polynomial: QuadraticPolynomial): List[Double] =
    val QuadraticPolynomial(a, b, c) = polynomial
    val discriminant = b * b - 4 * a * c
    if discriminant >= 0 then
      val rootOfDiscriminant = Math.sqrt(discriminant)
      val roots = List(-b + rootOfDiscriminant, -b - rootOfDiscriminant).map(_ / (2 * a))
      roots.toSet.toList
    else
      List()

// In Scala 3 it is possible to use "using" inside "given"s similar how nested implicit resolution was handled in Scala 2
given registeredAlgorithms(using linearAlgorthm: RootFindingAlgorithm[LinearPolynomial], quadraticAlgorithm: RootFindingAlgorithm[QuadraticPolynomial]): RootFindingAlgorithm[LinearPolynomial | QuadraticPolynomial] with
  def findRoots(polynomial: LinearPolynomial | QuadraticPolynomial): List[Double] =
    polynomial match
      case p: QuadraticPolynomial =>
        quadraticAlgorithm.findRoots(p)
      case p: LinearPolynomial =>
        linearAlgorithm.findRoots(p)

// Requiring an algorithm to be defined for a particular polynomial type with "using"
def findRoots[T <: Polynomial](polynomial: T)(using algorithm: RootFindingAlgorithm[T]): List[Double] =
  algorithm.findRoots(polynomial)

@main def keywordUsingMain: Unit =
  val quadraticOneRoot = QuadraticPolynomial(1, -2, 1)
  val quadraticTwoRoots = QuadraticPolynomial(3, -15, 18)
  val quadraticNoRoots = QuadraticPolynomial(-1, -1, -1)
  val linearOneRoot = LinearPolynomial(2, 1) 
  val cubicOneRoot = Polynomial(List(1, -3, -3, 1))

  val polynomialsWithRegisteredAlgorithms: List[QuadraticPolynomial | LinearPolynomial] = List(quadraticOneRoot, quadraticTwoRoots, quadraticNoRoots, linearOneRoot)

  polynomialsWithRegisteredAlgorithms.foreach(p =>
    println(p)
    println(findRoots(p))
  )

  println(cubicOneRoot)
  // Would not compile: no registered algorithm
  //println(findRoots(cubicOneRoot))