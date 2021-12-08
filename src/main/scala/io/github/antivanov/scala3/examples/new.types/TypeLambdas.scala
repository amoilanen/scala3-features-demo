package io.github.antivanov.scala3.examples.`new.types`

// Relation with the parameterized type definitions: parameterized type definition is a shorthand for a type lambda
type Function1 = [A, B] =>> A => B
// same as parameterized type
// type Function1[A, B] = A => B
type Function2 = [A, B, C] =>> (A, B) => C

type Result = [A] =>> Either[Throwable, A]
type ResultFunction2 = [A, B, C] =>> Function2[A, B, Result[C]]

@main def typeLambdasMain: Unit =
  def increment(x: Int): Int =
    x + 1
  
  def divide(x: Double, y: Double): Either[Throwable, Double] =
    if y == 0 then
      Left(IllegalArgumentException("Cannot divide by zero"))
    else
      Right(x / y)

  val f: Function1[Int, Int] = increment
  println(f(1))
  val g: ResultFunction2[Double, Double, Double] = divide
  println(g(6, 2))
