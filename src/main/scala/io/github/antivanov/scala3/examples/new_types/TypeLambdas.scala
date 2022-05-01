package io.github.antivanov.scala3.examples.new_types

import scala.util.{Failure, Success, Try}

object TypeLambdas:
  type Result = [T] =>> Either[Throwable, T]
  def getResult[T](value: => T): Result[T] =
    Try(value) match {
      case Success(result) => Right(result)
      case Failure(error) => Left(error)
    }

@main def typeLambdasMain: Unit =
  import TypeLambdas._
  val result = getResult(1 / 0)
  println(result)