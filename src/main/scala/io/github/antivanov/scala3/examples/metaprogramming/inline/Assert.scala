package io.github.antivanov.scala3.examples.metaprogramming.inline

object Assert:
  case class AssertionFailedException[T](expected: T, actual: T, message: String)
    extends RuntimeException(s"$message, expected $expected but was $actual")

  transparent inline def assertEquals[T](expected: T, actual: T, message: String = "Not equal"): Unit =
    if (expected != actual)
      throw new AssertionFailedException(expected, actual, message)

  transparent inline def assert(value: Boolean, message: String = "Not true"): Unit =
    assertEquals( true, value, message)


