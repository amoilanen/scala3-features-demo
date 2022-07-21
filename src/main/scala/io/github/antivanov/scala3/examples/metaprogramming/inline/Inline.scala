package io.github.antivanov.scala3.examples.metaprogramming.inline

import scala.annotation.tailrec

/*
 * Simplistic regexps which supports letters, dot and modifiers + and *, for example
 * (:?ab)+c*d.
 */
trait Regex:
  def matches(input: String): Boolean =
    matchAt(input, 0) == input.length

  def matchAt(input: String, position: Int): Int

  protected final def withinInputLimits(input: String, position: Int): Boolean =
    (position >= 0) && (position < input.length)

case class Character(ch: Char) extends Regex:
  def matchAt(input: String, position: Int): Int =
    if (withinInputLimits(input, position) && (input.charAt(position) == ch))
      position + 1
    else -1

object Dot extends Regex:
  def matchAt(input: String, position: Int): Int =
    if (withinInputLimits(input, position))
      position + 1
    else -1

trait Quantifier extends Regex:
  val regex: Regex

  @tailrec
  protected final def tryMatchAt(input: String, position: Int): Int =
    val lastMatch = regex.matchAt(input, position)
    if (lastMatch == -1)
      position
    else
      tryMatchAt(input, lastMatch)

case class PlusQuantifier(regex: Regex) extends Quantifier:
  def matchAt(input: String, position: Int): Int =
    val lastMatch = tryMatchAt(input, position)
    if (lastMatch == position)
      -1
    else
      lastMatch

case class AsteriskQuantifier(regex: Regex) extends Quantifier:
  def matchAt(input: String, position: Int): Int =
    tryMatchAt(input, position)

case class AssertionFailedException[T](expected: T, actual: T, message: String)
  extends RuntimeException(s"$message, expected $expected but encountered $actual")

transparent inline def assertEquals[T](expected: T, actual: T, message: String = "Not equal"): Unit =
  if (expected != actual)
    throw new AssertionFailedException(expected, actual, message)

transparent inline def assert(value: Boolean, message: String = "Not true"): Unit =
  assertEquals( true, value, message)

@main def inlineMain: Unit =
  val singleCharRegex = Character('a')
  assert(singleCharRegex.matches("a"))
  assert(!singleCharRegex.matches("b"))
  assert(!singleCharRegex.matches("ab"))
  //TODO: Test the following regexps
  //a
  //abc
  //a+b
  //(:?ab)+c
  //(:?ab)*c
  //a.c
  //(:?ab)+c*d.
