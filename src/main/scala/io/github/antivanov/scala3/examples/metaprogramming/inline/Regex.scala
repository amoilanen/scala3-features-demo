package io.github.antivanov.scala3.examples.metaprogramming.inline

import scala.annotation.tailrec

object Regex

  /*
   * Simplistic regular expressions which support letters, dot and modifiers + and *, for example
   * (:?ab)+c*d.
   */
  trait Regex:
    def matches(input: String): Boolean =
      matchAt(input, 0) == input.length

    def matchAt(input: String, position: Int): Int

    protected inline final def withinInputLimits(input: String, position: Int): Boolean =
      (position >= 0) && (position < input.length)

  case class Sequence(regexps: Regex*) extends Regex:
    override inline def matchAt(input: String, position: Int): Int =
      regexps.foldLeft(position)((currentPosition, regexp) =>
        currentPosition match
          case -1 => -1
          case _ => regexp.matchAt(input, currentPosition)
      )

  case class Character(ch: Char) extends Regex:
    override inline def matchAt(input: String, position: Int): Int =
      if (withinInputLimits(input, position) && (input.charAt(position) == ch))
        position + 1
      else -1

  object Dot extends Regex:
    override inline def matchAt(input: String, position: Int): Int =
      if (withinInputLimits(input, position))
        position + 1
      else -1

  trait Quantifier extends Regex:
    val regex: Regex

    protected inline final def tryMatchAt(input: String, position: Int): Int =
      var lastMatch = position
      var newMatch = regex.matchAt(input, position)
      while (newMatch != -1 && newMatch != lastMatch)
        lastMatch = newMatch
        newMatch = regex.matchAt(input, lastMatch)
      lastMatch

    // Cannot use "inline" with tailrec methods, the following will not work:
    //@tailrec
    //protected inline final def tryMatchAt(input: String, position: Int): Int =
    //  val lastMatch = regex.matchAt(input, position)
    //  if (lastMatch == -1)
    //    position
    //  else
    //    tryMatchAt(input, lastMatch)

  case class PlusQuantifier(regex: Regex) extends Quantifier:
    override inline def matchAt(input: String, position: Int): Int =
      val lastMatch = tryMatchAt(input, position)
      if (lastMatch == position)
        -1
      else
        lastMatch

  case class AsteriskQuantifier(regex: Regex) extends Quantifier:
    override inline def matchAt(input: String, position: Int): Int =
      tryMatchAt(input, position)