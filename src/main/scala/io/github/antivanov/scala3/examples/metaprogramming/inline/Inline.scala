package io.github.antivanov.scala3.examples.metaprogramming.inline

import scala.annotation.tailrec

inline def testRegexp(regexp: Regex, matchesInputs: Seq[String], doesNotMatchInputs: Seq[String]): Unit =
  matchesInputs.foreach(input =>
    assert(regexp.matches(input), s"'$regexp' should match '$input'")
  )
  doesNotMatchInputs.foreach(input =>
    assert(!regexp.matches(input), s"'$regexp' should not match '$input'")
  )

@main def inlineMain: Unit =
  //a
  val singleCharRegex = Character('a')
  testRegexp(
    singleCharRegex,
    matchesInputs = Seq("a"),
    doesNotMatchInputs = Seq("b", "ab")
  )

  //abc
  val multipleCharRegex = Sequence(
    Character('a'),
    Character('b'),
    Character('c')
  )
  testRegexp(
    multipleCharRegex,
    matchesInputs = Seq("abc"),
    doesNotMatchInputs = Seq("ab", "d", "abcd")
  )

  //a+b
  val plusQuantifierRegex = Sequence(
    PlusQuantifier(
      Character('a')
    ),
    Character('b')
  )
  testRegexp(
    plusQuantifierRegex,
    matchesInputs = Seq("aaab", "ab"),
    doesNotMatchInputs = Seq("b", "ac")
  )

  //(:?ab)+c
  val plusQuantifierOverSequenceRegex = Sequence(
    PlusQuantifier(
      Sequence(
        Character('a'),
        Character('b')
      )
    ),
    Character('c')
  )
  testRegexp(
    plusQuantifierOverSequenceRegex,
    matchesInputs = Seq("abc", "ababc", "abababc"),
    doesNotMatchInputs = Seq("c", "ac", "acacb", "ababcd", "d")
  )

  //(:?ab)*c
  val asteriskQuantifierOverSequenceRegex = Sequence(
    AsteriskQuantifier(
      Sequence(
        Character('a'),
        Character('b')
      )
    ),
    Character('c')
  )
  testRegexp(
    asteriskQuantifierOverSequenceRegex,
    matchesInputs = Seq("c", "abc", "ababc", "abababc"),
    doesNotMatchInputs = Seq("ac", "acacb", "ababcd", "d")
  )

  //a.c
  val dotRegex = Sequence(
    Character('a'),
    Dot,
    Character('c')
  )
  testRegexp(
    dotRegex,
    matchesInputs = Seq("abc", "adc"),
    doesNotMatchInputs = Seq("ac", "abb", "abcd")
  )

  //(:?ab)+c*d.f
  val allConstructsRegex = Sequence(
    PlusQuantifier(
      Sequence(
        Character('a'),
        Character('b')
      )
    ),
    AsteriskQuantifier(
      Character('c')
    ),
    Character('d'),
    Dot,
    Character('f')
  )
  testRegexp(
    allConstructsRegex,
    matchesInputs = Seq("abcdef", "ababcdef", "abddf"),
    doesNotMatchInputs = Seq("cdef", "abcdefa", "abcaef")
  )