package io.github.antivanov.scala3.examples.contextual_abstractions

import scala.language.strictEquality

object MapEquality:

  /*
   * Specifying that Maps can be compared with == and != only
   * if both their key types are comparable and their value types are comparable
   */
  given [K1, V1, K2, V2](using CanEqual[K1, K2], CanEqual[V1, V2]): CanEqual[Map[K1, V1], Map[K2, V2]] =
    CanEqual.derived

object ClassHierarchyComparisonExample:
  trait Fruit

  class Apple extends Fruit derives CanEqual
  class Orange extends Fruit // derives CanEqual
  // Another alternative to using "derives":
  given CanEqual[Orange, Orange] = CanEqual.derived

  class GrannySmithApple extends Apple
  class IdaRedApple extends Apple
  class NavelOrange extends Orange
  class Clementine extends Orange

@main
def multiversalEqualityMain: Unit =
  import MapEquality.given
  val m1: Map[String, String] = Map(
    "key1" -> "value1",
  )
  val m2: Map[String, Int] = Map(
    "key1" -> 1
  )
  val m3: Map[String, String] = Map(
    "key1" -> "value31"
  )
  println(m1 == m3)
  // Not allowed to compare maps which have incomparable value types: String and Int
  // println(m1 == m2)

  import ClassHierarchyComparisonExample.{GrannySmithApple, IdaRedApple, NavelOrange, Clementine}
  val apple1 = GrannySmithApple()
  val apple2 = IdaRedApple()
  val orange1 = NavelOrange()
  val orange2 = Clementine()
  println(apple1 == apple1)
  println(apple1 != apple2)
  println(orange1 == orange1)
  println(orange1 != orange2)
  // Cannot compare Apples with Oranges since no CanEqual[Apple, Orange] is defined
  // println(apple1 != orange1)