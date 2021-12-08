package io.github.antivanov.scala3.examples.enums

import scala.math._

/*
 * In Scala 3 it is possible to implement ADTs using enums.
 */
enum Tree[+T]:
  case Empty
  case Leaf(value: T)
  case Node(left: Tree[T], right: Tree[T])

import Tree._

def depth[T](tree: Tree[T]): Int =
  tree match
    case Empty => 0
    case Leaf(_) => 1
    case Node(left, right) => 1 + max(depth(left), depth(right))

@main def alegbraicDataTypes: Unit =
  val tree = Node(
    Node(
      Leaf(1),
      Empty
    ),
    Node(
      Node(
        Leaf(2),
        Leaf(3)
      ),
      Leaf(4)
    )
  )
  println(depth(tree)) // 4
