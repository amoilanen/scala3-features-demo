package io.github.antivanov.scala3.examples.`new`.types

trait Node[A]
case class Leaf[A](value: A) extends Node[A]
case class Tree[A](left: Node[A], right: Node[A]) extends Node[A]

// "reduce" has a polymorphic function type
val reduce: [A] => ((A, A) => A) => (Node[A]) => A =
  [A] => (f: (A, A) => A) => (node: Node[A]) =>
    node match
      case Leaf(x) => x
      case Tree(left, right) => f(reduce(f)(left), reduce(f)(right))

// fully equivalent to the following declaration of reduce as a method
/*
def reduce[A](f: (A, A) => A)(node: Node[A]): A =
  node match
    case Leaf(x) => x
    case Tree(left, right) => f(reduce(f)(left), reduce(f)(right))
*/

@main def polymorphicFunctionTypesMain: Unit =
  val treeSummer = reduce[Int](_ + _)
  val tree = Tree(
    Tree(
      Leaf(1),
      Tree(
        Leaf(2),
        Leaf(3))),
    Leaf(4))
  println(treeSummer(tree))
