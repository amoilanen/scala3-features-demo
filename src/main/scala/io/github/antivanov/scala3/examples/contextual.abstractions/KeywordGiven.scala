package io.github.antivanov.scala3.examples.contextual.abstractions

trait Show[T]:
  def show(value: T): String

class Cat(val name: String)

given stringShow: Show[String] with
  def show(value: String): String =
    value

given catShow: Show[Cat] with
  def show(cat: Cat): String =
    s"cat named ${cat.name}"

@main def keywordGivenMain: Unit =
  val tom = Cat("Tom")
  val catShow = summon[Show[Cat]]
  println(catShow.show(tom))