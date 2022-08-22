package io.github.antivanov.scala3.examples.other

import io.github.antivanov.scala3.examples.enums.Tree

object TraitParameters:
  trait Animal(name: String):
    def saySomething: String

  class Cat(name: String) extends Animal(name):
    override def saySomething: String =
      "purr"

  class Dog(name: String) extends Animal(name):
    override def saySomething: String =
      "woof-woof"

@main def traitParametersMain: Unit =
  import TraitParameters._
  val animals = Seq(Dog("Buddy"), Cat("Leo"), Dog("Rocky"))
  animals.foreach(animal => println(animal.saySomething))
