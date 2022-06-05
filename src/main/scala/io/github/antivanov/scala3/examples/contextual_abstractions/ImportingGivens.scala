package io.github.antivanov.scala3.examples.contextual_abstractions

object ImportingGivens:

  trait Show[T]:
    def show(value: T): String

  class Cat(val name: String)

  object ShowInstances:
    given catShow: Show[Cat] with
      def show(cat: Cat): String =
        s"cat named ${cat.name}"

    given listShow[T: Show]: Show[List[T]] with
      def show(values: List[T]): String =
        values.map(summon[Show[T]].show(_)).mkString(",")

    extension [T: Show](value: T)
      def show: String =
        summon[Show[T]].show(value)

@main def importingGivensMain: Unit =
  import ImportingGivens._
  import ImportingGivens.ShowInstances._

  // Givens should be imported separately in Scala 3
  import ShowInstances.given Show[Cat]
  import ShowInstances.given Show[List[?]]

  // Alternatively all of the givens can be imported at once
  //import ImportingGivens.ShowInstances.given

  val cats = List(Cat("Tom"), Cat("Garfield"), Cat("Felix"))
  println(cats.head.show)
  println(cats.show)