package io.github.antivanov.scala3.examples.contextual_abstractions

object ExtensionMethodsTypeClasses:
  // Type class definition: a trait in Scala 3 as in Scala 2
  trait Show[T]:
    def show(value: T): String

  // Utility to avoid writing summon[Show[T]]. and instead allow writing Show[T].
  object Show:
    def apply[T](using s: Show[T]): Show[T] =
      s

  class Cat(val name: String)

  /*
   * Type class instances
   */
  object ShowInstances:

    given catShow: Show[Cat] with
      def show(cat: Cat): String =
        s"cat named ${cat.name}"

    given listShow[T](using s: Show[T]): Show[List[T]] with
      def show(value: List[T]): String =
        value.map(s.show(_)).mkString(",")

    given optionShow[T](using s: Show[T]): Show[Option[T]] with
      def show(value: Option[T]): String =
        value.fold("")(s.show(_))

    extension [T](value: T)(using s: Show[T])
      def show: String =
        s.show(value)

@main def extensionMethodsTypeClassesMain: Unit =
  import ExtensionMethodsTypeClasses._
  import ExtensionMethodsTypeClasses.ShowInstances.given
  import ExtensionMethodsTypeClasses.ShowInstances._

  val tom = Cat("Tom")
  val felix = Cat("Felix")
  val garfield = Cat("Garfield")
  val catsList = List(tom, felix, garfield)
  val maybeCat: Option[Cat] = Some(tom)

  println(Show[Cat].show(tom))
  println(Show[List[Cat]].show(catsList))
  println(Show[Option[Cat]].show(maybeCat))

  println(tom.show)
  println(catsList.show)
  println(maybeCat.show)
