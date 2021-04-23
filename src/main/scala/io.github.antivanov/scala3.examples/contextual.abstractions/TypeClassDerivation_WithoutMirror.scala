package io.github.antivanov.scala3.examples.contextual_abstractions

trait Show[T]:
  def show(value: T): String

object Show:
  def apply[T](using s: Show[T]): Show[T] =
    s

trait Print[T]:
  def print(value: T): Unit

object Print:
  def apply[T](using p: Print[T]): Print[T] =
    p

  // derived might be also used to define one typeclass in terms of other typeclasses defined for the same type
  inline given derived[T](using s: Show[T]): Print[T] =
    new Print:
      def print(value: T): Unit =
        println(s.show(value))

class Cat(val name: String)

object ShowInstances:

  given catShow: Show[Cat] with
    def show(cat: Cat): String =
      s"cat named ${cat.name}"

  given catPrint: Print[Cat] = Print.derived

  given listShow[T](using s: Show[T]): Show[List[T]] with
    def show(value: List[T]): String =
      value.map(s.show(_)).mkString(",")

  given listPrint[T](using s: Show[T]): Print[List[T]] = Print.derived

  given optionShow[T](using s: Show[T]): Show[Option[T]] with
    def show(value: Option[T]): String =
      value.fold("")(s.show(_))

  given optionPrint[T](using s: Show[T]): Print[Option[T]] = Print.derived

  extension [T](value: T)(using s: Show[T])
    def show: String =
      s.show(value)

  extension [T](value: T)(using p: Print[T])
    def print: Unit =
      p.print(value)

object PrintInstances:

  import ShowInstances.given

  given catPrint: Print[Cat] = Print.derived

  given listPrint[T](using s: Show[T]): Print[List[T]] = Print.derived

  given optionPrint[T](using s: Show[T]): Print[Option[T]] = Print.derived
  
  extension [T](value: T)(using p: Print[T])
    def print: Unit =
      p.print(value)

@main def KeywordGiven: Unit =
  import ShowInstances.given
  import PrintInstances.given
  import PrintInstances._

  val tom = Cat("Tom")
  val felix = Cat("Felix")
  val garfield = Cat("Garfield")
  val catsList = List(tom, felix, garfield)
  val maybeCat: Option[Cat] = Some(tom)

  Print[Cat].print(tom)
  Print[List[Cat]].print(catsList)
  Print[Option[Cat]].print(maybeCat)

  tom.print
  catsList.print
  maybeCat.print
