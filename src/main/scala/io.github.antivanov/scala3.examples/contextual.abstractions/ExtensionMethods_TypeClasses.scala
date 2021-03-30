package io.github.antivanov.scala3.examples.contextual_abstractions

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
    extension (cat: Cat)(using s: Show[Cat])
      def show: String =
        summon[Show[Cat]].show(cat)

  given listShow[T](using s: Show[T]): Show[List[T]] with
    def show(value: List[T]): String =
      value.map(s.show(_)).mkString(",")
    extension (value: List[T])(using s: Show[List[T]])
      def show: String =
        summon[Show[List[T]]].show(value)

  given optionShow[T](using s: Show[T]): Show[Option[T]] with
    def show(value: Option[T]): String =
      value.fold("")(s.show(_))
    extension (value: Option[T])(using s: Show[Option[T]])
      def show: String =
        summon[Show[Option[T]]].show(value)

@main def KeywordGiven: Unit =
  import ShowInstances.given

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
