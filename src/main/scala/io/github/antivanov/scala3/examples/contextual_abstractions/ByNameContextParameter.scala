package io.github.antivanov.scala3.examples.contextual_abstractions

object ByNameContextParameter:

  trait Show[T]:
    def show(value: T): String

  object Show:
    def apply[T](using Show[T]): Show[T] =
      summon[Show[T]]

  given stringShow: Show[String] =
    identity[String]

  given intShow: Show[Int] =
    _.toString

  given exceptionShow: Show[Exception] =
    _.getMessage

  given eitherShow[A, B](using shA: => Show[A], shB: => Show[B]): Show[Either[A, B]] with
    def show(value: Either[A, B]): String = value match
        case Left(left) => shA.show(left)
        case Right(right) => shB.show(right)

@main def byNameContextParameterMain: Unit =
  import ByNameContextParameter._

  val value: Either[Exception, Int] = Right(2)

  /*
   * Although context parameter shA is evaluated "on demand" it still needs to be defined
   * and is evaluated at the latest in the body of `eitherShow` => we need to define Show[Exception]
   * even for a Right value
   */
  print(Show[Either[Exception, Int]].show(value))