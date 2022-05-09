package io.github.antivanov.scala3.examples.contextual_abstractions

import scala.util.{NotGiven, Try}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*

object KeywordGivenNegatedGivenMain:

  class ToEitherError(cause: Throwable) extends Exception(cause)

  trait KnownMonad[A]

  given knownFuture[A]: KnownMonad[Future[A]]()
  given knownEither[A]: KnownMonad[Either[Throwable, A]]()
  given knownOption[A]: KnownMonad[Option[A]]()

  def toEither[A](v: => A)(using NotGiven[KnownMonad[A]]): Either[Throwable, A] =
    Try(v).toEither.left.map(ToEitherError(_))

  def toEither[A](v: Future[A], timeout: Duration)(using ExecutionContext): Either[Throwable, A] =
    toEither(Await.result(v, timeout))

  def toEither[A, B](v: Either[A, B]): Either[Throwable, B] =
    v.left.map(_ => ToEitherError(new RuntimeException(s"Encountered $v")))

  def toEither[A](v: Option[A]): Either[Throwable, A] =
    v match {
      case None => Left(ToEitherError(new Exception("Encountered None")))
      case Some(v) => Right(v)
    }

@main def keywordGivenNegatedGivenMain: Unit =
  import KeywordGivenNegatedGivenMain._

  given ec: ExecutionContext = ExecutionContext.Implicits.global

  def throwingFunction(errorMessage: String): String =
    throw new RuntimeException(errorMessage)

  println(toEither("plainValueSuccess"))
  println(toEither(throwingFunction("plainValueFailure")))
  println(toEither(Future.successful("futureValueSuccess"), 100.millis))
  println(toEither(Future {
    throwingFunction("futureValueFailure")
  }, 100.millis))
  println(toEither(Right("eitherValueSuccess")))
  println(toEither(Left("eitherValueFailure")))
  println(toEither(Some("optionValueSuccess")))
  println(toEither(None))