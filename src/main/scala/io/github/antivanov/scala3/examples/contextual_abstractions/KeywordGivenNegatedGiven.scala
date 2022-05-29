package io.github.antivanov.scala3.examples.contextual_abstractions

import scala.util.{NotGiven, Try}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*

object KeywordGivenNegatedGiven:

  class ToEitherError(cause: Throwable) extends Exception(cause)

  type Result[A] = Either[Throwable, A]

  trait KnownMonad[A]

  given knownFuture[A]: KnownMonad[Future[A]]()
  given knownEither[A]: KnownMonad[Either[Throwable, A]]()
  given knownOption[A]: KnownMonad[Option[A]]()

  def toResult[A](v: => A)(using NotGiven[KnownMonad[A]]): Result[A] =
    Try(v).toEither.left.map(ToEitherError(_))

  def toResult[A](v: Future[A], timeout: Duration)(using ExecutionContext): Result[A] =
    toResult(Await.result(v, timeout))

  def toResult[A, B](v: Either[A, B]): Result[B] =
    v.left.map(_ => ToEitherError(new RuntimeException(s"Encountered $v")))

  def toResult[A](v: Option[A]): Result[A] =
    v match {
      case None => Left(ToEitherError(new Exception("Encountered None")))
      case Some(v) => Right(v)
    }

object KeywordGivenNegatedGivenExtensions:

  import KeywordGivenNegatedGiven._

  extension[A] (f: Future[A])
    def asResult(timeout: Duration)(using ExecutionContext): Result[A] =
      toResult(f, timeout)

  extension[A] (v: => A)(using NotGiven[KnownMonad[A]])
    def asResult: Result[A] =
      toResult(v)

  extension[A, B] (v: Either[A, B])
    def asResult: Result[B] =
      toResult(v)

  extension[A] (v: Option[A])
    def asResult: Result[A] =
      toResult(v)

@main def keywordGivenNegatedGivenMain: Unit =
  import KeywordGivenNegatedGiven._

  given ec: ExecutionContext = ExecutionContext.Implicits.global

  def throwingFunction(errorMessage: String): String =
    throw new RuntimeException(errorMessage)

  println(toResult("plainValueSuccess"))
  println(toResult(throwingFunction("plainValueFailure")))
  println(toResult(Future.successful("futureValueSuccess"), 100.millis))
  println(toResult(Future {
    throwingFunction("futureValueFailure")
  }, 100.millis))
  println(toResult(Right("eitherValueSuccess")))
  println(toResult(Left("eitherValueFailure")))
  println(toResult(Some("optionValueSuccess")))
  println(toResult(None))

  import KeywordGivenNegatedGivenExtensions._

  println("plainValueSuccess".asResult)
  println(throwingFunction("plainValueFailure").asResult)
  println(Future.successful("futureValueSuccess").asResult(100.millis))
  println(Future {
    throwingFunction("futureValueFailure")
  }.asResult(100.millis))
  println(Right("eitherValueSuccess").asResult)
  println(Left("eitherValueFailure").asResult)
  println(Some("optionValueSuccess").asResult)
  println(None.asResult)