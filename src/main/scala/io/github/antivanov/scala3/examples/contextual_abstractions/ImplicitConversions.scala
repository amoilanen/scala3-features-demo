package io.github.antivanov.scala3.examples.contextual_abstractions

import java.util.concurrent.{ CompletableFuture, ExecutorService, Executors }
import java.util.function.{ Supplier, Consumer }
import scala.concurrent.{ Await, Future, Promise, ExecutionContext }
import scala.concurrent.duration._

import scala.language.postfixOps

// In Scala 3 implicit conversions have to be enabled explicitly and are not enabled by default
import scala.language.implicitConversions

// Implicit conversion in Scala 3, rare case when we actually need to transform one type to another
object JavaFutureConversions:
  given javaFutureToScalaFuture[T](using executionContext: ExecutionContext): Conversion[CompletableFuture[T], Future[T]] with
    def apply(future: CompletableFuture[T]): Future[T] =
      val promise = Promise[T]()
      future.whenComplete( (value, error) =>
        if (error != null) then
          promise.failure(error)
        else
          promise.success(value)
      )
      promise.future

object JavaExecutorServiceConversions:
  given executionContextFromExecutorService(using executorService: ExecutorService): ExecutionContext =
    ExecutionContext.fromExecutorService(executorService)

def javaFutureSucceedingWithValue[T](value: T)(using executorService: ExecutorService): CompletableFuture[T] =
  CompletableFuture.supplyAsync(() => value, executorService)

def javaFutureFailingWithError[T](error: Throwable)(using executorService: ExecutorService): CompletableFuture[T] =
  val future = CompletableFuture[T]()
  future.completeExceptionally(error)
  future

@main def implicitConversionsMain: Unit =
  import JavaFutureConversions.given
  import JavaExecutorServiceConversions.given

  val timeout = 500.milliseconds
  val numberOfThreads = 10
  given executorService: ExecutorService = Executors.newFixedThreadPool(numberOfThreads)

  val expectedResult = javaFutureSucceedingWithValue("abcde")
    .map(_.length)
  println(Await.result(expectedResult, timeout))

  val expectedError = javaFutureFailingWithError(new RuntimeException("Error in the Java Future"))
  try
    Await.result(expectedError, timeout)
  catch
    case error =>
      println(s"Caught expected error $error")

  executorService.shutdown()