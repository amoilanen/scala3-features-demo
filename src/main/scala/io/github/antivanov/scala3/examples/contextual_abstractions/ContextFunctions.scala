package io.github.antivanov.scala3.examples.contextual_abstractions

import io.github.antivanov.scala3.examples.contextual_abstractions.ContextFunctions.User.InsertUserStatement
import io.github.antivanov.scala3.examples.contextual_abstractions.ContextFunctions.{SqlStatement, UserAttributes, UserId}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContext, Future}

import scala.concurrent._
import scala.concurrent.duration._

object ContextFunctions:

  /*
   * Unfortunately Future is non-referentially transparent => not the same
   * referentially transparent IO as in ZIO or Cats Effect
   */
  type IO[T] = ExecutionContext ?=> Future[T]
  type DatabaseIO[T] = Connection ?=> IO[T]

  case class ParsingError(message: String) extends Exception(message)

  trait Parser[T]:
    def parse(response: Object): Either[ParsingError, T]

  trait SqlStatement:
    def renderAsSql: String

  class Connection:
    def execute[T](statement: SqlStatement): Parser[T] ?=> IO[T] =
      // Not a real implementation, just a stub
      summon[Parser[T]].parse(null).fold(Future.failed(_), value => Future.successful(value))

  case class UserId(value: Int)
  case class UserAttributes(name: String, age: Int)
  case class User(userId: UserId, attributes: UserAttributes)

  object User:
    case class InsertUserStatement(userAttributes: UserAttributes) extends SqlStatement:
      // Not a real implementation, just a stub
      override def renderAsSql: String = "???"
    // Not a real implementation, just a stub
    given userIdParser: Parser[UserId] =
      _ => Right(UserId(5))

  object UserDao:
    def insert(user: UserAttributes): Parser[UserId] ?=> DatabaseIO[UserId] =
      val connection = summon[Connection]
      connection.execute(InsertUserStatement(user))

@main def contextFunctionsMain: Unit =
  import ContextFunctions.{Connection, DatabaseIO, User, UserDao}
  import User.given
  given connection: Connection = new Connection
  given executionContext: ExecutionContext = ExecutionContext.global

  val user = UserAttributes("John", 99)
  val userInsert: DatabaseIO[UserId] = UserDao.insert(user)
  val userId = Await.result(userInsert, 100.millis)

  print(userId)