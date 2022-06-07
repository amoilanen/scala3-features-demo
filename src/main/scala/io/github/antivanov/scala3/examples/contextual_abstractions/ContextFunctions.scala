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

  trait Parser[T]:
    def parse(response: Object): T

  trait SqlStatement:
    def renderAsSql: String

  class Connection:
    def execute[T](statement: SqlStatement): Parser[T] ?=> IO[T] =
      // Not a real implementation, just a stub
      Future { summon[Parser[T]].parse(null) }

  case class UserId(value: Int)
  case class UserAttributes(name: String, age: Int)
  case class User(userId: UserId, attributes: UserAttributes)

  object User {
    case class InsertUserStatement(userAttributes: UserAttributes) extends SqlStatement:
      // Not a real implementation, just a stub
      override def renderAsSql: String = "???"
    // Not a real implementation, just a stub
    given userIdParser: Parser[UserId] =
      _ => UserId(5)
  }

  object UserDao:
    import User.given

    def insert(user: UserAttributes): DatabaseIO[UserId] =
      val connection = summon[Connection]
      connection.execute(InsertUserStatement(user))

@main def contextFunctions: Unit =
  import ContextFunctions._
  import User.userIdParser
  given connection: Connection = new Connection
  given executionContext: ExecutionContext = ExecutionContext.global

  val user = UserAttributes("John", 99)
  val userInsert: DatabaseIO[UserId] = UserDao.insert(user)
  val userId = Await.result(userInsert, 100.millis)

  print(userId)

