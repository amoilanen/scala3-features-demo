package io.github.antivanov.scala3.examples.other

object TransparentTraits
  transparent trait LoggingSupport:
    def log(level: String, message: String): Unit =
      println(s"[$level] $message")

  trait Service

  class UserService(id: String) extends Service, LoggingSupport

  class TicketService(id: String) extends Service, LoggingSupport

@main def mainTransparentTraits: Unit =
  import TransparentTraits._

  /*
   * Type inferred as Seq[Service] not Seq[Service & LoggingSupport] because
   * LoggingSupport is a transparent trait
   */
  val services = Seq(UserService("userService1"), TicketService("ticketService1"))

  // The following line is not going to compile since the inferred type does not contain LoggingSupport
  //services.foreach(_.log("debug", "test"))

  val otherServices: Seq[Service & LoggingSupport] = Seq(UserService("userService2"), TicketService("ticketService2"))

  // The following line is going to compile since the explicitly provided type contains LoggingSupport
  otherServices.foreach(_.log("debug", "test"))