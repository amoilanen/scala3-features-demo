package io.github.antivanov.scala3.examples.contextual_abstractions

extension [T](x: List[T])

  def car: T =
    x.head

  def cdr: List[T] =
    x.tail

  def cadr: T =
    x.cdr.car

  def cddr: List[T] =
    x.cdr.cdr

  def caddr: T =
    x.cdr.cdr.car

  def cdddr: List[T] =
    x.cdr.cdr.cdr

  def carOption: Option[T] =
    x.headOption

  def cadrOption: Option[T] =
    x.cdr.carOption

  def caddrOption: Option[T] =
    x.cdr.cdr.carOption

@main def extensionMethods: Unit =
  val l = List(1, 2, 3, 4, 5, 6)
  println(l.car)
  println(l.cdr)
  println(l.cadr)
  println(l.cddr)
  println(l.caddr)
  println(l.cdddr)
  println(l.carOption)
  println(l.cadrOption)
  println(l.caddrOption)