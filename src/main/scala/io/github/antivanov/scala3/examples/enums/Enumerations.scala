package io.github.antivanov.scala3.examples.enums

import scala.annotation.tailrec

extension (x: Int)
   def mod(y: Int) =
      ((x % y) + y) % y

enum Direction(val xDirection: Int, val yDirection: Int):
  case Right extends Direction(1, 0)
  case Up extends Direction(0, 1)
  case Left extends Direction(-1, 0)
  case Down extends Direction(0, -1)

  def turnLeft: Direction =
    val ordinalAfterTurn = (this.ordinal + 1) mod Direction.values.length
    Direction.fromOrdinal(ordinalAfterTurn)

  def turnRight: Direction =
    val ordinalAfterTurn = (this.ordinal - 1) mod Direction.values.length
    Direction.fromOrdinal(ordinalAfterTurn)

case class Position(x: Int, y: Int, dir: Direction)

sealed trait Move:
  def update(p: Position): Position

case class Forward(distance: Int) extends Move:
  def update(p: Position): Position =
    Position(p.x + distance * p.dir.xDirection, p.y + distance * p.dir.yDirection, p.dir)

object TurnLeft extends Move:
  def update(p: Position): Position =
    p.copy(dir = p.dir.turnLeft)

object TurnRight extends Move:
  def update(p: Position): Position =
    p.copy(dir = p.dir.turnRight)

def followRoute(from: Position, route: List[Move]): Position =
  route.foldLeft(from)((p, move) =>
    move.update(p)
  )

@main def enumerationsMain: Unit =
  val route = List(
    Forward(2),
    TurnLeft,
    Forward(1),
    TurnRight,
    Forward(3),
    TurnRight,
    Forward(4),
    TurnRight,
    Forward(2),
    TurnRight,
    Forward(2),
    TurnLeft,
    Forward(1)
  )
  val initialPosition = Position(0, 0, Direction.Right)
  val finalPosition = followRoute(initialPosition, route)
  println(finalPosition)