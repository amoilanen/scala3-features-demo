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

sealed trait Move
case class Forward(distance: Int) extends Move
object TurnLeft extends Move
object TurnRight extends Move

@tailrec
def followRoute(from: Position, route: List[Move]): Position =
  route match
    case nextMove::remainingRoute =>
      val updatedPosition = nextMove match
        case Forward(distance) =>
          Position(from.x + distance * from.dir.xDirection, from.y + distance * from.dir.yDirection, from.dir)
        case TurnLeft =>
          from.copy(dir = from.dir.turnLeft)
        case TurnRight =>
          from.copy(dir = from.dir.turnRight)
      followRoute(updatedPosition, remainingRoute)
    case _ =>
      from

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