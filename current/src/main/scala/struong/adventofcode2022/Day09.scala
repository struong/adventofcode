package struong.adventofcode2022

import cats.effect.{IO, IOApp}

import scala.annotation.tailrec

object Day09 extends IOApp.Simple {
  override def run: IO[Unit] = {
    val inputFile = "testdata/Day09.txt"

    val program =
      Utils
        .read[IO](inputFile)
        .filter(_.trim.nonEmpty)
        .map(DirectionCommand.apply)
        .compile
        .toList
        .map(commands => uniquePosition9(Point(0, 0), commands))

    program
      .map(println)
  }

  // clockwise direction
  val offsets: Vector[Point] = Vector(
    Point(0, 1),
    Point(1, 1),
    Point(1, 0),
    Point(1, -1),
    Point(0, -1),
    Point(-1, -1),
    Point(-1, 0),
    Point(-1, 1)
  )

  def uniquePosition9(start: Point, directions: List[DirectionCommand]): Int = {
    @tailrec
    def path(head: Point, tails: List[Point], directions: List[DirectionCommand], visited: Set[Point]): Set[Point] = {
      if (directions.isEmpty) {
        visited
      } else {
        val target = DirectionCommand.to(head, directions.head)

        var ninePath = Set.empty[Point]

        val rope = tails.zipWithIndex.foldLeft((List.empty[Point])) { case (accum, (knot, index)) =>
          val prevKnot = accum.lift(index - 1).getOrElse(target)

          if (isClose(knot, prevKnot)) {
            accum :+ knot
          } else {
            val pathWalked = walk(knot, prevKnot)

            if (index == 8) {
              ninePath = pathWalked.toSet
            }

            accum :+ pathWalked.last
          }
        }

        path(target, rope, directions.drop(1), visited ++ ninePath)
      }
    }

    path(start, List.fill(9)(Point(0, 0)), directions, Set(start)).size
  }

  def uniquePositions(start: Point, directions: List[DirectionCommand]): Int = {
    @tailrec
    def path(head: Point, tail: Point, directions: List[DirectionCommand], visited: Set[Point]): Set[Point] = {
      if (directions.isEmpty) {
        visited
      } else {
        val target = DirectionCommand.to(head, directions.head)
        val pathWalked = walk(tail, target)
        if (pathWalked.isEmpty) {
          path(target, tail, directions.drop(1), visited)
        } else {
          path(target, pathWalked.last, directions.drop(1), pathWalked.toSet ++ visited)
        }
      }
    }

    path(start, start, directions, Set(start)).size
  }

  @tailrec
  def walk(current: Point, target: Point, visited: List[Point] = List.empty): List[Point] = {
    if (current == target || isClose(current, target)) {
      visited
    } else {
      val next = nextClosestPoint(current, target)
      walk(next, target, visited :+ next)
    }
  }

  def isClose(start: Point, target: Point): Boolean = {
    val isTouching = offsets.exists(offset => start + offset == target)
    isTouching || (start == target)
  }

  def nextClosestPoint(start: Point, target: Point): Point = {
    if (start == target) {
      start
    } else {
      // distance between (start + offset) to point
      val distances = offsets.map { case (offset) =>
        val dPoint = start + offset
        val dX = math.pow(dPoint.x - target.x, 2)
        val dY = math.pow(dPoint.y - target.y, 2)
        math.sqrt(dX + dY)
      }

      // Return the offset that matches the lowest distance
      val closestOffset = offsets(distances.indexOf(distances.min))
      // return the start point by that offset
      start + closestOffset
    }
  }
}

sealed abstract class DirectionCommand

final case class Right(steps: Int) extends DirectionCommand

final case class Left(steps: Int) extends DirectionCommand

final case class Up(steps: Int) extends DirectionCommand

final case class Down(steps: Int) extends DirectionCommand

object DirectionCommand {
  def to(
          currentPos: Point,
          directionCommand: DirectionCommand
        ): Point = {
    val (x, y) = (currentPos.x, currentPos.y)

    directionCommand match {
      case Right(steps) => Point(x + steps, y)
      case Left(steps) => Point(x - steps, y)
      case Up(steps) => Point(x, y + steps)
      case Down(steps) => Point(x, y - steps)
    }
  }

  def apply(input: String): DirectionCommand = {
    val line = input.split(' ')
    val (direction, steps) = (line(0), line(1).toInt)

    direction match {
      case "R" => Right(steps)
      case "L" => Left(steps)
      case "U" => Up(steps)
      case "D" => Down(steps)
      case _ => throw new RuntimeException(s"Invalid direction $direction")
    }
  }
}
