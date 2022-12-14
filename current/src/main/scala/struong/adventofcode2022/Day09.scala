package struong.adventofcode2022

import cats.effect.{IO, IOApp}
import fs2.Pipe
import struong.adventofcode2022.Utils.ArrayOps

import scala.annotation.tailrec
import scala.collection.mutable

object Day09 extends IOApp.Simple {
  override def run: IO[Unit] = {
    val inputFile = "testdata/Day09.txt"

    val program =
      Utils
        .read[IO](inputFile)
        .filter(_.trim.nonEmpty)
        .map(_.toList.map(_.asDigit).toList)
        .compile
        .toList
    //        .map(visibleTrees) // part 1

    program
      .map(println)
  }

  // clockwise direction
  private val offsets: Vector[(Int, Int)] = Vector(
    (0, 1),
    (1, 1),
    (1, 0),
    (1, -1),
    (0, -1),
    (-1, -1),
    (-1, 0),
    (-1, 1)
  )

  def breadthFirstSearch(
      input: Array[Array[Int]],
      start: (Int, Int),
      target: (Int, Int)
  ): List[(Int, Int)] = {
    if (start == target) {
      List.empty
    } else {
      val queue = mutable.Queue[List[(Int, Int)]]()
      queue.enqueue(List(start._1 -> start._2))

      var solution = List.empty[(Int, Int)]

      // So we do not backtrack
      var visited = Set[(Int, Int)](0 -> 0)

      while (queue.nonEmpty && solution.isEmpty) {
        val steps @ (x, y) :: _ = queue.dequeue
        val (targetX, targetY) = (target._1, target._2)

        val endX = input.head.length
        val endY = input.length

        for ((dx, dy) <- offsets) {
          val newX = x + dx
          val newY = y + dy

          // if target or end has been reached
          if (
            (newX == targetX && newY == targetY)
            || (newX > endX && newY > endY)
          ) {
            solution = (newX -> newY) :: steps
          }

          // if it's valid and not been visited
          if (input.at(newX, newY).isDefined && !visited(newX, newY)) {
            // add location to the queue
            visited = visited + (newX -> newY)
            queue.enqueue((newX -> newY) :: steps)
          }
        }
      }

      solution
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
      currentPos: (Int, Int),
      directionCommand: DirectionCommand
  ): (Int, Int) = {
    val (x, y) = (currentPos._1, currentPos._2)

    directionCommand match {
      case Right(steps) => (x + steps, y)
      case Left(steps)  => (x - steps, y)
      case Up(steps)    => (x, y + steps)
      case Down(steps)  => (x, y - steps)
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
      case _   => throw new RuntimeException(s"Invalid direction $direction")
    }
  }
}
