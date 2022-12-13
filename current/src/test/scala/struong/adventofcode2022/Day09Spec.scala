package struong.adventofcode2022

import munit.CatsEffectSuite
import struong.adventofcode2022.Utils.ArrayOps

import scala.collection.mutable
import scala.collection.mutable.Queue

class Day09Spec extends CatsEffectSuite {
  val input: String =
    """
      |R 4
      |U 4
      |L 3
      |D 1
      |R 4
      |D 1
      |L 5
      |R 2
      |""".stripMargin

  test("parse the input to a grid") {
    val grid = Array.fill(10, 10)(0)

    // clockwise direction
    val offsets = Vector(
      (0, 1),
      (1, 1),
      (1, 0),
      (-1, 1),
      (0, -1),
      (-1, -1),
      (-1, 0),
      (1, -1)
    )

    val endX = 10
    val endY = 10

    val targetX = 3
    val targetY = 5

    def breadthFirstSearch(
        input: Array[Array[Int]]
    ): Option[List[(Int, Int)]] = {
      val queue = mutable.Queue[List[(Int, Int)]]()
      queue.enqueue(List(0 -> 0))

      var solution: Option[List[(Int, Int)]] = None
      var visited = Set[(Int, Int)](0 -> 0)

      while (queue.nonEmpty && solution.isEmpty) {
        val steps @ (x, y) :: _ = queue.dequeue
        for ((dx, dy) <- offsets) {
          val newX = x + dx
          val newY = y + dy

          // if target or end has been reached
          if (
            (newX == targetX && newY == targetY) || (newX == endX && newY == endY)
          ) {
            solution = Some((newX -> newY) :: steps)
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

    println(breadthFirstSearch(grid))

    val set = Set(1, 2, 3)
    println(!set(5))
  }
}
