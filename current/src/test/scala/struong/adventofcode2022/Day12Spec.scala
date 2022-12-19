package struong.adventofcode2022

import fs2.text
import munit.CatsEffectSuite
import struong.adventofcode2022.Utils.ArrayOps

import scala.collection.mutable

class Day12Spec extends CatsEffectSuite {
  val input: String =
    """
      |Sabqponm
      |abcryxxl
      |accszExk
      |acctuvwj
      |abdefghi
     |""".stripMargin

  val hill: Hill = Hill(
    List(
      List(-14, 0, 1, 16, 15, 14, 13, 12),
      List(0, 1, 2, 17, 24, 23, 23, 11),
      List(0, 2, 2, 18, 25, -28, 23, 10),
      List(0, 2, 2, 19, 20, 21, 22, 9),
      List(0, 1, 3, 4, 5, 6, 7, 8)
    )
  )

  test("as numbered grid") {
    val program = fs2.Stream
      .emit(input)
      .through(text.lines)
      .filter(_.nonEmpty)
      .map { line =>
        line.map(_.toInt - 97).toList
      }
      .compile
      .toList

    assertEquals(Hill(program), hill)
  }

  final case class Hill(heightmap: List[List[Int]]) {
    private def findPoint(height: Int): Point = {
      val y = heightmap.indexWhere(_.contains(height))
      val x = heightmap(y).indexWhere(_ == height)
      Point(x, y)
    }
    // find S
    val start: Point = findPoint(-14)
    // find E
    val end: Point = findPoint(-28)

    private val offsets: Vector[Point] = Vector(
      Point(0, 1),
      Point(1, 0),
      Point(0, -1),
      Point(-1, 0)
    )

    def breadthFirstSearch: List[Point] = {
      val input = heightmap.map(_.toArray).toArray

      // swap 'S' and 'E' with 'a' and 'z'
      input(start.y)(start.x) = 0
      input(end.y)(end.x) = 27

      if (start == end) {
        List.empty
      } else {
        val queue = mutable.Queue[List[Point]]()
        queue.enqueue(List(start))

        var solution = List.empty[Point]

        // So we do not backtrack
        var visited = Set[Point](start)

        while (queue.nonEmpty && solution.isEmpty) {
          // steps is the full list
          // steps is bound to the full pattern: head :: tails
          val steps @ point :: _ = queue.dequeue
          val boundaryPoint = Point(input.head.length, input.length)

          for (offset <- offsets) {
            val newPoint = point + offset

            val destinationHeight = input.at(newPoint)
            val currentHeight = input.at(point)

            val isClimbable = destinationHeight.map(dst =>
              currentHeight.exists(cur => cur + 1 >= dst)
            )
            // if target or end has been reached
            if (isClimbable.contains(true) && (newPoint == end) || (newPoint > boundaryPoint)) {
              solution = newPoint :: steps
            }

            // if it's valid and not been visited
            if (input.at(newPoint).isDefined && !visited(newPoint)) {
              // add valid location to the queue
              // if at most one higher or lower
              println("*" * 50)
              println(s"newPoint = ${newPoint}")
              println(s"destinationHeight = ${destinationHeight}")
              println(s"point = ${point}")
              println(s"currentHeight = ${currentHeight}")
              println(s"isClimbable = ${isClimbable}")
              println(s"queue = ${queue}")

              isClimbable match {
                case Some(climbable) if climbable =>
                  visited = visited + newPoint
                  queue.enqueue(newPoint :: steps)
                case _ => queue.enqueue(steps)
              }
            }
          }
        }

        solution
      }
    }
  }

  test("find start and end") {
    assertEquals(hill.start, Point(0, 0))
    assertEquals(hill.end, Point(5, 2))
  }

  test("BFS") {
    println(hill.breadthFirstSearch)
  }
}
