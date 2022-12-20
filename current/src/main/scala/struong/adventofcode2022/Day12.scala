package struong.adventofcode2022

import cats.effect.{IO, IOApp}
import struong.adventofcode2022.Utils.ArrayOps

import scala.annotation.tailrec

object Day12 extends IOApp.Simple {
  override def run: IO[Unit] = {
    val inputFile = "testdata/Day12.txt"

    val program =
      Utils
        .read[IO](inputFile)
        .filter(_.trim.nonEmpty)
        .map { line =>
          line.map(_.toInt - 97).toList
        }
        .compile
        .toList
        .map(Hill)
        .map { hill =>
          // p1
//           hill.bfs(hill.start).length - 1
          hill.bestStart
        }

    program.map(println)
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

    def bestStart: Int = {
      val possibleStarts: List[Point] =
        heightmap.zipWithIndex.foldLeft(List(start)) {
          case (starts, (col, y)) =>
            val points = col.zipWithIndex.map { case (height, x) =>
              if (height == 0) {
                Some(Point(x, y))
              } else {
                None
              }
            }

            starts ++ points.flatten
        }

      possibleStarts
        .map { start =>
          bfs(start).length - 1
        }
        .filterNot(_ == -1)
        .min
    }

    def bfs(src: Point): List[Point] = {
      val input = heightmap.toArray.map(_.toArray)

      // swap 'S' and 'E' with 'a' and 'z'
      input(start.y)(start.x) = 0
      input(end.y)(end.x) = 26

      @tailrec
      def go(queue: List[List[Point]], visited: Set[Point]): List[Point] = {
        queue match {
          case path :: tail =>
            val current = path.last
            if (current == end) {
              path
            } else {

              if (!visited(current)) {
                val newQueue =
                  tail ++ neighbours(input, current).map(n => path :+ n)
                go(newQueue, visited + current)
              } else {
                go(tail, visited)
              }
            }
          case Nil => List()
        }
      }

      go(List(List(src)), Set.empty)
    }

    def neighbours(input: Array[Array[Int]], point: Point): List[Point] = {
      input.at(point) match {
        case Some(height) =>
          var neighbors: Array[Point] = Array()
          for (offset <- offsets) {
            val offsetPoint = point + offset
            input.at(offsetPoint).foreach { destinationHeight =>
              if (destinationHeight - height <= 1) {
                neighbors = neighbors :+ offsetPoint
              }
            }
          }

          neighbors.toList
        case None => List.empty
      }
    }
  }
}
