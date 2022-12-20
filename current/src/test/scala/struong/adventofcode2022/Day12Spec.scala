package struong.adventofcode2022

import fs2.text
import munit.CatsEffectSuite
import struong.adventofcode2022.Day12._

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

  test("find start and end") {
    assertEquals(hill.start, Point(0, 0))
    assertEquals(hill.end, Point(5, 2))
  }

  test("BFS") {
    assertEquals(hill.bfs(hill.start).length - 1, 31)
  }

  test("BFS varying starts") {
      assertEquals(hill.bestStart, 29)
  }
}
