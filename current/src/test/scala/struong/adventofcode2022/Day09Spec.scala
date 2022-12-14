package struong.adventofcode2022

import fs2.text
import munit.CatsEffectSuite

class Day09Spec extends CatsEffectSuite {
  val grid: Array[Array[Int]] = Array.fill(10, 10)(0)

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

  test("parse input") {
    val oneInput = "R 4"
    assertEquals(DirectionCommand(oneInput), Right(4))
  }

  test("all the inputs") {
    val parse = fs2.Stream
      .emit(input)
      .through(text.lines)
      .filter(_.trim.nonEmpty)
      .map(DirectionCommand.apply)

    val expected = List(
      Right(4),
      Up(4),
      Left(3),
      Down(1),
      Right(4),
      Down(1),
      Left(5),
      Right(2)
    )

    assertEquals(parse.compile.toList, expected)
  }


  test("BFS when start and target are the same") {
    val target = (3, 5)
    assertEquals(Day09.breadthFirstSearch(grid, target, target), List.empty)
  }

  test("R4") {
    val target = (4, 0)

    val expected = List((4, 0), (3, 0), (2, 0), (1, 0), (0, 0))
    assertEquals(Day09.breadthFirstSearch(grid, (0, 0), target), expected)
  }

  test("example") {
    val commands: List[DirectionCommand] = List(
      Right(4),
      Up(4)
//      Left(3),
//      Down(1),
//      Right(4),
//      Down(1),
//      Left(5),
//      Right(2)
    )

    val actual = commands.foldLeft(List(0 -> 0)) { case (visited, command) =>
      val current = visited.head
      println(s"current = ${current}")
      val target = DirectionCommand.to(current, command)
      println(s"target = ${target}")
      println(s"Day09.breadthFirstSearch(grid, target) = ${Day09.breadthFirstSearch(grid, current, target)}")

      Day09.breadthFirstSearch(grid, current, target) ++ visited
    }
    println(s"actual = ${actual.sorted.distinct}")
  }
}
