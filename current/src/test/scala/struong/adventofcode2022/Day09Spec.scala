package struong.adventofcode2022

import fs2.text
import munit.CatsEffectSuite
import struong.adventofcode2022.Day09._

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

  test("next to the closest same point") {
    val actual = nextClosestPoint(Point(2, 0), Point(2, 0))
    assertEquals(actual, Point(2, 0))
  }

  test("next to the closest right point") {
    val actual = nextClosestPoint(Point(0, 0), Point(2, 0))
    assertEquals(actual, Point(1, 0))
  }

  test("next to the closest left point") {
    val actual = nextClosestPoint(Point(3, 0), Point(0, 0))
    assertEquals(actual, Point(2, 0))
  }

  test("next to the closest point above") {
    val actual = nextClosestPoint(Point(0, 0), Point(0, 5))
    assertEquals(actual, Point(0, 1))
  }

  test("next to the closest point below") {
    val actual = nextClosestPoint(Point(0, 5), Point(0, 0))
    assertEquals(actual, Point(0, 4))
  }

  test("next to the closest diagonal point") {
    val actual = nextClosestPoint(Point(0, 0), Point(2, 2))
    assertEquals(actual, Point(1, 1))
  }

  test("is close or touching") {
    assertEquals(isClose(Point(0, 0), Point(0, 1)), true)
    assertEquals(isClose(Point(0, 0), Point(0, 0)), true)
    assertEquals(isClose(Point(0, 0), Point(1, 1)), true)

    assertEquals(isClose(Point(0, 0), Point(0, 2)), false)
    assertEquals(isClose(Point(0, 0), Point(2, 2)), false)
  }

  test("walk along a horizontal path") {
    val start = Point(0, 0)
    val target = Point(0, 4)

    val actual = walk(start, target)

    val expected = List(
      Point(0, 1),
      Point(0, 2),
      Point(0, 3)
    )

    assertEquals(actual, expected)
  }

  test("walk along a diagonal path") {
    val start = Point(0, 0)
    val target = Point(4, 4)

    val actual = walk(start, target)

    val expected = List(
      Point(1, 1),
      Point(2, 2),
      Point(3, 3)
    )

    assertEquals(actual, expected)
  }

  test("two knots example") {
    val parsedInput: List[DirectionCommand] = List(
      Right(4),
      Up(4),
      Left(3),
      Down(1),
      Right(4),
      Down(1),
      Left(5),
      Right(2)
    )

    assertEquals(uniquePositions(Point(0, 0), parsedInput), 13)
  }

  test("9 knots example") {
    val input9 = List(
      Right(5),
      Up(8),
      Left(8),
      Down(3),
      Right(17),
      Down(10),
      Left(25),
      Up(20)
    )

    assertEquals(uniquePosition9(Point(0, 0), input9), 36)
  }
}
