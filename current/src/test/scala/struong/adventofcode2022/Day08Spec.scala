package struong.adventofcode2022

import fs2.text
import munit.CatsEffectSuite
import struong.adventofcode2022.Day08.{visibleInnerTrees, visibleTrees}

class Day08Spec extends CatsEffectSuite {
  val input: String =
    """
      |30373
      |25512
      |65332
      |33549
      |35390
      |""".stripMargin

  test("parse the input to a grid") {
    val parse: List[List[Int]] = fs2.Stream
      .emit(input)
      .through(text.lines)
      .filter(_.nonEmpty)
      .map(_.toList.map(_.asDigit).toList)
      .toList

    val expected = List(
      List(3, 0, 3, 7, 3),
      List(2, 5, 5, 1, 2),
      List(6, 5, 3, 3, 2),
      List(3, 3, 5, 4, 9),
      List(3, 5, 3, 9, 0)
    )

    assertEquals(parse, expected)
  }

  test("visible trees in row") {
    val row = List(2, 5, 5, 1, 2)
    val expected = List(true, false, false)

    assertEquals(visibleInnerTrees(row), expected)
  }

  test("visible trees in reversed row") {
    val row = List(2, 5, 5, 1, 2).reverse
    val expected = List(false, true, false)

    assertEquals(visibleInnerTrees(row), expected)
  }

  test("example trees") {
    val input = List(
      List(3, 0, 3, 7, 3),
      List(2, 5, 5, 1, 2),
      List(6, 5, 3, 3, 2),
      List(3, 3, 5, 4, 9),
      List(3, 5, 3, 9, 0)
    )

    assertEquals(visibleTrees(input), 21)
  }
}
