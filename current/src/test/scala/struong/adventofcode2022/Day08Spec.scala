package struong.adventofcode2022

import fs2.text
import munit.CatsEffectSuite
import struong.adventofcode2022.Day08.{
  highestTrees,
  viewingDistance,
  visibleInnerTrees,
  visibleTrees
}

class Day08Spec extends CatsEffectSuite {
  val inputString: String =
    """
      |30373
      |25512
      |65332
      |33549
      |35390
      |""".stripMargin

  val input: List[List[Int]] = List(
    List(3, 0, 3, 7, 3),
    List(2, 5, 5, 1, 2),
    List(6, 5, 3, 3, 2),
    List(3, 3, 5, 4, 9),
    List(3, 5, 3, 9, 0)
  )

  test("parse the input to a grid") {
    val parse: List[List[Int]] = fs2.Stream
      .emit(inputString)
      .through(text.lines)
      .filter(_.nonEmpty)
      .map(_.toList.map(_.asDigit).toList)
      .toList

    assertEquals(parse, input)
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
    assertEquals(visibleTrees(input), 21)
  }

  test("highest visible trees in a row") {
    val row = Array(2, 5, 5, 1, 2)
    val lookLeft = viewingDistance(2, row)
    val lookRight = viewingDistance(2, row.reverse)

    assertEquals(lookLeft, 1)
    assertEquals(lookRight, 2)
  }

  test("highest visible tree in a column") {
    val column = Array(3, 5, 3, 5, 3)
    val lookUp = viewingDistance(1, column)
    val lookDown = viewingDistance(3, column.reverse)

    assertEquals(lookUp, 1)
    assertEquals(lookDown, 2)
  }

  test("highest example row") {
    val row = Array(3, 3, 5, 4, 9)
    val lookLeft = viewingDistance(2, row)
    val lookRight = viewingDistance(2, row.reverse)

    assertEquals(lookLeft, 2)
    assertEquals(lookRight, 2)
  }

  test("highest example column") {
    val column = Array(3, 5, 3, 5, 3)
    val lookUp = viewingDistance(3, column)
    val lookDown = viewingDistance(1, column.reverse)

    assertEquals(lookUp, 2)
    assertEquals(lookDown, 1)
  }

  test("highest visible trees from example") {
    val actual = highestTrees(input)
    assertEquals(actual, 8)
  }
}
