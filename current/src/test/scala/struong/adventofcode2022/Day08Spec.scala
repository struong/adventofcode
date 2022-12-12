package struong.adventofcode2022

import fs2.text
import munit.CatsEffectSuite

import scala.annotation.tailrec

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

  def innerRow[A](row: List[A]): List[A] = row.drop(1).dropRight(1)

  def visibleInnerTrees(row: List[Int]): List[Boolean] = {
    @tailrec
    def go(
        visibleTrees: List[Boolean],
        index: Int
    ): List[Boolean] = {
      if (index == row.length) {
        visibleTrees
      } else {
        if (visibleTrees(index - 1) == true) {
          val currentTree = row(index)
          go(visibleTrees :+ (currentTree > row(index - 1)), index + 1)
        } else {
          go(visibleTrees :+ false, index + 1)
        }
      }
    }

    innerRow(go(List(true), 1))
  }

  test("visible trees in row") {
    val row = List(2, 5, 5, 1, 2)
    val expected = List(true, false, false)

    assertEquals(visibleInnerTrees(row), expected)
  }

  test("visible trees in reversed row") {
    val row = List(2, 5, 5, 1, 2).reverse
    val expected = List(false, false, false)

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

    def trees(input: List[List[Int]]): List[List[Boolean]] = {
      input.map(visibleInnerTrees)
    }

    val leftToRight = innerRow(trees(input))
    val rightToLeft = innerRow(trees(input.map(_.reverse)).map(_.reverse))
    val topToBottom = innerRow(trees(input.transpose))
    val bottomToTop = innerRow(trees(input.transpose(_.reverse)).map(_.reverse))

//    println(s"leftToRight = ${leftToRight}")
//    println(s"rightToLeft = ${rightToLeft}")

    println(s"topToBottom = ${topToBottom}")
    println(s"bottomToTop = ${bottomToTop}")

    val columns = topToBottom
      .zip(bottomToTop)
      .map { case (x, y) =>
        x.zip(y)
          .map { case (l, r) =>
            l || r
          }
      }

    println(s"columns = ${columns}")

    val rows =
      leftToRight
        .zip(rightToLeft)
        .map { case (x, y) =>
          x.zip(y)
            .map { case (l, r) =>
              l || r
            }
        }

    val grid: Array[Array[Boolean]] = Array.fill(3, 3)(true)

    for (i <- 0 until grid.length) {
      for (j <- 0 until grid.length) {
        val rowValue = rows(i)(j)
        val columnValue = columns(j)(i)
        grid(i)(j) = rowValue || columnValue
      }
    }

    println(s"grid = ${grid.toString}")

    assertEquals(1, 21)

  }
}
