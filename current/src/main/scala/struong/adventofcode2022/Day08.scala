package struong.adventofcode2022

import cats.effect.{IO, IOApp}

import scala.annotation.tailrec

object Day08 extends IOApp.Simple {
  override def run: IO[Unit] = {
    val inputFile = "testdata/Day08.txt"

    val program =
      Utils
        .read[IO](inputFile)
        .filter(_.trim.nonEmpty)
        .map(_.toList.map(_.asDigit).toList)
        .compile
        .toList
        .map(visibleTrees)

    program
      .map {
        println
      }
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
        // A tree is visible if all of the other trees between it and an edge of the grid are shorter than it
        val treesInBetween = row.slice(0, index)
        val isBigger = row(index) > treesInBetween.max
        go(visibleTrees :+ isBigger, index + 1)
      }
    }

    go(List.empty, 1).dropRight(1)
  }

  def trees(input: List[List[Int]]): List[List[Boolean]] = {
    input.map(visibleInnerTrees)
  }

  def combineTrees(left: List[List[Boolean]], right: List[List[Boolean]]): List[List[Boolean]] = left
    .zip(right)
    .map { case (x, y) =>
      x.zip(y)
        .map { case (l, r) =>
          l || r
        }
    }

  def visibleTrees(input: List[List[Int]]): Int = {
    val leftToRight = innerRow(trees(input))
    val rightToLeft = innerRow(trees(input.map(_.reverse)).map(_.reverse))
    val topToBottom = innerRow(trees(input.transpose))
    val bottomToTop = innerRow(trees(input.transpose.map(_.reverse)).map(_.reverse))

    val columns = combineTrees(topToBottom, bottomToTop)

    val rows = combineTrees(leftToRight, rightToLeft)

    val height = input.length
    val width = input.head.length

    val grid: Array[Array[Boolean]] = Array.fill(width - 2, height - 2)(true)

    for (i <- 0 until width - 2) {
      for (j <- 0 until height - 2) {
        val rowValue = rows(i)(j)
        val columnValue = columns(j)(i)
        grid(i)(j) = rowValue || columnValue
      }
    }

    val outerEdges = 2 * (height + width - 2)
    grid.map(inner => inner.count(_ == true)).sum + outerEdges
  }
}