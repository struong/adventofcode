package struong.adventofcode2021

import scala.annotation.tailrec

final case class Grid(
    rows: Seq[Set[Int]],
    columns: Seq[Set[Int]],
    gridNumber: Int,
    lastWinner: Option[Int] = None
) {
  def score: Option[Int] = lastWinner.map(_ * rows.map(_.sum).sum)
}

final case class BingoBoard(numbersCalled: Seq[Int], grids: Seq[Grid]) {

  def lastWinner: Option[Grid] = {
    val winningGrids = findWinners

    val winners = winningGrids
      .flatMap {
        case grid =>
          grid.lastWinner.map(winningNumber =>
            numbersCalled.indexOf(winningNumber)
          )
      }
      .sorted
      .reverse

    if (winners.isEmpty) {
      None
    } else {
      val firstWinningNumber = numbersCalled(winners.head)
      winningGrids
        .filter(_.lastWinner.contains(firstWinningNumber))
        .headOption
    }
  }

  def firstWinner: Option[Grid] = {
    val winningGrids = findWinners

    val winners = winningGrids.flatMap {
      case grid =>
        grid.lastWinner.map(winningNumber =>
          numbersCalled.indexOf(winningNumber)
        )
    }.sorted

    if (winners.isEmpty) {
      None
    } else {
      val firstWinningNumber = numbersCalled(winners.head)
      winningGrids
        .filter(_.lastWinner.contains(firstWinningNumber))
        .headOption
    }
  }

  private def findWinners: Seq[Grid] = {
    def findWinner(grid: Grid): Option[Grid] = {
      @tailrec
      def lookThroughGrid(
          previous: Grid,
          numbersLeft: Seq[Int],
          winner: Boolean
      ): Option[Grid] = {
        if (numbersLeft.isEmpty) {
          None
        } else if (winner) {
          Some(previous)
        } else {
          val number = numbersLeft.head
          val newRows = previous.rows.map(_ - number)
          val newCols = previous.columns.map(_ - number)

          // find winners
          val lastWinner =
            if (
              newRows
                .exists(_.isEmpty) || newCols.exists(_.isEmpty)
            ) {
              Some(number)
            } else {
              None
            }

          val current = Grid(newRows, newCols, previous.gridNumber, lastWinner)
          lookThroughGrid(current, numbersLeft.drop(1), lastWinner.isDefined)
        }
      }

      lookThroughGrid(grid, numbersCalled, false)
    }

    grids.flatMap(findWinner)
  }
}

object Day04 {
  def parse(input: Seq[String]): BingoBoard = {
    val (numbersCalled, lines) = input.splitAt(1)

    val intLines = lines
      .filter(_.nonEmpty)
      .map(_.trim)
      .foldLeft(Seq(Seq.empty[Int])) {
        case (acc, cur) =>
          val line = cur.split("\\s+")

          val i = line.toList.map(_.toInt)
          acc :+ i
      }
      .filter(_.nonEmpty)

    val grids = Seq.tabulate(intLines.size / 5) { index =>
      val row = intLines.splitAt(index * 5)._2.take(5)
      val col = row.transpose
      Grid(row.map(_.toSet), col.map(_.toSet), index)
    }

    BingoBoard(numbersCalled.head.split(",").map(_.toInt), grids)
  }

  def main(args: Array[String]): Unit = {
    lazy val input = io.Source
      .fromInputStream(getClass.getResourceAsStream("Day04.txt"))
      .getLines()
      .toSeq

    val bingoBoard = parse(input)

    val scoreFirst = bingoBoard.firstWinner.map(_.score)
    val scoreLast = bingoBoard.lastWinner.map(_.score)
    println(s"scoreFirst = ${scoreFirst}")
    println(s"scoreLast = ${scoreLast}")
  }
}
