package struong.adventofcode2021

final case class Grid(
    rows: Seq[Set[Int]],
    columns: Seq[Set[Int]],
    gridNumber: Int,
    lastWinner: Option[Int] = None
) {
  def score: Option[Int] = lastWinner.map(_ * rows.map(_.sum).sum)
}
final case class BingoBoard(numbersCalled: Seq[Int], grids: Seq[Grid]) {
  def firstWinner: Option[Grid] = {
    val winningGrids = findWinners

    val winners = winningGrids.flatMap {
      case grid =>
        grid.lastWinner.map(winningNumber =>
          numbersCalled.indexOf(winningNumber)
        )
    }.sorted

    println(s"winners = ${winners}")

    if (winners.isEmpty) {
      None
    } else {
      val firstWinningNumber = numbersCalled(winners.head)
      println(s"firstWinningNumber = ${firstWinningNumber}")
      val x = winningGrids
        .filter(_.lastWinner.contains(firstWinningNumber))
        .headOption
      println(s"x = ${x}")
      x
    }
  }

  private def findWinners: Seq[Grid] = {
    grids.flatMap { grid =>
      numbersCalled
        .scanLeft(grid) {
          case (g, number) =>
            // find and drop numbers
            val newRows = g.rows.map(_ - number)
            val newCols = g.columns.map(_ - number)

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

//            println(s"number = ${number}")
//            println(s"newRows = ${newRows}")
//            println(s"newCols = ${newCols}")
//            println(s"lastWinner = ${lastWinner}")

            Grid(newRows, newCols, g.gridNumber, lastWinner)
        }
    }
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

    val score = bingoBoard.firstWinner.map(_.score)
    println(s"score = ${score}")
  }
}
