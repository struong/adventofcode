package struong.adventofcode2021

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day04Spec extends AnyFunSuite with Matchers with OptionValues {

  lazy val input = io.Source
    .fromInputStream(getClass.getResourceAsStream("Day04.txt"))
    .getLines()
    .toSeq

  test("calculate the first winner") {
    val bingoBoard = Day04.parse(input)

    bingoBoard.firstWinner.value.score shouldBe Some(4512)
  }

  test("calculate the last winner") {
    val bingoBoard = Day04.parse(input)

    bingoBoard.lastWinner.value.score shouldBe Some(1924)
  }
}
