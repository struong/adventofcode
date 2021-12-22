package struong.adventofcode2021

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day04Spec extends AnyFunSuite with Matchers with OptionValues {

  test("calculate the score") {
    lazy val input = io.Source
      .fromInputStream(getClass.getResourceAsStream("Day04.txt"))
      .getLines()
      .toSeq

    val bingoBoard = Day04.parse(input)

    bingoBoard.firstWinner.value.score shouldBe Some(4512)
  }
}
