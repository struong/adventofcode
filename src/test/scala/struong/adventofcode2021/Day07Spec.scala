package struong.adventofcode2021

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day07Spec extends AnyFunSuite with Matchers {

  test("fuel usage to position 2") {
    val input = Seq(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)
    Crabs.fuelCost(input) shouldBe Crabman(37, 2)
  }
}
