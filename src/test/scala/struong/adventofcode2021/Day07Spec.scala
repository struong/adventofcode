package struong.adventofcode2021

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day07Spec extends AnyFunSuite with Matchers {

  test("constant fuel usage for target 2") {
    val input = Seq(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)
    Crabs.calculateFuelCost(input, 2) shouldBe 37
  }

  test("optimal fuel usage using constant burn") {
    val input = Seq(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)
    Crabs.fuelCost(input, false) shouldBe Crabman(37, 2)
  }

  test("fast fuel usage for target 2") {
    val input = Seq(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)
    Crabs.calculateFuelFastCost(input, 5) shouldBe 168
  }

  test("optimal fuel usage using fast burn") {
    val input = Seq(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)
    Crabs.fuelCost(input, true) shouldBe Crabman(168, 5)
  }
}
