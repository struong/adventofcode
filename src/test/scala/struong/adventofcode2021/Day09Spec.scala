package struong.adventofcode2021

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day09Spec extends AnyFunSuite with Matchers {


  test("parse the output") {
    val expected = Seq(Seq(2, 1, 9, 9, 9, 4, 3, 2, 1, 0), Seq(3, 9, 8, 7, 8, 9, 4, 9, 2, 1), Seq(9, 8, 5, 6, 7, 8, 9, 8, 9, 2), Seq(8, 7, 6, 7, 8, 9, 6, 7, 8, 9), Seq(9, 8, 9, 9, 9, 6, 5, 6, 7, 8))

    Day09.parse shouldBe expected
  }

  test("lowest points") {
    val input = Day09.parse

    LavaTubes.heightMapSum(input) shouldBe 15
  }

  test("basin size") {
    val input = Day09.parse
    val lowPoints = LavaTubes.lowPoints(LavaTubes.heightmap(input))

    val scores = lowPoints.map(point => LavaTubes.basin(Seq(point), Set.empty, input, 0))
    val topScores = scores.sorted(Ordering.Int.reverse).take(3).product

    topScores shouldBe 1134
  }
}
