package struong.adventofcode2021

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import struong.adventofcode2021.Day05._

class Day06Spec extends AnyFunSuite with Matchers {

  test("one day") {
    val initState = Day06.parse(Seq(3, 4, 3, 1, 2))
    val expected = Map(0 -> 1, 1 -> 1, 2 -> 2, 3 -> 1)

    Lanternfish.generation(1, 1, initState) shouldBe expected
  }

  test("two days") {
    val initState = Day06.parse(Seq(3, 4, 3, 1, 2))
    val expected = Map(0 -> 1, 1 -> 2, 2 -> 1, 6 -> 1, 8 -> 1)

    Lanternfish.generation(1, 2, initState) shouldBe expected
  }

  test("3 days") {
    val initState = Day06.parse(Seq(3, 4, 3, 1, 2))
    val expected = Map(0 -> 2, 1 -> 1, 5 -> 1, 6 -> 1, 7 ->1, 8 -> 1)

    Lanternfish.generation(1, 3, initState) shouldBe expected
  }

  test("18 days") {
     val initState = Day06.parse(Seq(3, 4, 3, 1, 2))
    val expected = Map(0 -> 3,1 -> 5, 2 -> 3, 3 -> 2,4 -> 2,5 -> 1,  6 -> 5,  7 -> 1,  8 -> 4)

    Lanternfish.generation(1, 18, initState) shouldBe expected
  }

}
