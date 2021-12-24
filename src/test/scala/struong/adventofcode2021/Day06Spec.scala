package struong.adventofcode2021

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import struong.adventofcode2021.Day05._

class Day06Spec extends AnyFunSuite with Matchers {

  test("one day") {
    val initState = Seq(3, 4, 3, 1, 2)
    val expected = Seq(2, 3, 2, 0, 1)

    Lanternfish.time(1, initState) shouldBe expected
  }

  test("two days") {
    val initState = Seq(3, 4, 3, 1, 2)
    val expected = Seq(1, 2, 1, 6, 0, 8)

    Lanternfish.time(2, initState) shouldBe expected
  }

  test("3 days") {
    val initState = Seq(3, 4, 3, 1, 2)
    val expected = Seq(0,1,0,5,6,7,8)

    Lanternfish.time(3, initState) shouldBe expected
  }

  test("18 days") {
    val initState = Seq(3, 4, 3, 1, 2)
    val expected = Seq(6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8)

    Lanternfish.time(18, initState) shouldBe expected
  }

  test("18 days") {
    val initState = Seq(3, 4, 3, 1, 2)
    val expected = Seq(6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8)

    Lanternfish.time(18, initState) shouldBe expected
  }
}
