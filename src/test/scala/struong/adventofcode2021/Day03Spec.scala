package struong.adventofcode2021

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day03Spec extends AnyFunSuite with Matchers {
  val input = Seq(
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
  )

  test("calculate power consumption") {
    val diagnostics = Diagnostics(
      Helper.makeGamma(input),
      Helper.makeOxygen(input),
      BitCounter(Seq.empty[Int])
    )
    diagnostics.powerConsumption shouldBe 198
  }

  test("calculate oxygen rating") {
    val diagnostics = Diagnostics(
      Helper.makeGamma(input),
      Helper.makeOxygen(input),
      Helper.makeCO2(input)
    )

    println(s"diagnostics = ${diagnostics}")
    diagnostics.lifeSupportRating shouldBe 230
  }
}
