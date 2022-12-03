package struong.adventofcode2022

import cats.effect._
import munit.CatsEffectSuite

class Day01Spec extends CatsEffectSuite {

  test("five elves example") {
    val testFile = "testdata/spec/Day01.txt"
    val maxCalories = Utils.read[IO](testFile)
      .through(Day01.maxCalories(1))

    assertIO(
      maxCalories.compile.toList,
      List(24000)
    )
  }

  test("five elves example top 3") {
    val testFile = "testdata/spec/Day01.txt"
    val maxCalories = Utils.read[IO](testFile)
      .through(Day01.maxCalories(3))

    assertIO(
      maxCalories.compile.toList,
      List(45000)
    )
  }
}
