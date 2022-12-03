package struong.adventofcode2022

import cats.effect._
import munit.CatsEffectSuite


class Day02p2Spec extends CatsEffectSuite {

  test("three rounds example") {
    val testFile = "testdata/spec/Day02.txt"
    val score = Utils.read[IO](testFile)
      .through(Day02p2.score)

    assertIO(
      score.compile.toList,
      List(12)
    )
  }
}
