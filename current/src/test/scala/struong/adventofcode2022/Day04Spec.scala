package struong.adventofcode2022

import cats.effect.IO
import fs2.text
import munit.CatsEffectSuite

class Day04Spec extends CatsEffectSuite {

  test("overlaps in example") {
    val input =
      """
        |2-4,6-8
        |2-3,4-5
        |5-7,7-9
        |2-8,3-7
        |6-6,4-6
        |2-6,4-8
        |""".stripMargin


    val test = fs2.Stream
      .emit(input)
      .through(text.lines)
      .filter(_.trim.nonEmpty)
      .through(Day04.overlappingPairs[IO](Day04.contains))

    assertIO(test.compile.toList, List(2))
  }

  test("overlaps all in example") {
    val input =
      """
        |2-4,6-8
        |2-3,4-5
        |5-7,7-9
        |2-8,3-7
        |6-6,4-6
        |2-6,4-8
        |""".stripMargin


    val test = fs2.Stream
      .emit(input)
      .through(text.lines)
      .filter(_.trim.nonEmpty)
      .through(Day04.overlappingPairs[IO](Day04.containsAny))

    assertIO(test.compile.toList, List(4))
  }
}
