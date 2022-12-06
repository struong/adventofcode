package struong.adventofcode2022

import cats.effect.IO
import fs2.text
import munit.CatsEffectSuite

class Day05Spec extends CatsEffectSuite {

  val input: String =
    """
      |    [D]
      |[N] [C]
      |[Z] [M] [P]
      | 1   2   3
      |
      |move 1 from 2 to 1
      |move 3 from 1 to 3
      |move 2 from 2 to 1
      |move 1 from 1 to 2
      |""".stripMargin

  val startingCrates: fs2.Stream[IO, Map[Int, List[String]]] = fs2.Stream
    .emit(input)
    .through(text.lines)
    .filter(_.nonEmpty)
    .filter(!_.startsWith("move"))
    .through(Day05.readCrates[IO])

  test("read starting crates") {
    val expected = Map(2 -> List("D", "C", "M"), 1 -> List("N", "Z"), 3 -> List("P"))
    assertIO(startingCrates.compile.toList, List(expected))
  }

  test("p1 - move single crates") {
    val program =
      for {
        state <- startingCrates
        moved <- fs2
          .Stream
          .emit(input)
          .through(text.lines)
          .filter(_.nonEmpty)
          .filter(_.startsWith("move"))
          .through(Day05.moveCrates[IO](state, moveOne = true))
      } yield moved

    val expected = "CMZ"
    assertIO(program.compile.toList, List(expected))
  }

  test("p2 - move multiple crates") {
    val program =
      for {
        state <- startingCrates
        moved <- fs2
          .Stream
          .emit(input)
          .through(text.lines)
          .filter(_.nonEmpty)
          .filter(_.startsWith("move"))
          .through(Day05.moveCrates[IO](state, moveOne = false))
      } yield moved

    val expected = "MCD"
    assertIO(program.compile.toList, List(expected))
  }
}
