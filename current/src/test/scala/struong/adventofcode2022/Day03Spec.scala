package struong.adventofcode2022

import cats.effect.IO
import fs2.text
import munit.CatsEffectSuite
import struong.adventofcode2022.Day03.{distinctItems, priorityScore}

class Day03Spec extends CatsEffectSuite {

  val input =
    """
      |vJrwpWtwJgWrhcsFMMfFFhFp
      |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
      |PmmdzqPrVvPwwTWBwg
      |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
      |ttgJtRGJQctTZtZT
      |CrZsJsPPZsGzwwsLwLmpwMDw
      |""".stripMargin

  test("priority score of examples") {
    assertEquals(priorityScore("p"), 16)
    assertEquals(priorityScore("L"), 38)
    assertEquals(priorityScore("P"), 42)
    assertEquals(priorityScore("v"), 22)
    assertEquals(priorityScore("t"), 20)
    assertEquals(priorityScore("s"), 19)
  }

  test("part 1 - sum of priorities") {
    val test = fs2.Stream
      .emit(input)
      .through(text.lines)
      .filter(_.trim.nonEmpty)
      .through(Day03.backpack[IO])

    assertIO(test.compile.toList, List(157))
  }


  test("part 2 - first group share r item") {
    val input = List("vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg")
    assertEquals(distinctItems(input), "r")
  }

  test("part 2 - second group share Z item") {
    val input = List("wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn", "ttgJtRGJQctTZtZT", "CrZsJsPPZsGzwwsLwLmpwMDw")
    assertEquals(distinctItems(input), "Z")
  }

  test("part 2 - stuff") {
        val test = fs2.Stream
          .emit(input)
          .through(text.lines)
          .filter(_.trim.nonEmpty)
          .sliding(3, 3)
          .through(Day03.groupBackpacks[IO])

        assertIO(test.compile.toList, List(70))
  }
}
