package struong.adventofcode2022

import fs2.text
import munit.CatsEffectSuite
import struong.adventofcode2022.Day11.monkeyGames

class Day11Spec extends CatsEffectSuite {
  val input: String =
    """
     |Monkey 0:
     |  Starting items: 79, 98
     |  Operation: new = old * 19
     |  Test: divisible by 23
     |    If true: throw to monkey 2
     |    If false: throw to monkey 3
     |
     |Monkey 1:
     |  Starting items: 54, 65, 75, 74
     |  Operation: new = old + 6
     |  Test: divisible by 19
     |    If true: throw to monkey 2
     |    If false: throw to monkey 0
     |
     |Monkey 2:
     |  Starting items: 79, 60, 97
     |  Operation: new = old * old
     |  Test: divisible by 13
     |    If true: throw to monkey 1
     |    If false: throw to monkey 3
     |
     |Monkey 3:
     |  Starting items: 74
     |  Operation: new = old + 3
     |  Test: divisible by 17
     |    If true: throw to monkey 0
     |    If false: throw to monkey 1
     |""".stripMargin

  test("example") {
    val program = fs2.Stream
      .emit(input)
      .through(text.lines)
      .filter(_.nonEmpty)
      .chunkN(6)
      .through(monkeyGames(20))
      .map {
        _.toList
          .map(x => println(s"id: ${x._2.id}: ${x._2.inspected}"))
      }

    program.compile.toList
  }
}
