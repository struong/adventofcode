package struong.adventofcode2022

import fs2.text
import munit.CatsEffectSuite

class Day07Spec extends CatsEffectSuite {
  val input: String =
    """
      |$ cd /
      |$ ls
      |dir a
      |14848514 b.txt
      |8504156 c.dat
      |dir d
      |$ cd a
      |$ ls
      |dir e
      |29116 f
      |2557 g
      |62596 h.lst
      |$ cd e
      |$ ls
      |584 i
      |$ cd ..
      |$ cd ..
      |$ cd d
      |$ ls
      |4060174 j
      |8033020 d.log
      |5626152 d.ext
      |7214296 k
      |""".stripMargin

  test("example as map of directories & sizes") {
    val test: fs2.Stream[fs2.Pure, Map[String, Int]] = fs2.Stream
      .emit(input)
      .through(text.lines)
      .filter(_.nonEmpty)
      .through(Day07.pipe)
      .lastOr(Map.empty)
      .map(Day07.updateMapSizes)

    val expected = Map("/" -> 48381165, "/a" -> 94853, "/a/e" -> 584, "/d" -> 24933642)
    assertEquals(test.compile.toList, List(expected))
  }

  test("sizes") {
    val input = Map(
      "/" -> 23352670,
      "/a" -> 94269,
      "/d" -> 24933642,
      "/a/e" -> 584
    )
    val expected = Map(
      "/" -> 48381165,
      "/a" -> 94853,
      "/d" -> 24933642,
      "/a/e" -> 584
    )

    assertEquals(Day07.updateMapSizes(input), expected)
  }

  test("part 2") {
    val input = Map(
      "/" -> 48381165,
      "/a" -> 94853,
      "/d" -> 24933642,
      "/a/e" -> 584
    )

    assertEquals(Day07.part2(input), Some(24933642))
  }
}