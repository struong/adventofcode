package struong.adventofcode2022

import munit.CatsEffectSuite
import struong.adventofcode2022.Day06.findMarker4

class Day06Spec extends CatsEffectSuite {
  test("example 1") {
    val input = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
    assertEquals(findMarker4(input), 7)
  }

  test("example 2") {
    val input = "bvwbjplbgvbhsrlpgdmjqwftvncz"
    assertEquals(findMarker4(input), 5)
  }

  test("example 3") {
    val input = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
    assertEquals(findMarker4(input), 10)
  }

  test("example 4") {
    val input = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
    assertEquals(findMarker4(input), 11)
  }
}
