package struong.adventofcode2022

import cats.effect._
import munit.CatsEffectSuite

class UtilsSpec extends CatsEffectSuite {

  val testFile = "testdata/spec/Read.txt"

  test("can read a file") {
    val readLines = Utils.read[IO](testFile)
      .compile
      .toList

    val expected = List("Hello", "World")
    assertIO(readLines, expected)
  }
}
