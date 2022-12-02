package struong.adventofcode2021

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day08Spec extends AnyFunSuite with Matchers {

  test("parse the output") {
    val input = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"

    val parse = input.split("\\|").toSeq(1).trim.split("\\s+")
    parse shouldBe Seq("fdgacbe", "cefdb", "cefbgd" , "gcbe")
  }

  test("count digits in output") {
    lazy val input = io.Source
      .fromInputStream(getClass.getResourceAsStream("Day08.txt"))
      .getLines()
      .toList
      .flatMap(_.split("\\|").toSeq(1).trim.split("\\s+"))

    Day08.p1(input) shouldBe 26
  }

  test("one line") {
    val input = Day08.parseLeft("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

    val knownValues =  SevenSegments.makeFixed(input, Map.empty)

    val expected = Map(5 -> "cdfbe", 1 -> "ab", 6 -> "cdfgeb", 9 -> "cefabd", 7 -> "dab", 3 -> "fbcad", 8 -> "acedgfb", 4 -> "eafb", 2 -> "gcdfa", 0 -> "cagedb").view.mapValues(_.sorted).toMap

    SevenSegments.generateDisplay(input,knownValues) shouldBe expected
  }

  test("score") {
    val left = Day08.parseLeft("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")
    val right = Day08.parseRight("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

    val knownValues =  SevenSegments.generateDisplay(left,SevenSegments.makeFixed(left, Map.empty))

    SevenSegments.score(knownValues, right) shouldBe 5353
  }

  test("score output") {
    lazy val leftInput = io.Source
      .fromInputStream(getClass.getResourceAsStream("Day08.txt"))
      .getLines()
      .toList
      .map(_.split("\\|").toSeq(0).trim.split("\\s+"))

    lazy val rightInput = io.Source
      .fromInputStream(getClass.getResourceAsStream("Day08.txt"))
      .getLines()
      .toList
      .map(_.split("\\|").toSeq(1).trim.split("\\s+"))

    var sum = 0
    for(i <- 0 until leftInput.size) {

      val left = leftInput(i).toList
      val right = rightInput(i).toList

      val knownValues =  SevenSegments.generateDisplay(left,SevenSegments.makeFixed(left, Map.empty))

      sum += SevenSegments.score(knownValues, right)
    }

    sum shouldBe 61229
  }
}
