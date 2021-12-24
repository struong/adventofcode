package struong.adventofcode2021

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import struong.adventofcode2021.Day05._

class Day05Spec extends AnyFunSuite with Matchers {

  test("should be able to print a grid") {
    val start = Position(0, 0)
    val end = Position(9, 9)

    val grid = Grid(start, end)

    val expected =
      "5.........\n..........\n..........\n..........\n..........\n..........\n..........\n..........\n..........\n.........."

    grid.raw(0)(0).hit = 5
    grid.toString() shouldBe expected
  }

  test("update a row") {
    val raw = List("0,0 -> 5, 0")
    val input = Hydrothermal.parse(raw)

    val grid = Grid(Position(0, 0), Position(5, 1))

    val newGrid = grid.update(input)

    val expected = "111111\n......"
    newGrid.toString() shouldBe expected
  }

  test("update a column") {
    val raw = List("0,0 -> 0, 5")
    val input = Hydrothermal.parse(raw)

    val grid = Grid(Position(0, 0), Position(2, 5))

    val newGrid = grid.update(input)

    val expected = "1..\n1..\n1..\n1..\n1..\n1.."
    newGrid.toString() shouldBe expected
  }

  test("parse diagonal line of input") {
    val inputs = Hydrothermal.parse("9,7 -> 7,9")

    val expected = List(
      Position(9, 7),
      Position(8, 8),
      Position(7, 9)
    )

    inputs shouldBe expected
  }

  test("diagonal lines") {
    val input = Hydrothermal.parse("1,1 -> 3,3")

    var grid = Grid(Position(0, 0), Position(3, 3))
    val newGrid = grid.update(input)
    val expected = "....\n.1..\n..1.\n...1"
    newGrid.toString() shouldBe expected
  }

  test("running example") {
    lazy val raw = io.Source
      .fromInputStream(getClass.getResourceAsStream("Day05.txt"))
      .getLines()
      .toList

    val input = Hydrothermal.parse(raw)

    val grid = Grid(Position(0, 0), Position(9, 9))

    val newGrid = grid.update(input)

//    val nonDiagonalGrid =
//      ".......1..\n..1....1..\n..1....1..\n.......1..\n.112111211\n..........\n..........\n..........\n..........\n222111...."

//    grid.toString() shouldBe expectedGrid
    newGrid.overlap shouldBe 12
  }

}
