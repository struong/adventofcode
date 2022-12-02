package struong.adventofcode2021

object Day05 {
  final case class Cell(row: Int, column: Int) {
    var hit: Int = 0
  }

  final case class Grid(raw: Array[Array[Cell]]) {
    def update(input: List[Position]): Grid = {
      input.foreach {
        case point =>
          raw(point.y)(point.x).hit += 1
      }

      this
    }

    def overlap: Int = {
      var overlapCount = 0
      raw.foreach { i =>
        i.foreach { cell =>
          if (cell.hit >= 2) overlapCount += 1
        }
      }

      overlapCount
    }

    override def toString(): String = {
      var output = ""

      raw.foreach { i =>
        i.foreach { cell =>
          output += (if (cell.hit == 0) "." else cell.hit.toString)
        }

        output += "\n"
      }

      output.trim
    }
  }

  object Grid {
    def apply(start: Position, end: Position): Grid = {
      val grid = (for (i <- start.y to end.y) yield {
        (for (j <- start.x to end.x) yield {
          Cell(i, j)
        }).toArray
      }).toArray

      Grid(grid)
    }
  }

  final case class Position(x: Int, y: Int)


  object Hydrothermal {
    def is45Degrees(start: Position, end: Position): Boolean = {
      val delta = math.abs(end.y - start.y) / math.abs(end.x - start.x)
      delta == 1
    }

    def parse(input: List[String]): List[Position] = {
      input.map(Hydrothermal.parse).flatten
    }

    def directedTo(start: Int, end: Int): Range.Inclusive = {
      Range.inclusive(start, end, if (start > end) -1 else 1)
    }

    def parse(input: String): List[Position] = {
      val line = input.replace("->", ",").split(",")
      val coords = line.flatMap(_.trim.toIntOption)

      val start = Position(coords(0), coords(1))
      val end = Position(coords(2), coords(3))

      if (start.x == end.x) {
          directedTo(start.y, end.y).map (Position(start.x, _)).toList
      } else if (start.y == end.y) {
          directedTo(start.x, end.x).map(Position(_, start.y)).toList
      } else if (is45Degrees(start, end)) {
        val positions =
          (directedTo(start.x, end.x) zip directedTo(start.y, end.y)).map { x =>
            Position(x._1, x._2)
          }
        positions.toList
      } else {
        List.empty
      }
    }

    def vents(grid: Grid, point: Position): Grid = {

      grid.raw(point.x)(point.y).hit += 1
      grid
    }
  }

  def main(args: Array[String]): Unit = {
    lazy val raw = io.Source
      .fromInputStream(getClass.getResourceAsStream("Day05.txt"))
      .getLines()
      .toList

    val input = Hydrothermal.parse(raw)

    val grid = Grid(Position(0, 0), Position(1000, 1000))

    val newGrid = grid.update(input)

    println(s"grid.overlap = ${newGrid.overlap}")
  }
}
