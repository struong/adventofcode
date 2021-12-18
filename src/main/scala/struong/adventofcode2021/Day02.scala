package struong.adventofcode2021

final case class Position(horizontal: Int, depth: Int, aim: Int) {
  def moveUp(matched: String): Position =
    Position(horizontal, depth, aim - matched.toInt)

  def moveDown(matched: String): Position =
    Position(horizontal, depth, aim + matched.toInt)

  def moveForward(matched: String): Position =
    Position(horizontal + matched.toInt, depth + (matched.toInt * aim), aim)
}

object Day02 {
  def main(args: Array[String]): Unit = {
    lazy val input = io.Source
      .fromInputStream(getClass.getResourceAsStream("Day02.txt"))
      .getLines()
      .toSeq

    val forwardPattern = "forward (\\d+)".r
    val downPattern = "down (\\d+)".r
    val upPattern = "up (\\d+)".r

    val initialPosition = Position(0, 0, 0)

    val finalPosition = input.foldLeft(initialPosition) {
      case (current, value) =>
        value match {
          case forwardPattern(matched) =>
            current.moveForward(matched)
          case downPattern(matched) => current.moveDown(matched)
          case upPattern(matched)   => current.moveUp(matched)
        }
    }

    val finalHorizontal = finalPosition.horizontal
    val finalDepth = finalPosition.depth

    println(finalHorizontal * finalDepth)
  }
}
