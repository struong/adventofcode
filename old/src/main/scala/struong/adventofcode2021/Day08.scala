package struong.adventofcode2021

import scala.annotation.tailrec

object SevenSegments {

  def score(knownValues: Map[Int, String], input: List[String]): Int = {
    input.foldLeft(Seq.empty[String]) {
        case (acc, current) =>
          val key = knownValues.filter(t => t._2 == current.sorted)
          println(s"knownValues = ${knownValues}")
          println(s"current = ${current.sorted}")
          acc :+ key.head._1.toString
      }.mkString.toInt
  }

  @tailrec
  def makeFixed( displays: List[String],
                 knownValues: Map[Int, String]
               ): Map[Int, String] = {
    if (knownValues.size == 8 || displays.size == 0) knownValues
    else {
      val display = displays.head.sorted

      val entry = display.size match {
        case 2 => Map(1 -> display)
        case 3 => Map(7 -> display)
        case 4 => Map(4 -> display)
        case 7 => Map(8 -> display)
        case _ => Map.empty
      }

      makeFixed(displays.tail, knownValues ++ entry)
    }
  }

  @tailrec
  def generateDisplay(
      displays: List[String],
      knownValues: Map[Int, String]
  ): Map[Int, String] = {
    if (knownValues.size == 10 || displays.isEmpty) knownValues
    else {
      val display = displays.head.sorted

      val entry = {
        display.size match {
          case length if length == 5 =>
            if (display.intersect(knownValues(1)) == knownValues(1)) {
              Map(3 -> display)
            } else if (knownValues(4).intersect(display).length == 3) {
              Map(5 -> display)
            } else
              Map(2 -> display)
          case length if length == 6 =>
            if (display.intersect(knownValues(1)) != knownValues(1)) {
              Map(6-> display)
            } else if (knownValues(4).intersect(display).length == 3) {
              Map(0 -> display)
            } else {
              Map(9 -> display)
            }
          case _ => Map.empty
        }
      }

      generateDisplay(displays.tail, knownValues ++ entry)
    }
  }
}

object Day08 {
  def p1(input: List[String]): Int = {
    input.filter { output =>
      output.size == 2 || output.size == 4 || output.size == 3 || output.size == 7
    }.length
  }

  def parseLeft(input: String): List[String] = {
    input.split("\\|").toSeq(0).trim.split("\\s+").toList
  }

  def parseRight(input: String): List[String] = {
    input.split("\\|").toSeq(1).trim.split("\\s+").toList
  }

  def main(args: Array[String]): Unit = {
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

    println(s"sum = ${sum}")

  }
}
