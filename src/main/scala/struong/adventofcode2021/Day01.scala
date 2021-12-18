package struong.adventofcode2021

import scala.annotation.tailrec

object Day01 {
  def main(args: Array[String]): Unit = {

    lazy val input = io.Source
      .fromInputStream(getClass.getResourceAsStream("Day01.txt"))
      .getLines()
      .toSeq
      .map(_.toInt)

    println(measureDepth(input, 3))
  }

  def measureDepth(input: Seq[Int], windowSize: Int = 1): Int = {
    @tailrec
    def dive(in: Seq[Int], result: Int): Int = {
      if (in.size < windowSize) {
        result
      } else {
        val isDeeper =
          in.tail.take(3).sum > in.take(3).sum

        val newResult =
          if (isDeeper) {
            result + 1
          } else {
            result
          }

        dive(in.tail, newResult)
      }
    }

    dive(input, 0)
  }
}
