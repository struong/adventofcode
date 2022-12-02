package struong.adventofcode2021

import scala.annotation.tailrec

object Lanternfish {

  @tailrec
  def generation(
      day: Int,
      until: Int,
      input: Map[Int, Long]
  ): Map[Int, Long] = {
    if (day > until) {
      input
    } else {
      val newLanternfishes = input.foldLeft(Map.empty[Int, Long]) {
        case (acc, (daysLeft, numberOfFish)) =>
          if (daysLeft == 0) {
            Map(
              6 -> (numberOfFish + acc.getOrElse(6, 0L)),
              8 -> (numberOfFish + acc.getOrElse(8, 0L))
            )
          } else {
            val newFish = Map(
              daysLeft - 1 -> (numberOfFish + acc.getOrElse(daysLeft - 1, 0L))
            )

            acc ++ newFish
          }
      }

      generation(day + 1, until, newLanternfishes)
    }

  }
}

object Day06 {
  def parse(input: String): Map[Int, Long] = {
    val inputStr = input.split(",").map(_.toInt)

    inputStr.toList
      .groupMapReduce(identity)(_ => 1L)(_ + _)
  }

  def parse(input: Seq[Int]): Map[Int, Long] = {
    input.toList
      .groupMapReduce(identity)(_ => 1L)(_ + _)
  }

  def main(args: Array[String]): Unit = {
    lazy val input = io.Source
      .fromInputStream(getClass.getResourceAsStream("Day06.txt"))
      .mkString
      .trim

    val result = Lanternfish.generation(1, 80, parse(input))
    println(s"result = ${result.values.sum}")
    val result2 = Lanternfish.generation(1, 256 , parse(input))
    println(s"result = ${result2.values.sum}")
  }
}
