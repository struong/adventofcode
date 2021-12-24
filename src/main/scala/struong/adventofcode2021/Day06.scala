package struong.adventofcode2021

object Lanternfish {

  def time(days: Int, input: Seq[Int]): Seq[Int] = {
    var latest = input

    for (_ <- 0 until days) {
      latest = grow(latest)
      val newFishes = Seq.fill(latest.count(_ == -1))(8)
      latest = latest.map(x => if (x == -1) 6 else x) ++ newFishes
    }

    latest
  }

  private def grow(input: Seq[Int]): Seq[Int] = {
    input.map(_ - 1)
  }
}

object Day06 {
  def parse(input: String): Seq[Int] = {
    input.split(",").map(_.toInt)
  }

  def main(args: Array[String]): Unit = {
    lazy val input = io.Source
      .fromInputStream(getClass.getResourceAsStream("Day06.txt"))
      .mkString
      .trim

    val result = Lanternfish.time(80, parse(input))
    println(s"result.length = ${result.length}")

  }
}
