package struong.adventofcode2021

final case class Crabman(fuelCost: Int, position: Int)

object Crabs {
  def fuelCost(input: Seq[Int]): Crabman = {
    val max = input.max
    val min = input.min

    println(s"max = ${max}")
    println(s"min = ${min}")

    Seq.range(min, max).foldLeft(Crabman(Int.MaxValue - 1, Int.MaxValue)) {
      case (lowestCrabman, position) =>
        val newFuelCost = calculateFuelCost(input, position)
        if(lowestCrabman.fuelCost > newFuelCost) {
          Crabman(newFuelCost, position)
        } else {
          lowestCrabman
        }
    }
  }

  private def calculateFuelCost(input: Seq[Int], target: Int): Int = {
    input.foldLeft(0) {
      case (acc, current) =>
        acc + math.abs(target - current)
    }
  }
}


object Day07 {

  def main(args: Array[String]): Unit = {
    lazy val input = io.Source
      .fromInputStream(getClass.getResourceAsStream("Day07.txt"))
      .mkString
      .trim
      .split(",")
      .map(_.toInt)
      .toSeq


    val result = Crabs.fuelCost(input)
    println(s"result = ${result}")
  }
}
