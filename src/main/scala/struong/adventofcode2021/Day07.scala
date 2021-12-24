package struong.adventofcode2021

final case class Crabman(fuelCost: Int, position: Int)

object Crabs {
  def fuelCost(input: Seq[Int], fastBurn: Boolean): Crabman = {
    val max = input.max
    val min = input.min

    Seq.range(min, max).foldLeft(Crabman(Int.MaxValue - 1, Int.MaxValue)) {
      case (lowestCrabman, position) =>
        val newFuelCost =
          if (fastBurn) calculateFuelFastCost(input, position)
          else calculateFuelCost(input, position)
        if (lowestCrabman.fuelCost > newFuelCost) {
          Crabman(newFuelCost, position)
        } else {
          lowestCrabman
        }
    }
  }

  def calculateFuelCost(input: Seq[Int], target: Int): Int = {
    input.foldLeft(0) {
      case (acc, current) =>
        acc + math.abs(target - current)
    }
  }

  def calculateFuelFastCost(input: Seq[Int], target: Int): Int = {
    input.foldLeft(0) {
      case (acc, current) =>
        val distance = math.abs(current - target) + 1

        val total = (1 until distance).sum
        acc + total
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

    val result = Crabs.fuelCost(input, false)
    println(s"result = ${result}")
    val result2 = Crabs.fuelCost(input, true)
    println(s"result2 = ${result2}")
  }
}
