package struong.adventofcode2021

final case class Diagnostics(gammaRate: BitCounter) {

  private def flip(x: Int) = x ^ (1 << 0)
  def epsilonRate =
    BitCounter(
      gammaRate.line.map(x => flip(x))
    )

  def powerConsumption: Int = gammaRate.asDecimal * epsilonRate.asDecimal
}

final case class BitCounter(
    line: Seq[Int]
) {
  def asDecimal: Int = {
    val binary = line.mkString
    Integer.parseInt(binary, 2)
  }
}

object BitCounter {
  def parse(input: Seq[String]): Seq[Seq[Int]] = {
    input.map { in =>
      in.toList.map(_.asDigit)
    }
  }

  private def getBit(sum: Int, input: Seq[String]): Int = {
    val halfSize = input.size / 2
    if (sum > halfSize) 1 else 0
  }

  def make(input: Seq[String]): BitCounter = {

    val parsedInput = parse(input)

    val emptyLine = Seq.fill(input.head.size)(0)

    val accum = parsedInput.foldLeft(emptyLine) {
      case (current, acc) =>
        current.lazyZip(acc).map(_ + _)
    }

    val bits = accum.map(x => getBit(x, input))

    BitCounter(bits)
  }
}

object Day03 {

  def main(args: Array[String]): Unit = {
    lazy val input = io.Source
      .fromInputStream(getClass.getResourceAsStream("Day03.txt"))
      .getLines()
      .toSeq

    val diagnostics = Diagnostics(BitCounter.make(input))
    println(diagnostics.powerConsumption)
  }
}
