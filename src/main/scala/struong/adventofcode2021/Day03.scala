package struong.adventofcode2021

import scala.annotation.tailrec

final case class Diagnostics(
    gammaRate: BitCounter,
    oxygen: BitCounter,
    co2: BitCounter
) {

  private def flip(x: Int) = x ^ (1 << 0)
  private def inverse(bitCounter: BitCounter) =
    BitCounter(
      bitCounter.line.map(x => flip(x))
    )

  def powerConsumption: Int = gammaRate.asDecimal * inverse(gammaRate).asDecimal

  def lifeSupportRating: Int = oxygen.asDecimal * co2.asDecimal
}

object Diagnostics {
  private def parse(input: Seq[String]): Seq[Seq[Int]] = {
    input.map { in =>
      in.toList.map(_.asDigit)
    }
  }

  private def getBit(sum: Int, input: Seq[String]): Int = {
    val halfSize = input.size / 2
    if (sum > halfSize) 1 else 0
  }

  private def accumulate(input: Seq[Seq[Int]]): Seq[Int] = {
    val emptyLine = Seq.fill(input.head.size)(0)

    input.foldLeft(emptyLine) {
      case (l, r) =>
        l.lazyZip(r).map(_ + _)
    }
  }

  private def cleanse(
      input: Seq[Seq[Int]],
      keeps: (Seq[Seq[Int]], Int) => Boolean
  ): Seq[Seq[Int]] = {
    val maxIndex = input.head.size

    @tailrec
    def append(in: Seq[Seq[Int]], index: Int): Seq[Seq[Int]] = {
      index match {
        case index if in.size == 1 || index == maxIndex => in
        case _ =>
          val keep = keeps(in, index)
          val newInput = if (keep) {
            in.filter(x => x(index) == 1)
          } else {
            in.filter(x => x(index) == 0)
          }
          append(newInput, index + 1)
      }
    }

    append(input, 0)
  }

  def keepOnes(in: Seq[Seq[Int]], index: Int): Boolean = {
    accumulate(in)(index) >= (in.size / 2.0)
  }

  def keepZeroes(in: Seq[Seq[Int]], index: Int): Boolean = {
    accumulate(in)(index) < (in.size / 2.0)
  }
  def apply(input: Seq[String]): Diagnostics = {
    val parsedInput = parse(input)
    val accum = accumulate(parsedInput)

    val gamma = accum.map(x => getBit(x, input))

    Diagnostics(
      BitCounter(gamma),
      BitCounter(cleanse(parsedInput, keepOnes).flatten),
      BitCounter(cleanse(parsedInput, keepZeroes).flatten)
    )

  }
}

final case class BitCounter(
    line: Seq[Int]
) {
  def asDecimal: Int = {
    val binary = line.mkString
    Integer.parseInt(binary, 2)
  }
}

object Day03 {

  def main(args: Array[String]): Unit = {
    lazy val input = io.Source
      .fromInputStream(getClass.getResourceAsStream("Day03.txt"))
      .getLines()
      .toSeq

    val diagnostics = Diagnostics(input)

    println(s"diagnostics.powerConsumption = ${diagnostics.powerConsumption}")
    println(s"diagnostics.lifeSupportRating = ${diagnostics.lifeSupportRating}")
  }
}
