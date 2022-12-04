package struong.adventofcode2022

import cats.Applicative
import cats.effect.{IO, IOApp}
import fs2.Pipe

object Day04 extends IOApp.Simple {
  override def run: IO[Unit] = {
    val inputFile = "testdata/Day04.txt"

    Utils.read[IO](inputFile)
      .filter(_.trim.nonEmpty)
//      .through(overlappingPairs(contains)) // part 1
      .through(overlappingPairs(containsAny)) // part 2
      .map(println)
      .compile
      .drain
  }

  def overlappingPairs[F[_] : Applicative](f: (String, String) => Boolean): Pipe[F, String, Int] = lines =>
    lines.fold(0) {
      case (accum, line) =>
        val pairs = line.split(',')
        if (f(pairs(0), pairs(1))) {
          accum + 1
        } else {
          accum
        }
    }

  def toRange(input: String): List[Int] = {
    val split = input.split('-')
    val (start, end) = (split(0).toInt, split(1).toInt)

    Range.inclusive(start, end).toList
  }

  def contains(elf1: String, elf2: String): Boolean = {
    val r1 = toRange(elf1)
    val r2 = toRange(elf2)

    val intersection = r1.intersect(r2)
    intersection.length == math.min(r1.length, r2.length)
  }

  def containsAny(elf1: String, elf2: String): Boolean = {
    toRange(elf1).intersect(toRange(elf2)).nonEmpty
  }
}
