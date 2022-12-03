package struong.adventofcode2022

import cats.Applicative
import cats.effect.{IO, IOApp}
import cats.implicits.catsSyntaxApplicativeId
import fs2.Pipe

object Day01 extends IOApp.Simple {
  override def run: IO[Unit] = {
    val inputFile = "testdata/Day01.txt"

    Utils.read[IO](inputFile)
      .through(maxCalories(3))
      .map(println)
      .compile
      .drain
  }

  def maxCalories[F[_] : Applicative](topN: Int): Pipe[F, String, Int] = lines =>
    lines.fold(Map.empty[Int, Int]) { (accum, word) =>
      val wordInt: Int = word.toIntOption.getOrElse(0)
      val currentElf = {
        if (accum.isEmpty || word.isEmpty)
          accum.size + 1
        else
          accum.size
      }

      accum.updatedWith(currentElf) {
        case Some(value) => Some(value + wordInt)
        case None => Some(wordInt)
      }
    }.evalTap { map =>
      val sortedList = map.valuesIterator.toList.sorted(Ordering[Int].reverse)
      println(sortedList).pure[F]
    }.map(_.valuesIterator.toList.sorted(Ordering[Int].reverse).take(topN).sum)
}
