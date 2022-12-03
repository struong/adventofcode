package struong.adventofcode2022

import cats.Applicative
import cats.effect.{IO, IOApp}
import fs2.Pipe

object Day03 extends IOApp.Simple {
  override def run: IO[Unit] = {
    val inputFile = "testdata/Day03.txt"

    Utils.read[IO](inputFile)
//      .through(backpack) // part 1 answer
      .sliding(3, 3)
      .through(groupBackpacks)
      .map(println)
      .compile
      .drain
  }

  def priorityScore(input: String): Int = {
    input.foldLeft(0) { case (accum, char) =>
      if (char.isLower) {
        accum + char.toInt - 96
      } else {
        accum + char.toInt - 65 + 27
      }
    }
  }

  def backpack[F[_] : Applicative]: Pipe[F, String, Int] = lines =>
    lines
      .fold(0) { case (accum, line) =>
        val halfLength = line.length / 2
        val (first, second) = (line.substring(0, halfLength), line.substring(halfLength, line.length))
        val intersection = first.intersect(second).distinct
        accum + priorityScore(intersection)
      }

  def distinctItems(input: List[String]): String = {
    input.foldLeft(input.head) { case (common, line) =>
      line.intersect(common).distinct
    }
  }

  def groupBackpacks[F[_] : Applicative]: Pipe[F, fs2.Chunk[String], Int] = chunk =>
    chunk
      .fold(0) { case (accum, lines) =>
        accum + priorityScore(distinctItems(lines.toList))
      }

}
