package struong.adventofcode2022

import cats.effect.{IO, IOApp}
import fs2.Pipe

object Day02p2 extends IOApp.Simple {

  override def run: IO[Unit] = {
    val inputFile = "testdata/Day02.txt"

    Utils.read[IO](inputFile)
      .through(score)
      .map(println)
      .compile
      .drain
  }

  sealed abstract class ShapeType(val score: Int)

  object ShapeType {
    sealed trait Shaped {
      _: ShapeType =>}

    case object Rock extends ShapeType(1) with Shaped
    case object Paper extends ShapeType(2) with Shaped
    case object Scissors extends ShapeType(3) with Shaped

    def fromString(input: String): ShapeType =
      input match {
        case "A" => Rock
        case "B" => Paper
        case "C" => Scissors
        case _ => throw new RuntimeException(s"$input is not a valid Shape")
      }
  }

  sealed abstract class ResultType(val score: Int)
  object ResultType {
    case object Win extends ResultType(6)
    case object Lose extends ResultType(0)
    case object Draw extends ResultType(3)

    def fromString(input: String): ResultType =
      input match {
        case "X" => Lose
        case "Y" => Draw
        case "Z" => Win
        case _ => throw new RuntimeException(s"$input is not a valid Shape")
      }
  }

  case class Round(p1: ShapeType, p2: ResultType) {
    def score: Int = {
      import struong.adventofcode2022.Day02p2.ResultType._
      import struong.adventofcode2022.Day02p2.ShapeType._

      (p1, p2) match {
        case (Rock, Win) => Paper.score + Win.score
        case (Rock, Lose) => Scissors.score + Lose.score
        case (Paper, Win) => Scissors.score + Win.score
        case (Paper, Lose) => Rock.score + Lose.score
        case (Scissors, Win) => Rock.score + Win.score
        case (Scissors, Lose) => Paper.score + Lose.score
        case (shape, _) => shape.score + Draw.score
      }
    }
  }

  def linesToRound(line: String): Round = {
    val lines = line.split(' ')
    Round(ShapeType.fromString(lines(0)), ResultType.fromString(lines(1)))
  }

  def score[F[_]]: Pipe[F, String, Int] = lines =>
    lines
      .filter(s => s.trim.nonEmpty)
      .map(linesToRound)
      .fold(0) { (accum, round) =>
        accum + round.score
      }
}
