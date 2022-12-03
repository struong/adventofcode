package struong.adventofcode2022

import cats.Show
import cats.effect.{IO, IOApp}
import fs2.Pipe

object Day02p1 extends IOApp.Simple {
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
        case "A" | "X" => Rock
        case "B" | "Y" => Paper
        case "C" | "Z" => Scissors
        case _ => throw new RuntimeException(s"$input is not a valid Shape")
      }
  }

  import struong.adventofcode2022.Day02p1.ShapeType._

  implicit val shapeTypeShow: Show[ShapeType] = Show.show[ShapeType] {
    case shape: Shaped => s"Shape: ${shape.toString}, Score: ${shape.score.toString}"
  }

  val win = 6
  val lose = 0
  val draw = 3

  case class Round(p1: ShapeType, p2: ShapeType) {
    def score: Int = {
      val play = (p1, p2) match {
        case (Rock, Paper) => win
        case (Rock, Scissors) => lose
        case (Paper, Rock) => lose
        case (Paper, Scissors) => win
        case (Paper, Scissors) => win
        case (Scissors, Rock) => win
        case (Scissors, Paper) => lose
        case _ => draw
      }
      play + p2.score
    }
  }

  def linesToRound(line: String): Round = {
    val lines = line.split(' ')
    Round(ShapeType.fromString(lines(0)), ShapeType.fromString(lines(1)))
  }

  def score[F[_]]: Pipe[F, String, Int] = lines =>
    lines
      .filter(s => s.trim.nonEmpty)
      .map(linesToRound)
      .fold(0) { (accum, round) =>
        accum + round.score
      }
}
