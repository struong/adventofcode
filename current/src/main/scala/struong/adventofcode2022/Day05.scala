package struong.adventofcode2022

import cats._
import cats.effect.{IO, IOApp}
import fs2.{Pipe, text}

final case class CratePos(amount: Int, start: Int, end: Int)

object CratePos {
  def apply(line: String): CratePos = {
    val moveRegex = """move (\d+) from (\d+) to (\d+)""".r
    line match {
      case moveRegex(amount, start, end) => CratePos(amount.toInt, start.toInt, end.toInt)
      case _ => throw new RuntimeException(s"can't regex $line")
    }
  }
}

object Day05 extends IOApp.Simple {
  override def run: IO[Unit] = {
    val inputFile = "testdata/Day05.txt"

    val startingCrates: fs2.Stream[IO, Map[Int, List[String]]] =
      Utils.read[IO](inputFile)
        .filter(_.trim.nonEmpty)
        .filter(!_.startsWith("move"))
        .through(Day05.readCrates[IO])

    val program =
      for {
        state <- startingCrates
        moved <- Utils.read[IO](inputFile)
          .filter(_.trim.nonEmpty)
          .filter(_.startsWith("move"))
          .through(Day05.moveCrates[IO](state, true))
      } yield moved

    program.map(println).compile.drain
  }

  private def parseLine(line: String): String =
    line
      .trim
      .replace("[", "")
      .replace("]", "")


  private def createMapFromRow(line: String): Map[Int, List[String]] = {
    val cratePositions = line.sliding(3, 4).toList.map(parseLine)

    cratePositions.zipWithIndex.foldLeft(Map.empty[Int, List[String]]) { case (accum, (letter, index)) =>
      val offsetIndex = index + 1
      if (letter.nonEmpty && letter.head.isLetter) {
        accum.updatedWith(offsetIndex) {
          case Some(value) => Some(value :+ letter)
          case None => Some(List(letter))
        }
      } else {
        accum
      }
    }
  }

  def readCrates[F[_] : Applicative]: Pipe[F, String, Map[Int, List[String]]] = { lines =>
    lines
      .fold(Map.empty[Int, List[String]]) { case (accum, line) =>
        val currentRow = createMapFromRow(line)

        currentRow.foldLeft(accum) { case (lineAccum, (key, letters)) =>
          lineAccum.updatedWith(key) {
            case Some(value) => Some(value ++ letters)
            case None => Some(letters)
          }
        }
      }
  }

  def moveCrates[F[_] : Applicative](startState: Map[Int, List[String]], moveOne: Boolean): Pipe[F, String, String] = { lines =>
    lines
      .map(CratePos(_))
      .fold(startState) { case (state, cratePos) =>

        val cratesToMove = {
         val movingCrates =  state
            .getOrElse(cratePos.start, throw new RuntimeException("No crates to move at ${cratePos.start}"))
            .take(cratePos.amount)

          if(moveOne) movingCrates.reverse else movingCrates
        }

        val takeState = state.updatedWith(cratePos.start) {
          case Some(values) => Some(values.drop(cratePos.amount))
          case None => throw new RuntimeException(s"No crates to move at ${cratePos.start}")
        }

        val offerState = takeState.updatedWith(cratePos.end) {
          case Some(values) => Some(cratesToMove ++ values)
          case None => Some(cratesToMove)
        }
        offerState
      }
      .map { endState =>
        endState
          .toSeq
          .sortBy(_._1)
          .map {
            case (_, crates) => crates.head
          }.mkString
      }
  }
}
