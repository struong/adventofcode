package struong.adventofcode2022

import cats.effect.{IO, IOApp}

object Day06 extends IOApp.Simple {
  override def run: IO[Unit] = {
    val inputFile = "testdata/Day06.txt"

    val program =
      Utils.read[IO](inputFile)
        .filter(_.trim.nonEmpty)
        .map(findMarker14)

    program.map(println).compile.drain
  }

  private def findMarker(input: String, markerLength: Int): Int = {
    val findUnique = input.sliding(markerLength)

    val firstMarker = findUnique.find(marker => marker.distinct.length == markerLength)

    firstMarker match {
      case Some(value) => input.indexOfSlice(value) + value.length
      case None => -1
    }
  }
  def findMarker4(input: String): Int = {
    findMarker(input,4)
  }

  def findMarker14(input: String): Int = {
    findMarker(input,14)
  }

}
