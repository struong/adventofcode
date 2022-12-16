package struong.adventofcode2022

import cats.effect.{IO, IOApp}
import fs2.Pipe

object Day10 extends IOApp.Simple {
  override def run: IO[Unit] = {
    val inputFile = "testdata/Day10.txt"

    val program =
      Utils
        .read[IO](inputFile)
        .filter(_.trim.nonEmpty)
        .through(strength)

    program
      .map(println)
      .compile
      .drain
  }

  def strength[F[_]]: Pipe[F, String, Int] = lines =>
    lines
      .fold(Signal(0, 1, 0)) { (signal, line) =>
        line.split(" ") match {
          case Array("noop") => signal.noop
          case Array("addx", op) => signal.add(op)
        }
      }
      .map(_.strength)

  final case class Signal(cycle: Int, register: Int, strength: Int) {
    private def checkStrength(cycle: Int): Signal = {
      val cycles = List(20, 60, 100, 140, 180, 220)
      if (cycles.contains(cycle)) {
        copy(strength = strength + (cycle * register))
      } else {
        this
      }
    }

    def add(op: String): Signal = {
      noop.completeAdd(op)
    }

    def completeAdd(op: String): Signal = {
      val nextCycle = cycle + 1
      checkStrength(nextCycle).copy(nextCycle, register + op.toInt)
    }

    def noop: Signal = {
      val nextCycle = cycle + 1
      checkStrength(nextCycle).copy(nextCycle)
    }
  }
}
