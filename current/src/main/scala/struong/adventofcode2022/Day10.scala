package struong.adventofcode2022

import cats.Applicative
import cats.effect.{IO, IOApp}
import cats.implicits.catsSyntaxApplicativeId
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

  def strength[F[_]: Applicative]: Pipe[F, String, Int] = lines =>
    lines
      .fold(Signal(0, 1, 0, "")) { (signal, line) =>
        line.split(" ") match {
          case Array("noop") => signal.noop
          case Array("addx", op) => signal.add(op)
        }
      }
      .evalTap { x =>
        println(x.crt.grouped(40).mkString("\n")).pure[F]
      }
      .map(_.strength)


  final case class Signal(cycle: Int, register: Int, strength: Int, crt: String) {

    private def checkStrength(cycle: Int): Signal = {
      val spritePos = Range.inclusive(register, register + 2).toList
      val display = if (spritePos.contains(cycle % 40)) {
        "#"
      } else
        "."

      val crtSignal = copy(crt = crt ++ display)

      val cycles = List(20, 60, 100, 140, 180, 220)
      if (cycles.contains(cycle)) {
        crtSignal.copy(strength = strength + (cycle * register))
      } else {
        crtSignal
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
