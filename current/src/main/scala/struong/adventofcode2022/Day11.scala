package struong.adventofcode2022

import cats.effect.{IO, IOApp}
import fs2.{Chunk, Pipe}

import scala.annotation.tailrec

object Day11 extends IOApp.Simple {
  override def run: IO[Unit] = {
    val inputFile = "testdata/Day11.txt"

    val program =
      Utils
        .read[IO](inputFile)
        .filter(_.trim.nonEmpty)
        .chunkN(6)
        .through(monkeyGames(10000))
        .map {
          _.toList
            .map(_._2.inspected)
            .sorted
            .takeRight(2)
            .product
        }

    program.map(println).compile.drain
  }

  def monkeyGames[F[_]](
      numberOfRounds: Int
  ): Pipe[F, Chunk[String], Map[Int, Monkey]] = { chunk =>
    chunk
      .map { chunk =>
        Monkey(chunk)
      }
      .fold(Map.empty[Int, Monkey]) { case (accum, monkey) =>
        accum + (monkey.id -> monkey)
      }
      .map { state =>
        val rounds = 1 to numberOfRounds
        rounds.toList.foldLeft(state) { case (currentState, _) =>
          round(currentState)
        }
      }
  }

  final case class Op(operator: String) {
    def worryLevel(item: Int): Int = {
      // e.g. Op(old,+,3)
      val operation = operator.split(',')
      val operand = operation(2).toIntOption.getOrElse(item)
      operation(1) match {
        case "+" => item + operand
        case "*" => item * operand
        case _   => throw new RuntimeException(s"Can't parse ${operation(1)}")
      }
    }
  }

  final case class Monkey(
      id: Int,
      items: List[Int],
      op: Op,
      divisible: Int,
      trueDst: Int,
      falseDst: Int,
      inspected: BigInt
  ) {
    def throwItems: (Monkey, List[(Int, Int)]) = {
      val thrownItems = items.map { item =>
        val worryLevel = op.worryLevel(item)
        val boredValue = math.floor(worryLevel / 3).toInt

        val dst =
          if (boredValue % divisible == 0)
            trueDst
          else
            falseDst

        (boredValue, dst)
      }

      (
        copy(items = List.empty, inspected = inspected + items.size),
        thrownItems
      )
    }
  }

  object Monkey {
    def apply(input: Chunk[String]): Monkey = {
      val id = input(0).split(" ")(1).dropRight(1).toInt
      val items = input(1).split(":")(1).split(',').map(_.trim.toInt).toList
      val op =
        input(2).split("=")(1).split(' ').filter(_.nonEmpty).mkString(",")
      val divisible = input(3).split("by ")(1).toInt
      val trueDst = input(4).split("monkey ")(1).toInt
      val falseDst = input(5).split("monkey ")(1).toInt

      Monkey(
        id,
        items,
        Op(op),
        divisible,
        trueDst,
        falseDst,
        0
      )
    }
  }

  def updateState(
      currentState: Map[Int, Monkey],
      thrownItems: List[(Int, Int)]
  ): Map[Int, Monkey] = {
    thrownItems.foldLeft(currentState) {
      case (currentState, (thrownItem, id)) =>
        currentState.updatedWith(id) {
          case Some(monkey) =>
            Some(monkey.copy(items = thrownItem :: monkey.items))
          case None =>
            throw new RuntimeException(
              s"Thrown item to monkey that does not exist id: $id"
            )
        }
    }
  }

  def round(state: Map[Int, Monkey]): Map[Int, Monkey] = {
    val monkeys = state.keys.toList.sorted
    monkeys.foldLeft(state) { case (currentState, id) =>
      val currentMonkey = currentState(id)
      val (updatedMonkey, thrownItems) = currentMonkey.throwItems
      val throwingMonkeyState = currentState.updated(id, updatedMonkey)

      updateState(throwingMonkeyState, thrownItems)
    }
  }

}
