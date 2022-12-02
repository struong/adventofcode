package struong.adventofcode2021

import struong.adventofcode2021.RichOps.MatrixOps

import scala.annotation.tailrec

object RichOps {
  implicit class MatrixOps[A](val array: Seq[Seq[A]]) {
    def at(i: Int, j: Int): Option[A] = {
      array.lift(i).flatMap(_.lift(j))
    }

    def exists(i: Int, j: Int): Option[(Int, Int)] = {
      if(array.at(i, j).isDefined) {
        Some(i, j)
      } else {
        None
      }
    }
  }
}

object LavaTubes {
  def lowPoints(input: Seq[Seq[Int]]): Seq[(Int, Int)] = {
    var q = Seq.empty[(Int, Int)]

    for (i <- 0 until input.length) yield {
      for (j <- 0 until input(i).length) yield {
        if (input(i)(j) > 0) {
          q = q :+ (i -> j)
        }
      }
    }

    q
  }

  @tailrec
  def basin(q: Seq[(Int, Int)], visited: Set[(Int, Int)], input: Seq[Seq[Int]], score: Int): Int = {
    if(q.isEmpty) score
    else {
      val (x, y) = q.head

      if(input.at(x, y).isEmpty || visited.contains((x, y)) || (input(x)(y) == 9)) {
        basin(q.tail, visited, input, score)
      } else {
        val toVisit = q.tail ++ Seq(input.exists(x - 1, y), input.exists(x + 1, y), input.exists(x, y + 1), input.exists(x, y - 1)).flatten

        basin(toVisit, visited + (x -> y), input, score + 1)
      }
    }
  }

  def heightMapSum(input: Seq[Seq[Int]]): Int = {
    heightmap(input).flatten.sum
  }

  def heightmap(input: Seq[Seq[Int]]): Seq[Seq[Int]] = {
   for (i <- 0 until input.length) yield {
      for (j <- 0 until input(i).length) yield {
        if (isLowPoint(i, j, input)) {
          input(i)(j) + 1
        } else {
          0
        }
      }
    }
  }

  def isLowPoint(i: Int, j: Int, input: Seq[Seq[Int]]): Boolean = {
    val point = input(i)(j)
    val peeks = for (iModifier <- -1 to 1) yield {
      for (jModifier <- -1 to 1) yield {
        input.at(i + iModifier, j + jModifier)
      }
    }.flatten

    val lowerPoints = peeks.flatMap { x =>
      x.filter { v =>
        v > point
      }
    }

    lowerPoints.length == (peeks.flatten.length - 1)
  }
}

object Day09 {

  def parse: Seq[Seq[Int]] = {
    io.Source
      .fromInputStream(getClass.getResourceAsStream("Day09.txt"))
      .mkString
      .trim
      .linesIterator
      .map(
        _.toSeq
          .map(_.asDigit)
      )
      .toSeq
  }

  def main(args: Array[String]): Unit = {
    val input = Day09.parse

    println(LavaTubes.heightmap(input))
    
    val lowPoints = LavaTubes.lowPoints(LavaTubes.heightmap(input))
    val scores = lowPoints.map(point => LavaTubes.basin(Seq(point), Set.empty, input, 0))
    val topScores = scores.sorted(Ordering.Int.reverse).take(3).product

    println(s"topScores = ${topScores}")
  }
}
