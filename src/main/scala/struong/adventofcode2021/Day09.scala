package struong.adventofcode2021

import struong.adventofcode2021.RichOps.MatrixOps

object RichOps {
  implicit class MatrixOps[A](val array: Seq[Seq[A]]) {
    def at(i: Int, j: Int): Option[A] = {
      array.lift(i).flatMap(_.lift(j))
    }
  }
}

object LavaTubes {
  def heightmap(input: Seq[Seq[Int]]): Int = {
    val lowPoints = for (i <- 0 until input.length) yield {
      for (j <- 0 until input(i).length) yield {
        if(isLowPoint(i, j, input)) {
          input(i)(j) + 1
        } else {
          0
        }
      }
    }

    lowPoints.flatten.sum
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
  }
}
