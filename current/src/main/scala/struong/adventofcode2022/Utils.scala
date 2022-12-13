package struong.adventofcode2022

import fs2.io.file.{Files, Path}

object Utils {
  def read[F[_]: Files](path: String): fs2.Stream[F, String] = {
    Files[F].readUtf8Lines(Path(path))
  }

  implicit class ArrayOps[A](val array: Array[Array[A]]) {
    def prettyPrint(): Unit = array.map(_.mkString(" ")).foreach(println)

    def at(i: Int, j: Int): Option[A] = {
      array.lift(i).flatMap(_.lift(j))
    }
  }

  implicit class MatrixOps[A](val array: Seq[Seq[A]]) {
    def at(i: Int, j: Int): Option[A] = {
      array.lift(i).flatMap(_.lift(j))
    }

    def exists(i: Int, j: Int): Option[(Int, Int)] = {
      if (array.at(i, j).isDefined) {
        Some(i, j)
      } else {
        None
      }
    }
  }
}
