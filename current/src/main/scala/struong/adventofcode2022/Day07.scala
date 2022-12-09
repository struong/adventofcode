package struong.adventofcode2022

import cats.effect.{IO, IOApp}
import fs2.{Pipe, Pull}

sealed abstract class Command

case object ListFiles extends Command

case class ChangeDirectory(path: String) extends Command

case class File(size: Int) extends Command

case class Dir(directory: String) extends Command

object Command {
  def apply(line: String): Command =
    line match {
      case "$ ls" => ListFiles
      case _ if line.startsWith("$ cd") =>
        val dir = line.split(' ')(2)
        ChangeDirectory(dir)
      case _ if (line.startsWith("dir")) => Dir(line.split(' ')(1))
      case _ =>
        File(line.split(' ')(0).toInt)
    }
}

object Day07 extends IOApp.Simple {
  override def run: IO[Unit] = {
    val inputFile = "testdata/Day07.txt"

    val program =
      Utils
        .read[IO](inputFile)
        .filter(_.trim.nonEmpty)
        .through(Day07.pipe)
        .lastOr(Map.empty)
        .map(updateMapSizes)
        .map(part2)

    program
      .map {
        println
      }
      .compile
      .drain
  }

  def part1(input: Map[String, Int]): Int = {
    input.values.filter(_ < 100000).sum
  }

  def part2(input: Map[String, Int]): Option[Int] = {
    input.get("/").flatMap { usedSpace =>
      input.values.toList.sorted.find(70000000 - usedSpace + _ >= 30000000)
    }
  }

  def pipe[F[_]]: Pipe[F, String, Map[String, Int]] = {
    val rootPath = "/"

    def newWd(cwd: String, path: String): String = {
      if (cwd == rootPath) {
        cwd + path
      } else {
        cwd + "/" + path
      }
    }

    // When handling recursion, use pull
    def go(
            s: fs2.Stream[F, String],
            current: Map[String, Int],
            cwd: String
          ): Pull[F, Map[String, Int], Unit] =
      s.pull.uncons1.flatMap {
        case Some((line, tail)) =>
          Command(line) match {
            case ListFiles => go(tail, current, cwd)
            case Dir(path) =>
              val newDirectories = current + (newWd(cwd, path) -> 0)
              Pull.output1(newDirectories) >> go(tail, newDirectories, cwd)
            case ChangeDirectory(path) =>
              // cd / (first command) do nothing
              if (path == rootPath) {
                go(tail, current, cwd)
              } else if (path == "..") {
                val currentDir =
                  cwd.split('/').dropRight(1).mkString("/")
                go(tail, current, currentDir)
              } else {

                go(tail, current, newWd(cwd, path))
              }
            case File(size) =>
              val newDirectories = current.updatedWith(cwd) {
                case Some(value) => Some(value + size)
                case None => Some(size)
              }

              Pull.output1(newDirectories) >> go(tail, newDirectories, cwd)
          }

        case None => Pull.done
      }

    val start = Map[String, Int](rootPath -> 0)
    lines => go(lines, start, rootPath).stream
  }

  def updateMapSizes(input: Map[String, Int]): Map[String, Int] = {
    // go through dir and sub dir and accumulate sizes
    input.foldLeft(Map.empty[String, Int]) { case (accum, (path, _)) =>
      val size = input.keys.map { key =>
        if (key.startsWith(path) || key == path) {
          input(key)
        } else 0
      }.sum

      accum + (path -> size)
    }
  }
}
