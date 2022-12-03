package struong.adventofcode2022

import fs2.io.file.{Files, Path}

object Utils {
  def read[F[_]: Files](path: String): fs2.Stream[F, String] = {
    Files[F].readUtf8Lines(Path(path))
  }
}
