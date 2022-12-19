package struong.adventofcode2022

final case class Point(x: Int, y: Int) {
  def +(that: Point): Point = Point(x + that.x, y + that.y)
  def >(that: Point): Boolean = x > that.x && y > that.y
}
