object Day02 extends App {
  val input = io.Source.fromResource("Day01").getLines().toList.map(_.toInt)
  val inputSet = input.toSet


  def check(input: List[Int], target: Int): List[Int] = {
    input match {
      case Nil => Nil
      case x :: tail =>
        tail.flatMap {
          case element if (element + x) == target =>
            List(element, x)
          case _ => Nil
        } ++ check(tail, target)
    }
  }



  println(check(input, 2020))
}
