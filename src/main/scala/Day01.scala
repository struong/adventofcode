object Day01 extends App {
  val input = io.Source.fromResource("Day01").getLines().toList.map(_.toInt)

  def check(input: List[Int]): List[Int] = {
    input match {
      case Nil => Nil
      case x :: tail =>
        tail.flatMap {
        case element if (element + x) == 2020 =>
          List(element, x)
        case _ => Nil
      } ++ check(tail)
    }
  }

  val result = check(input)
  println(input.product)

  val inputSet = input.toSet

  inputSet.foreach { i =>
    val complement = 2020 - i
    if (inputSet.contains(complement)) {
      println(s"complement = ${complement}")
      println(s"i = ${i}")
      println(complement * i)
    }
  }
}
